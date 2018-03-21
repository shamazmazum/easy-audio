;; Copyright (c) 2012-2013, Vasily Postnicov
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :easy-audio.wav)

(defun subchunk-type=>string (type)
  (concatenate
   'string
   (mapcar #'code-char
           (reverse
            (loop while (/= type 0) collect
                 (prog1
                     (logand type #xff)
                   (setq type (ash type -8))))))))

(defun skip-subchunk (c)
  "Invoke @c(skip-subchank) restart"
  (invoke-restart 'skip-subchunk c))

(defun check-wav-id (number)
  (if (/= number +wav-id+)
      (error 'wav-error :format-control "Not a wav stream")))

(defun check-wav-format (number)
  (if (/= number +wav-format+)
      (error 'wav-error :format-control "Not a wav stream")))

(defreader (read-chunk-header "Reads RIFF chunk header") ()
  (() (:octets 4) :function check-wav-id)
  (() (:octets 4) :endianness :little) ; I Think we can ignore chunk size here and retreive it later
  (() (:octets 4) :function check-wav-format))

(defun read-fact-subchunk (reader size)
  "Reads fact subchunk of size SIZE from reader"
  (let ((fact (make-fact-subchunk :samples-num (read-bits 32 reader :endianness :little))))
    (if (/= size 4) (error 'wav-error-subchunk
                           :format-control "Fact subchunk size is not 4. Do not know what to do"
                           :rest-bytes (- size 4)
                           :subchunk fact)
      fact)))

(defreader (read-format-subchunk%) ((make-format-subchunk))
  (format-audio-format (:octets 2) :endianness :little)
  (format-channels-num (:octets 2) :endianness :little)
  (format-samplerate (:octets 4) :endianness :little)
  (() (:octets 4) :endianness :little) ; byte rate
  (() (:octets 2) :endianness :little) ; block align
  (format-bps (:octets 2) :endianness :little))

(defreader (read-extended-format) ((make-format-subchunk))
  (format-valid-bps (:octets 2) :endianness :little)
  (format-channel-mask (:octets 4) :endianness :little)
  (format-subformat (:octet-vector (make-array 16 :element-type '(unsigned-byte 8)))))

(defun check-extensible-audio-format (format)
  "Check extensible audio format magick"
  (if (= (format-audio-format format)
         +wave-format-extensible+)
      (let ((subformat (format-subformat format)))
        (if (not (equalp (subseq subformat 2) +wave-format-extensible-magick+))
            (error 'wav-error-subchunk
                   :format-control "Invalid extensible format magick"
                   :rest-bytes 0
                   :subchunk format))
        (setf (format-audio-format format)
              (logior (aref subformat 0)
                      (ash (aref subformat 1) 8)))))
  format)

(defun read-format-subchunk (reader size)
  "Reads format subchunk of size SIZE from reader"
  (let ((subchunk (read-format-subchunk% reader)))
    (if (= size 16) subchunk
        (let ((extended-size (read-octets 2 reader :endianness :little)))
          ;; Sanity checks
          (if (not (or
                    (and (zerop extended-size) (= size 18))
                    (and (= extended-size 22) (= size 40))))
              (error 'wav-error-subchunk
                     :format-control "Malformed format subchunk"
                     :rest-bytes (- size 18)
                     :reader reader
                     :subchunk subchunk))

          (when (not (zerop extended-size))
            (read-extended-format reader subchunk)
            (check-extensible-audio-format subchunk))))
    subchunk))

(defun read-subchunks (reader)
  "Reads all subchunks of wav file begining with
   format subchunk and ending with data."
  (let (chunks)
    (tagbody
     read-subchunks-loop
     (let ((type (read-bits 32 reader))
           (size (read-bits 32 reader :endianness :little)))

       (easy-audio-early:with-interactive-debug
           (restart-case
               (push
                (cond
                  ((= type +format-subchunk+)
                   (read-format-subchunk reader size))
                  ((= type +fact-subchunk+)
                   (read-fact-subchunk reader size))
                  ((= type +data-subchunk+)
                   (make-data-subchunk :size size))
                  (t
                   (error 'wav-error-subchunk
                          :format-control "Unknown subchunk, type ~x (~s)"
                          :format-arguments (list type (subchunk-type=>string type))
                          :reader reader
                          :rest-bytes size)))
                chunks)
             
             (skip-subchunk (c)
               :interactive (lambda () (list easy-audio-early:*current-condition*))
               (if (not (zerop (wav-error-rest-bytes c)))
                   (read-octets (wav-error-rest-bytes c)
                                (wav-error-reader c))))))
       (if (/= type +data-subchunk+) (go read-subchunks-loop))))
    chunks))

(defun open-wav (stream)
  "Opens a wav stream and returns a bit reader object"
  (read-chunk-header
   (make-reader :stream stream)))

(defun read-wav-header (reader)
  "Reads wav file header from @c(reader). Returns a list of subchunks in the stream"
  (let ((header-subchunks (nreverse (read-subchunks reader))))
    ;; Sanity checks
    (let ((format-subchunk (car header-subchunks)))
      (if (not (typep format-subchunk 'format-subchunk))
          (error 'wav-error :format-control "First subchunk is not format"))

      (if (not (or (= (format-audio-format format-subchunk) +wave-format-pcm+)
                   (find-if #'(lambda (x) (typep x 'fact-subchunk)) header-subchunks)))
          (error 'wav-error :format-control "No fact subchunk in compressed wav")))
    header-subchunks))

;; Helper function(s)
(defun samples-num (subchunks)
  "Returns a number of interchannel samples in the stream."
  (let ((fact (find 'fact-subchunk subchunks :key #'type-of))
        (data (find 'data-subchunk subchunks :key #'type-of))
        (format (find 'format-subchunk subchunks :key #'type-of)))
    (if fact
        (fact-samples-num fact)
        (/ (data-size data) (format-channels-num format)
           (ash (format-bps format) -3)))))

(defun decompose (buffer channel-buffers)
  (let ((nsamples (length (car channel-buffers)))
        (channels (length channel-buffers)))
    (loop
       for i below nsamples
       for idx from 0 by channels
       do
         (loop
            for channel in channel-buffers
            for offset from 0 by 1
            do
              (setf (aref channel i)
                    (aref buffer (+ idx offset))))))
  channel-buffers)

(defun read-wav-data (reader format nsamples &key decompose)
  "Read a portion of audio data in the wav stream. Requires a @c(bitreader) and
@c(format) subchunk. Reads exactly @c(nsamples) interchannel
samples. Optionally, decomposes them into different by-channel arrays if
@c(decompose) is @c(T)."
  (let* ((channels (format-channels-num format))
         (bps (format-bps format))
         (buffer (make-array (* nsamples channels) :element-type '(signed-byte 32))))
    (loop for i below (length buffer) do
         (setf (aref buffer i)
               (read-bits bps reader :endianness :little)))
    (if decompose
        (decompose buffer
                   (loop repeat channels collect
                        (make-array nsamples :element-type '(signed-byte 32))))
        buffer)))

(defun decode-wav-data (format buffer)
  "Decodes wav audio data in the @c(buffer). Often, in the case of uncompressed
  data, it simply returns the @c(buffer) unmodified."
  (let ((audio-format (format-audio-format format)))
    (cond
      ((= audio-format +wave-format-pcm+) buffer)
      ((= audio-format +wave-format-alaw+)
       (map-into buffer #'easy-audio.general:g.711-alaw-decode buffer))
      ((= audio-format +wave-format-mulaw+)
       (map-into buffer #'easy-audio.general:g.711-ulaw-decode buffer))
      (t (error 'wav-error
                :format-control "Unknown audio encoding: ~d"
                :format-arguments (list audio-format))))))
