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

(defun read-chunk-header (reader)
  "Reads RIFF chunk header"
  (if (/= (read-bits 32 reader)
          +wav-id+)
      (error 'wav-error :message "Not a wav stream"))
  
  ;; I Think we can ignore chunk size here and retreive it later
  (read-bits 32 reader :endianness :little)

  (if (/= (read-bits 32 reader)
          +wav-format+)
      (error 'wav-error :message "Not a wav stream"))
  
  reader)

(defun read-fact-subchunk (reader size)
  "Reads fact subchunk of size SIZE from reader"
  (let ((fact (make-fact-subchunk :samples-num (read-bits 32 reader :endianness :little))))
    (if (/= size 4) (error 'wav-error-subchunk
                           :message "Fact subchunk size is not 4. Do not know what to do"
                           :rest-bytes (- size 4)
                           :subchunk fact)
      fact)))

(defun read-format-subchunk (reader size)
  "Reads format subchunk of size SIZE from reader"
  (let ((subchunk (make-format-subchunk)))
    (setf (format-audio-format subchunk)
          (read-bits 16 reader :endianness :little)

          (format-channels-num subchunk)
          (read-bits 16 reader :endianness :little)

          (format-samplerate subchunk)
          (read-bits 32 reader :endianness :little))

    ;; No sanity checks by now
    (read-bits 32 reader :endianness :little) ;; Byte rate
    (read-bits 16 reader :endianness :little) ;; Block align

    (setf (format-bps subchunk)
          (read-bits 16 reader :endianness :little))

    (if (= size 16) subchunk
      (error 'wav-error-subchunk
             :message "Extended format subchunk is not supported"
             :rest-bytes (- size 16)
             :subchunk subchunk))))

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
                          :message (format nil "Unknown subchunk, type ~x (~s)"
                                           type (subchunk-type=>string type))
                          :reader reader
                          :rest-bytes size)))
                chunks)
             
             (skip-subchunk (c)
               :interactive (lambda () (list easy-audio-early:*current-condition*))
               (read-bits (* 8 (wav-error-rest-bytes c))
                          (wav-error-reader c)))))
       (if (/= type +data-subchunk+) (go read-subchunks-loop))))
    chunks))

(defun read-wav-header (stream)
  "Reads wav file header from @c(stream). Returns two values:
   list of subchunks and size of audio data to read.
   Stream position is set to begining of audio data."
  (let* ((reader (read-chunk-header
                  (make-reader :stream stream)))
         (header-subchunks (read-subchunks reader))
         (data-subchunk (car header-subchunks)))

    (setq header-subchunks (nreverse header-subchunks))

    ;; Sanity checks
    (let ((format-subchunk (car header-subchunks)))
      (if (not (typep format-subchunk 'format-subchunk))
          (error 'wav-error :message "First subchunk is not format"))

      (if (not (or (= (format-audio-format format-subchunk) +wave-format-pcm+)
                   (find-if #'(lambda (x) (typep x 'fact-subchunk)) header-subchunks)))
          (error 'wav-error :message "No fact subchunk in compressed wav")))
    
    ;; Invalidate bitreader and set stream position
    ;; to actual begining of sound data
    (file-position stream (reader-position reader))
    
    (values header-subchunks
            (data-size data-subchunk))))

;; Helper function(s)

(defun samples-num (subchunks)
  "Finds fact subchunk and gets number of samples in file from it"
  (let ((fact (find-if #'(lambda (subchunk) (typep subchunk 'fact-subchunk))
                       subchunks)))
    (fact-samples-num fact)))
