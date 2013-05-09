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

(defun read-chunk-header (reader)
  (if (/= (bitreader.be-bignum:read-bits 32 reader)
          +wav-id+)
      (error 'wav-error :message "Not a wav stream"))
  
  ;; I Think we can ignore chunk size here and retreive it later
  (read-bits 32 reader)

  (if (/= (bitreader.be-bignum:read-bits 32 reader)
          +wav-format+)
      (error 'wav-error :message "Not a wav stream"))
  
  reader)

(defun read-fact-subchunk (reader size)
  (if (/= size 4) (error 'wav-error-subchunk
                         :message "Fact subchunk size is not 4. Do not know what to do"
                         :rest-bytes size))
  (make-fact-subchunk :samples-num (read-bits 32 reader)))

(defun read-format-subchunk (reader size)
  (let ((subchunk (make-format-subchunk)))
    (setf (format-audio-format subchunk)
          (read-bits 16 reader)

          (format-channels-num subchunk)
          (read-bits 16 reader)

          (format-samplerate subchunk)
          (read-bits 32 reader))

    ;; No sanity checks by now
    (read-bits 32 reader) ;; Byte rate
    (read-bits 16 reader) ;; Block align

    (setf (format-bps subchunk)
          (read-bits 16 reader))

    (if (= size 16) subchunk
      (error 'wav-error-subchunk
             :message "Extended format subchunk is not supported"
             :rest-bytes (- size 16)))))

(defun read-subchunks (reader)
  (let ((chunks
         (loop for type = (bitreader.be-bignum:read-bits 32 reader)
               until (= type +data-subchunk+)
               collect
               (let ((size (read-bits 32 reader)))
                 (cond
                  ((= type +format-subchunk+)
                   (read-format-subchunk reader size))
                  ((= type +fact-subchunk+)
                   (read-fact-subchunk reader size))
                  (t
                   (error 'wav-error-subchunk
                          :message "Unknown subchunk"
                          :rest-bytes size)))))))
    (append chunks (list (make-data-subchunk :size (read-bits 32 reader))))))

(defun read-wav-header (stream)
  (let* ((reader (read-chunk-header
                  (make-reader :stream stream)))
         (header-subchunks (read-subchunks reader))
         (format-subchunk (car header-subchunks)))

    ;; Sanity checks
    (if (not (typep format-subchunk 'format-subchunk))
        (error 'wav-error :message "First subchunk is not format"))

    (if (not (or (= (format-audio-format format-subchunk) +wave-format-pcm+)
                 (find-if #'(lambda (x) (typep x 'fact-subchunk)) header-subchunks)))
        (error 'wav-error :message "Not fact subchunk in compressed wav"))
    
    ;; Invalidate bitreader and set stream position
    ;; to actual begining of sound data
    (file-position stream (reader-position reader))
    
    (values header-subchunks
            (data-size (car (last header-subchunks))))))
