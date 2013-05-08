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

(defun read-wav-header (stream)
  "Reads WAV header and returns it and number
   of bytes of `audio' data to read"
  (let ((bitreader (make-reader :stream stream))
        (header (make-wav-header)))
    (if (/= (bitreader.be-bignum:read-bits 32 bitreader)
            +wav-id+)
        (error 'wav-error :message "Not a wav stream"))
    
    ;; I Think we can ignore chunk size here and retreive it later
    (read-bits 32 bitreader)

    (if (/= (bitreader.be-bignum:read-bits 32 bitreader)
            +wav-format+)
        (error 'wav-error :message "Not a wav stream"))
    
    ;; Subchunk 1
    (if (/= (bitreader.be-bignum:read-bits 32 bitreader)
            +subchunk1-id+)
        (error 'wav-error :message "Not a wav stream"))

    (let ((subchunk1-size (read-bits 32 bitreader)))
      (setf (wav-header-audio-format header)
            (read-bits 16 bitreader)

            (wav-header-channels-num header)
            (read-bits 16 bitreader)

            (wav-header-samplerate header)
            (read-bits 32 bitreader))

      (let ((byterate (read-bits 32 bitreader))
            (block-align (read-bits 16 bitreader)))

        (setf (wav-header-bps header)
              (read-bits 16 bitreader))

        ;; Sanity checks
        (if (not
             (and (= byterate (floor (* (wav-header-samplerate header)
                                        (wav-header-channels-num header)
                                        (wav-header-bps header)) 8))
                  (= byterate (* (wav-header-samplerate header) block-align))))
            (error 'wav-error :message "Malformed wav stream"))

        (let ((extra-params-size (- subchunk1-size 16)))
          (cond
           ((> extra-params-size 0)
            (setq extra-params-size (read-bits 16 bitreader))
            (let ((extra-params (make-array (list extra-params-size)
                                            :element-type '(unsigned-byte 8))))
              (read-octet-vector extra-params bitreader)
              (setf (wav-header-extra-params header) extra-params)))))))

    ;; Subchunk 2
    (if (/= (bitreader.be-bignum:read-bits 32 bitreader)
            +subchunk2-id+)
        (error 'wav-error :message "Not a wav stream"))
    (let ((subchunk2-size (read-bits 32 bitreader)))

      ;; Invalidate bitreader and set stream position
      ;; to actual begining of sound data
      (file-position stream (reader-position bitreader))

      (values header subchunk2-size))))
