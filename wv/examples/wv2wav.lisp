;; Copyright (c) 2012-2015, Vasily Postnicov
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

(in-package :easy-audio.wv-examples)

(defun wv2wav (wv-name wav-name)
  "Decode wavpack to wav."
  (with-open-wv (reader wv-name)
    (restore-sync reader)
    (let* ((first-block (read-wv-block reader))
           (channels (block-channels first-block))
           (bps (block-bps first-block))
           (total-samples (block-total-samples first-block))
           (samplerate (block-samplerate first-block)))

      (reader-position reader 0)
      (restore-sync reader)

      (with-output-to-wav (out-stream wav-name
                           :supersede t
                           :samplerate samplerate
                           :channels channels
                           :bps bps
                           :totalsamples total-samples)
        (handler-case
            (loop with samples-written = 0
                  while (< samples-written total-samples)
                  for block = (read-wv-block reader)
                  for samples = (block-block-samples block)
                  for buf = (make-array (* samples channels) :element-type '(signed-byte 32))
                  do
                     (incf samples-written samples)
                     (write-sequence (mixchannels buf (decode-wv-block block))
                                     out-stream)))))))
