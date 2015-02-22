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
  "Decodes wavpack to wav."
  (with-open-file (in wv-name :element-type '(unsigned-byte 8))
    (let ((reader (make-reader-from-stream in)))
      (restore-sync reader)
      (let* ((block-reader (make-wv-block-reader reader))
             (first-block (funcall block-reader reader))
             (channels (block-channels first-block))
             (bps (block-bps first-block))
             (samples (block-block-samples first-block))
             (out-buf (make-array (* samples channels) :element-type '(signed-byte 32))))

        (reader-position reader 0)
        (restore-sync reader)

        (with-open-file (out wav-name
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
          (write-pcm-wav-header out
                                :samplerate (block-samplerate first-block)
                                :channels channels
                                :bps bps
                                :totalsamples (block-total-samples first-block)))

        (with-open-file (out wav-name
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create
                             :element-type (list 'signed-byte bps))
          (file-position out (/ (ash 44 3) bps))
          (handler-case
              (loop while t do
                   (write-sequence (mixchannels out-buf (decode-wv-block (funcall block-reader reader))) out))
            (bitreader-eof () ())))))))
