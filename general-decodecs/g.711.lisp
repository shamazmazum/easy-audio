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

(in-package :easy-audio.general)

(declaim (optimize (speed 3)))

(deftype ub8 () '(unsigned-byte 8))
(deftype b16 () '(signed-byte 16))

(declaim (ftype (function (ub8) b16) g.711-alaw-decode))
(defun g.711-alaw-decode (coded-val)
  (let* ((toggled-bits (logxor coded-val #x55))
         (mantissa (ldb (byte 4 0) toggled-bits))
         (exp (ldb (byte 3 4) toggled-bits))
         (res
          (if (= exp 0) (logior (ash mantissa 4) #x8)
            (ash (logior (ash mantissa 4) #x108)
                 (1- exp)))))

    (if (/= 0 (logand coded-val #x80)) res (- res))))

(declaim (ftype (function (ub8) b16) g.711-ulaw-decode))
(defun g.711-ulaw-decode (coded-value)
  (let* ((inv (- #xff coded-value))
         (exp (ldb (byte 3 4) inv))
         (mantissa (ldb (byte 4 0) inv))
         (res (- (ash (logior (ash mantissa 3) #x84) exp) 128)))

    (if (/= (logand coded-value #x80) 0) res (- res))))
