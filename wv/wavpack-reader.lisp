;; Copyright (c) 2012-2014, Vasily Postnicov
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

;; This is actually utility functions just like in flac/flac-reader.lisp
(in-package :easy-audio.wv)

;; FIXME: is there a better way to keep these magic numbers out of code?
;; Can we calculate them in place?
(declaim (type (sa-ub 8) +exp2-table+))
(defparameter +exp2-table+
  (make-array (list 256)
              :element-type '(ub 8)
              :initial-contents
              '#.(with-open-file (in (merge-pathnames "exp2table.lisp-expr"
                                                      *compile-file-truename*))
                   (read in))))

(declaim (ftype (function ((sb 16)) (sb 32)) exp2s))
(defun exp2s (val)
  (declare (optimize (speed 3))
           (type (sb 16)))
  (if (< val 0)
      (- (exp2s (- val)))
      (let ((m (logior (aref +exp2-table+
                             (logand val #xff))
                       #x100))
            (exp (ash val -8)))
        (ash m (- exp 9)))))
