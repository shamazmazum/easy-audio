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

(in-package :easy-audio.wv)

;; NB: multiplication of weight and sample may be a bignum
(declaim (ftype (function ((sb 32) (sb 32)) (sb 32)) apply-weight))
(defun apply-weight (weight sample)
  (ash (+ 512 (* weight sample)) -10))

(declaim (ftype (function ((sb 32) (sb 32) (sb 32)) (sb 32)) delta-weight))
(defun delta-weight (delta source result)
  (declare (optimize (speed 3))
           (type (sb 32) delta result source))
  (if (and (/= source 0)
           (/= result 0))
      (if (< (logxor source result) 0)
          (- delta) delta) 0))

(declaim (ftype (function ((sb 32) (sb 32)) (sb 32))
                correlate-sample/w-term-17
                correlate-sample/w-term-18))

(defun correlate-sample/w-term-17 (i-1 i-2)
  (declare (optimize (speed 3))
           (type (sb 32) i-1 i-2))
  (- (* 2 i-1) i-2))

(defun correlate-sample/w-term-18 (i-1 i-2)
  (declare (optimize (speed 3))
           (type (sb 32) i-1 i-2))
  (+ i-1 (ash (- i-1 i-2) -1)))

(defun decode-wv-block (wv-block)
  (let ((decorr-samples (block-decorr-samples wv-block))
        (decorr-passes (block-decorr-passes wv-block))
        (residual (block-residual wv-block))) ; Will be destructively modified to output

    (if (bit-set-p (block-flags wv-block) +flags-hybrid-mode+)
        (error 'block-error :message "Hybrid encoding is not supported"))

    (macrolet ((correlate-sample (sample-form result-place weight-place)
                 ;; Must be called with 'delta' in the scope
                 (let ((sample-sym (gensym)))
                   `(let ((,sample-sym ,sample-form))
                      (psetf ,result-place
                             (+ (apply-weight ,weight-place ,sample-sym) ,result-place)
                             ,weight-place
                             (+ ,weight-place (delta-weight delta ,sample-sym ,result-place)))))))

      ;; FIXME: This is quite a boilerplate
      (flet ((correlation-pass (pass)
               (let ((term (decorr-pass-term pass))
                     (delta (decorr-pass-delta pass))
                     (weights (decorr-pass-weight pass)))
                 (cond
                   ((> term 0)
                    (loop for out-i in residual
                          for i from 0 by 1 do
                         (cond
                           ((= term 17)
                            (loop for j from 2 below (length out-i) do
                                 (correlate-sample (correlate-sample/w-term-17
                                                    (aref out-i (- j 1))
                                                    (aref out-i (- j 2)))
                                                   (aref out-i j)
                                                   (aref weights i))))
                           ((= term 18)
                            (loop for j from 2 below (length out-i) do
                                 (correlate-sample (correlate-sample/w-term-18
                                                    (aref out-i (- j 1))
                                                    (aref out-i (- j 2)))
                                                   (aref out-i j)
                                                   (aref weights i))))
                           (t
                            (loop for j from term below (length out-i) do
                                 (correlate-sample (aref out-i (- j term))
                                                   (aref out-i j)
                                                   (aref weights i)))))))
                   (t (error 'block-error :message "Negative terms are not supported yet"))))
               pass)

             ;; Needs to be checked
             (warmup (pass &key lastp)
               (let ((term (decorr-pass-term pass))
                     (delta (decorr-pass-delta pass))
                     (weights (decorr-pass-weight pass)))

                 (cond
                   ((> term 0)
                    (loop for out-i in residual
                       for decorr-samples-i in decorr-samples
                       for i from 0 by 1 do
                         (cond
                           ((= term 17)
                            (cond
                              (lastp
                               ;; The first sample in the block
                               (correlate-sample (correlate-sample/w-term-17
                                                  (aref decorr-samples-i 0)
                                                  (aref decorr-samples-i 1))
                                                 (aref out-i 0)
                                                 (aref weights i))

                               ;; The second sample in the block
                               (correlate-sample (correlate-sample/w-term-17
                                                  (aref out-i 0)
                                                  (aref decorr-samples-i 0))
                                                 (aref out-i 1)
                                                 (aref weights i)))
                              (t
                               (correlate-sample (correlate-sample/w-term-17
                                                  (aref out-i 0)
                                                  0)
                                                 (aref out-i 1)
                                                 (aref weights i)))))

                           ((= term 18)
                            (cond
                              (lastp
                               ;; The first sample in the block
                               (correlate-sample (correlate-sample/w-term-18
                                                  (aref decorr-samples-i 0)
                                                  (aref decorr-samples-i 1))
                                                 (aref out-i 0)
                                                 (aref weights i))

                               ;; The second sample in the block
                               (correlate-sample (correlate-sample/w-term-18
                                                  (aref out-i 0)
                                                  (aref decorr-samples-i 0))
                                                 (aref out-i 1)
                                                 (aref weights i)))
                              (t
                               (correlate-sample (correlate-sample/w-term-18
                                                  (aref out-i 0)
                                                  0)
                                                 (aref out-i 1)
                                                 (aref weights i)))))
                           (t
                            (if lastp
                                (loop for j below (length decorr-samples-i) do
                                     (correlate-sample (aref decorr-samples-i j)
                                                       (aref out-i j)
                                                       (aref weights i))))))))

                   (t (error 'block-error :message "Negative terms are not yet supported"))))
               pass))

        (destructuring-bind (last . first) decorr-passes
          (mapc (lambda (pass) (correlation-pass (warmup pass))) (reverse first))
          (warmup last :lastp t)
          (correlation-pass last))))

    (let ((shift (left-shift-amount (block-flags wv-block))))
      (labels ((shift-sample (sample)
                 (ash sample shift))
               (shift-channel (channel-out)
                 (map-into channel-out #'shift-sample channel-out)))

        (if (/= shift 0)
            (mapc #'shift-channel residual))))

    residual))
