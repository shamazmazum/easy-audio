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

(declaim (optimize
          #+easy-audio-unsafe-code
          (safety 0) (speed 3)))

;; NB: multiplication of weight and sample may be a bignum
(declaim (ftype (function ((sb 32) (sb 32)) (sb 32)) apply-weight))
(defun apply-weight (weight sample)
  (declare (type (sb 32) weight sample))
  (ash (+ 512 (* weight sample)) -10))

(declaim (ftype (function ((sb 32) (sb 32) (sb 32) (sb 32)) (sb 32))
                update-weight update-weight-clip))
(defun update-weight (weight delta source result)
  (declare (type (sb 32) delta result source))
  (if (and (/= source 0)
           (/= result 0))
      (let ((sign (ash (logxor source result) -31)))
        (+ (logxor delta sign)
           weight
           (- sign))) weight))

(defun update-weight-clip (weight delta source result)
  (declare (type (signed-byte 32) weight delta source result))
  (if (and (/= source 0)
           (/= result 0))
      (let* ((sign (ash (logxor source result) -31))
             (weight (+ (logxor weight sign) (- delta sign))))
        (if (> weight 1024)
            (setq weight 1024))
        (- (logxor weight sign) sign))
      weight))

(declaim (ftype (function ((sb 32) (sb 32)) (sb 32))
                correlate-sample/w-term-17
                correlate-sample/w-term-18))

(defun correlate-sample/w-term-17 (i-1 i-2)
  (declare (type (sb 32) i-1 i-2))
  (- (* 2 i-1) i-2))

(defun correlate-sample/w-term-18 (i-1 i-2)
  (declare (type (sb 32) i-1 i-2))
  (+ i-1 (ash (- i-1 i-2) -1)))

(defmacro correlate-sample (sample-form result-place weight-place update-method)
  ;; Must be called with 'delta'in the scope
  (let ((sample-sym (gensym)))
    `(let ((,sample-sym ,sample-form))
       (psetf ,result-place
              (+ (apply-weight ,weight-place ,sample-sym) ,result-place)
              ,weight-place
              (,update-method ,weight-place delta ,sample-sym ,result-place)))))

(macrolet ((define-correlation-pass/w-term>8 (name correlate-sample-name)
             `(defun ,name (residual delta weight &key decorr-samples)
                (declare (type (sb 32) weight delta)
                         (type (sa-sb 32) residual))

                (cond
                  (decorr-samples
                   (let ((decorr-samples decorr-samples))
                     (declare (type (sa-sb 32) decorr-samples))

                     ;; The first sample in the block
                     (correlate-sample (,correlate-sample-name
                                        (aref decorr-samples 0)
                                        (aref decorr-samples 1))
                                       (aref residual 0)
                                       weight update-weight)

                     ;; The second sample in the block
                     (correlate-sample (,correlate-sample-name
                                        (aref residual 0)
                                        (aref decorr-samples 0))
                                       (aref residual 1)
                                       weight update-weight)))
                  (t
                   (correlate-sample (,correlate-sample-name
                                      (aref residual 0)
                                      0)
                                     (aref residual 1)
                                     weight update-weight)))

                (loop for j from 2 below (length residual) do
                     (correlate-sample (,correlate-sample-name
                                        (aref residual (- j 1))
                                        (aref residual (- j 2)))
                                       (aref residual j)
                                       weight update-weight))
                weight)))

  (define-correlation-pass/w-term>8 correlation-pass/w-term-17 correlate-sample/w-term-17)
  (define-correlation-pass/w-term>8 correlation-pass/w-term-18 correlate-sample/w-term-18))

(defun correlation-pass/w-term-i (residual delta weight term &key decorr-samples)
  (declare (type (sb 32) weight delta)
           (type (integer 1 8) term)
           (type (sa-sb 32) residual))

  (if decorr-samples
      (let ((decorr-samples decorr-samples))
        (declare (type (sa-sb 32) decorr-samples))
        (loop for j below (length decorr-samples) do
             (correlate-sample (aref decorr-samples j)
                               (aref residual j)
                               weight update-weight))))

  (loop for j from term below (length residual) do
       (correlate-sample (aref residual (- j term))
                         (aref residual j)
                         weight update-weight))

  weight)

(defun correlation-pass/w-term--1 (residual delta weights &key decorr-samples)
  (let ((residual-1 (first  residual))
        (residual-2 (second residual)))
    (declare (type (sa-sb 32) residual-1 residual-2 weights)
             (type (sb 32) delta))
    (when decorr-samples
      (correlate-sample (first decorr-samples)
                        (aref residual-1 0)
                        (aref weights 0)
                        update-weight-clip))

    (correlate-sample (aref residual-1 0)
                      (aref residual-2 0)
                      (aref weights 1)
                      update-weight-clip)

    (loop for i from 1 below (length residual-1) do
         (correlate-sample
          (aref residual-2 (1- i))
          (aref residual-1 i)
          (aref weights 0)
          update-weight-clip)
         (correlate-sample
          (aref residual-1 i)
          (aref residual-2 i)
          (aref weights 1)
          update-weight-clip))))

(defun correlation-pass/w-term--2 (residual delta weights &key decorr-samples)
  (let ((residual-1 (first  residual))
        (residual-2 (second residual)))
    (declare (type (sa-sb 32) residual-1 residual-2 weights)
             (type (sb 32) delta))
    (when decorr-samples
      (correlate-sample (second decorr-samples)
                        (aref residual-2 0)
                        (aref weights 1)
                        update-weight-clip))

    (correlate-sample (aref residual-2 0)
                      (aref residual-1 0)
                      (aref weights 0)
                      update-weight-clip)

    (loop for i from 1 below (length residual-1) do
         (correlate-sample
          (aref residual-1 (1- i))
          (aref residual-2 i)
          (aref weights 1)
          update-weight-clip)
         (correlate-sample
          (aref residual-2 i)
          (aref residual-1 i)
          (aref weights 0)
          update-weight-clip))))

(defun correlation-pass/w-term--3 (residual delta weights &key decorr-samples)
  (let ((residual-1 (first  residual))
        (residual-2 (second residual)))
    (declare (type (sa-sb 32) residual-1 residual-2 weights)
             (type (sb 32) delta))
    (when decorr-samples
      (correlate-sample
       (first decorr-samples)
       (aref residual-1 0)
       (aref weights 0)
       update-weight-clip)
      (correlate-sample
       (second decorr-samples)
       (aref residual-1 1)
       (aref weights 1)
       update-weight-clip))

    (loop for i from 1 below (length residual-1) do
         (correlate-sample
          (aref residual-1 (1- i))
          (aref residual-2 i)
          (aref weights 1)
          update-weight-clip)
         (correlate-sample
          (aref residual-2 (1- i))
          (aref residual-1 i)
          (aref weights 0)
          update-weight-clip))))

(defun restore-joint-stereo (residual-1 residual-2)
  (declare (type (sa-sb 32) residual-1 residual-2))
  (map-into residual-2 (lambda (sample-1 sample-2)
                         (declare (type (sb 32) sample-1 sample-2))
                         (- sample-2 (ash sample-1 -1)))
            residual-1 residual-2)
  (map-into residual-1 #'+
            residual-1 residual-2))

(defun decode-wv-block (wv-block)
  "Decode a wavpack block, destructively modifying it.
   This function returns a list of simple-arrays, each
   correspoding to a separate channel"
  (let ((decorr-samples (block-decorr-samples wv-block))
        (decorr-passes (block-decorr-passes wv-block))
        (residual (block-residual wv-block))) ; Will be destructively modified to output

    (if (flag-set-p wv-block +flags-hybrid-mode+)
        (error 'block-error :message "Hybrid encoding is not supported"))

    (flet ((correlation-pass (pass &key decorr-samples)
             (let ((term (decorr-pass-term pass))
                   (delta (decorr-pass-delta pass))
                   (weights (decorr-pass-weight pass)))
               (cond
                 ((> term 0)
                  (do ((i 0 (1+ i))
                       (residual% residual (cdr residual%))
                       (decorr-samples% decorr-samples (cdr decorr-samples%)))
                      ((null residual%))

                    (setf (aref weights i)
                          (cond
                            ((= term 18) (correlation-pass/w-term-18
                                          (car residual%) delta (aref weights i)
                                          :decorr-samples (car decorr-samples%)))
                            ((= term 17) (correlation-pass/w-term-17
                                          (car residual%) delta (aref weights i)
                                          :decorr-samples (car decorr-samples%)))
                            (t           (correlation-pass/w-term-i
                                          (car residual%) delta (aref weights i) term
                                          :decorr-samples (car decorr-samples%)))))))
                 (t
                  (cond
                    ((= term -1) (correlation-pass/w-term--1 residual delta weights
                                                             :decorr-samples decorr-samples))
                    ((= term -2) (correlation-pass/w-term--2 residual delta weights
                                                             :decorr-samples decorr-samples))
                    ((= term -3) (correlation-pass/w-term--3 residual delta weights
                                                             :decorr-samples decorr-samples))))))
             t))

      (if decorr-passes
          (destructuring-bind (last . first) decorr-passes
            (mapc #'correlation-pass (reverse first))
            (correlation-pass last :decorr-samples decorr-samples))))

    (let ((shift (left-shift-amount wv-block)))
      (declare (type (ub 8) shift))
      (labels ((shift-sample (sample)
                 (the (sb 32) (ash sample shift)))
               (shift-channel (channel-out)
                 (declare (type (sa-sb 32) channel-out))
                 (map-into channel-out #'shift-sample channel-out)))

        (if (/= shift 0)
            (mapc #'shift-channel residual))))

    (if (flag-set-p wv-block +flags-stereo-joint+)
        (restore-joint-stereo (first residual) (second residual)))

    residual))
