(in-package :easy-audio.wv)

;; NB: multiplication of weight and sample may be a bignum
(sera:-> apply-weight ((sb 32) (sb 32))
         (values (sb 32) &optional))
(declaim (inline apply-weight))
(defun apply-weight (weight sample)
  (ash (+ 512 (* weight sample)) -10))

(sera:-> update-weight ((sb 32) (sb 32) (sb 32) (sb 32))
         (values (sb 32) &optional))
(declaim (inline update-weight))
(defun update-weight (weight delta source result)
  (if (or (zerop source)
          (zerop result))
      weight
      (let ((sign (ash (logxor source result) -31)))
        (+ (logxor delta sign) weight (- sign)))))

(sera:-> update-weight-clip ((sb 32) (sb 32) (sb 32) (sb 32))
         (values (sb 32) &optional))
(declaim (inline update-weight-clip))
(defun update-weight-clip (weight delta source result)
  (if (or (zerop source)
          (zerop result))
      weight
      (let* ((sign (ash (logxor source result) -31))
             (weight (+ (logxor weight sign) (- delta sign))))
        (- (logxor (min weight 1024) sign) sign))))

(sera:-> correlate-sample/w-term-17 ((sb 32) (sb 32))
         (values (sb 32) &optional))
(declaim (inline correlate-sample/w-term-17))
(defun correlate-sample/w-term-17 (i-1 i-2)
  (- (* 2 i-1) i-2))

(sera:-> correlate-sample/w-term-18 ((sb 32) (sb 32))
         (values (sb 32) &optional))
(declaim (inline correlate-sample/w-term-18))
(defun correlate-sample/w-term-18 (i-1 i-2)
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
             `(progn
                (sera:-> ,name ((sa-sb 32) (sb 32) (sb 32) integer (maybe (sa-sb 32)))
                         (values (sb 32) &optional))
                (defun ,name (residual delta weight term decorr-samples)
                  (declare (optimize (speed 3))
                           (ignore term))
                  (cond
                    (decorr-samples
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
                                       weight update-weight))
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
                  weight))))

  (define-correlation-pass/w-term>8 correlation-pass/w-term-17 correlate-sample/w-term-17)
  (define-correlation-pass/w-term>8 correlation-pass/w-term-18 correlate-sample/w-term-18))

(sera:-> correlation-pass/w-term-i
         ((sa-sb 32) (sb 32) (sb 32) (integer 1 8) (maybe (sa-sb 32)))
         (values (sb 32) &optional))
(defun correlation-pass/w-term-i (residual delta weight term decorr-samples)
  (declare (optimize (speed 3)))
  (when decorr-samples
    (loop for j below term do
          (correlate-sample (aref decorr-samples j)
                            (aref residual j)
                            weight update-weight)))
  (loop for j from term below (length residual) do
       (correlate-sample (aref residual (- j term))
                         (aref residual j)
                         weight update-weight))
  weight)

(sera:-> correlation-pass/w-term--1
         ((sa-sb 32) (sa-sb 32) (sb 32) (sa-sb 32) list)
         (values &optional))
(defun correlation-pass/w-term--1 (residual-1 residual-2 delta weights decorr-samples)
  (declare (optimize (speed 3)))
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
         update-weight-clip))
  (values))

(sera:-> correlation-pass/w-term--2
         ((sa-sb 32) (sa-sb 32) (sb 32) (sa-sb 32) list)
         (values &optional))
(defun correlation-pass/w-term--2 (residual-1 residual-2 delta weights decorr-samples)
  (declare (optimize (speed 3)))
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
         update-weight-clip))
  (values))

(sera:-> correlation-pass/w-term--3
         ((sa-sb 32) (sa-sb 32) (sb 32) (sa-sb 32) list)
         (values &optional))
(defun correlation-pass/w-term--3 (residual-1 residual-2 delta weights decorr-samples)
  (declare (optimize (speed 3)))
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
         update-weight-clip))
  (values))

(sera:-> restore-joint-stereo ((sa-sb 32) (sa-sb 32))
         (values (sa-sb 32) &optional))
(defun restore-joint-stereo (residual-1 residual-2)
  (declare (optimize (speed 3)))
  (map-into residual-2
            (lambda (sample-1 sample-2)
              (- sample-2 (ash sample-1 -1)))
            residual-1 residual-2)
  (map-into residual-1 #'+
            residual-1 residual-2))

;; TODO: Refactor
(defun int32-fixup (wv-block)
  "Do samples fixup if sample size is > 24 bits"
  (declare (optimize (speed 3)))
  ;; How slow is this?
  (let ((int32-info (block-int32-info wv-block))
        (wvx-bits (block-wvx-bits wv-block)))
    (unless int32-info
      (error 'block-error
             :format-control "sample size is > 24 bits and no int32-info metadata block"))
    (let ((sent-bits (metadata-sent-bits int32-info))
          (zeros (metadata-zeros int32-info))
          (ones (metadata-ones int32-info))
          (dups (metadata-dups int32-info))
          (shift-add 0))
      (declare (type (ub 8) sent-bits zeros ones dups shift-add))
      (labels ((fixup-sample (sample)
                 (declare (type (sb 32) sample))
                 (cond
                   ((/= zeros 0)
                    (the (sb 32) (ash sample zeros)))
                   ((/= ones 0)
                    (1- (the (sb 32) (ash (1+ sample) ones))))
                   ((/= dups 0)
                    (- (the (sb 32) (ash (+ sample (logand sample 1)) dups))
                       (logand sample 1)))
                   (t sample)))
               (fixup-sample-wvx (sample fixup)
                 (declare (type (sb 32) sample fixup))
                 (fixup-sample (logior (the (sb 32) (ash sample sent-bits)) fixup))))
        (cond
          (wvx-bits
           (labels ((fixup-channel (channel wvx-bits)
                      (declare (type (sa-sb 32) channel wvx-bits))
                      (map-into channel #'fixup-sample-wvx channel wvx-bits)))
             (mapc #'fixup-channel (block-residual wv-block) wvx-bits)))
           ((and (= sent-bits 0)
                 (or (/= zeros 0)
                     (/= ones 0)
                     (/= dups 0)))
           (labels ((fixup-channel (channel)
                      (declare (type (sa-sb 32) channel))
                      (map-into channel #'fixup-sample channel)))
             (mapc #'fixup-channel (block-residual wv-block))))
          (t (setq shift-add (+ zeros sent-bits ones dups)))))
        shift-add)))

(defun decode-wv-block (wv-block)
  "Decode a wavpack block, destructively modifying it. This function
returns a list of simple-arrays, each correspoding to a separate
channel."
  (declare (optimize (speed 3)))
  (let ((decorr-samples (block-decorr-samples wv-block))
        (decorr-passes (block-decorr-passes wv-block))
        (residual (block-residual wv-block))) ; Will be destructively modified to output

    (when (flag-set-p wv-block +flags-hybrid-mode+)
      (error 'block-error :format-control "Hybrid encoding is not supported"))

    (flet ((correlation-pass (pass &optional decorr-samples)
             (let ((term (decorr-pass-term pass))
                   (delta (decorr-pass-delta pass))
                   (weights (decorr-pass-weight pass)))
               (if (> term 0)
                   (do ((i 0 (1+ i))
                        (r residual (cdr r))
                        (ds decorr-samples (cdr ds)))
                       ((null r))
                     (setf (aref weights i)
                           (funcall
                            (cond
                              ((= term 18) #'correlation-pass/w-term-18)
                              ((= term 17) #'correlation-pass/w-term-17)
                              (t           #'correlation-pass/w-term-i))
                            (car r) delta (aref weights i) term
                            (car ds))))
                   (funcall
                    (cond
                      ((= term -1) #'correlation-pass/w-term--1)
                      ((= term -2) #'correlation-pass/w-term--2)
                      ((= term -3) #'correlation-pass/w-term--3))
                    (first residual) (second residual) delta weights decorr-samples)))
             (values)))

      (when decorr-passes
        (destructuring-bind (last . first) decorr-passes
          (mapc #'correlation-pass (reverse first))
          (correlation-pass last decorr-samples))))

    (let ((shift (left-shift-amount wv-block)))
      (declare (type (ub 8) shift))
      (when (flag-set-p wv-block +flags-shifted-int+)
        (incf shift (int32-fixup wv-block)))
      (labels ((shift-sample (sample)
                 (the (sb 32) (ash sample shift)))
               (shift-channel (channel-out)
                 (declare (type (sa-sb 32) channel-out))
                 (map-into channel-out #'shift-sample channel-out)))
        (when (/= shift 0)
          (mapc #'shift-channel residual))))

    (when (flag-set-p wv-block +flags-stereo-joint+)
      (restore-joint-stereo (first residual) (second residual)))

    residual))
