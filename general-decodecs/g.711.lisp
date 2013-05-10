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
