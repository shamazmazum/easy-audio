(in-package :easy-audio.general)

(declaim (optimize (speed 3)))

(declaim (ftype (function ((ub 8)) (sb 16)) g.711-alaw-decode))
(defun g.711-alaw-decode (coded-value)
  "Decode 8-bit unsigned A-law coded data to 16-bit signed data"
  (let* ((toggled-bits (logxor coded-value #x55))
         (mantissa (ldb (byte 4 0) toggled-bits))
         (exp (ldb (byte 3 4) toggled-bits))
         (res
          (if (= exp 0) (+ (ash mantissa 4) #x8)
            (ash (+ (ash mantissa 4) #x108)
                 (1- exp)))))

    (if (> coded-value #x7f) res (- res))))

(declaim (ftype (function ((ub 8)) (sb 16)) g.711-ulaw-decode))
(defun g.711-ulaw-decode (coded-value)
  "Decode 8-bit unsigned mu-law coded data to 16-bit signed data"
  (let* ((inv (- #xff coded-value))
         (exp (ldb (byte 3 4) inv))
         (mantissa (ldb (byte 4 0) inv))
         (res (ash (+ (ash mantissa 3) #x84) exp)))

    (if (> coded-value #x7f)
        (- res #x84)
        (- #x84 res))))
