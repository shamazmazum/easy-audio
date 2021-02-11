(in-package :easy-audio.ape)

(defconstant +mono-silence+ 1)
(defconstant +stereo-silence+ 3)
(defconstant +pseudo-stereo+ 4)
(defconstant +history-size+ 512)
(defconstant +predictor-order+ 8)

(defconstant +ydelaya+ (+ 18 (* +predictor-order+ 4)))
(defconstant +ydelayb+ (+ 18 (* +predictor-order+ 3)))
(defconstant +xdelaya+ (+ 18 (* +predictor-order+ 2)))
(defconstant +xdelayb+ (+ 18 (* +predictor-order+ 1)))

(defconstant +yadaptcoeffsa+ 18)
(defconstant +xadaptcoeffsa+ 14)
(defconstant +yadaptcoeffsb+ 10)
(defconstant +xadaptcoeffsb+  5)


(defun decode-frame (reader metadata n)
  ;; FIXME: flushes reader's buffer
  (reader-position reader (tell-frame-start-and-size metadata n))
  t)
