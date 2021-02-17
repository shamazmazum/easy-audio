(in-package :easy-audio.ape)

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

(defparameter *predictor-versions*
  '#.(reverse '(0 3930 3950)))

(defun predictor-promote-version (version)
  (find version *predictor-versions* :test #'>=))

(defun decode-frame (frame)
  (predictor-decode
   frame
   (predictor-promote-version
    (frame-version frame)))
  frame)
