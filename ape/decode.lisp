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

(defparameter *filter-orders*
  '(()
    (16)
    (64)
    (32 256)
    (16 256 1280)))

(defparameter *fracbits*
  '(()
    (11)
    (11)
    (10 13)
    (11 13 15)))

(defun predictor-promote-version (version)
  (find version *predictor-versions* :test #'>=))

;;(declaim (inline make-dispaced-array))
(defun make-displaced-array (array offset size)
  (declare (type simple-array array))
  (make-array size
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset offset))

;;(declaim (inline dot-product))
(defun dot-product (x y)
  (declare (type (array (sb 16)) x y))
  (reduce
   #'+
   (map-into
    (make-array (length x)
                :element-type '(sb 32))
    #'* x y)))

;;(declaim (inline clamp))
(defun clamp (x min max)
  (declare (type (sb 32) x min max))
  (min (max x min) max))

(defun decode-frame (frame)
  (predictor-decode
   frame
   (predictor-promote-version
    (frame-version frame))))

(defun apply-filter (entropy order fracbits)
  (declare (type (sa-sb 32) entropy)
           (type (integer 1 15) fracbits)
           (type non-negative-fixnum order))
  (let ((coeffs (make-array order
                            :element-type '(sb 16)
                            :initial-element 0))
        (buffer (make-array (+ +history-size+ (ash order 1))
                            :element-type '(sb 16)
                            :initial-element 0))
        (avg 0))
    (declare (type (sa-sb 16) coeffs buffer)
             (type (sb 32) avg))
    (dotimes (i (length entropy))
      (let* ((buffer-idx (rem i +history-size+))
             (history (make-displaced-array buffer buffer-idx order))
             (adapt (make-displaced-array buffer (+ buffer-idx order) order))
             (current-data (aref entropy i)))

        ;; Copy data from back to start when... we need to
        (when (zerop buffer-idx)
          (map-into buffer #'identity
                    (make-displaced-array
                     buffer +history-size+
                     (ash order 1))))

        ;; Decoded output
        (let ((new-data
                (+ current-data
                   (ash (+ (ash 1 (1- fracbits))
                           (dot-product coeffs adapt))
                        (- fracbits)))))

          ;; Update filter coeffs
          (map-into coeffs
                    (lambda (x y)
                      (declare (type (sb 16) x y))
                      (- x (* y (signum current-data))))
                    coeffs history)

          (setf (aref entropy i)
                new-data
                (aref buffer (+ buffer-idx (ash order 1)))
                (clamp new-data -32768 32767)
                (aref adapt 0)
                (cond
                  ((<= (abs new-data)
                       (truncate (* avg 4) 3))
                   (* -8 (signum new-data)))
                  ((<= (abs new-data)
                       (* avg 3))
                   (* -16 (signum new-data)))
                  (t
                   (* -32 (signum new-data)))))
          (incf avg (truncate (- (abs new-data) avg) 16))

        (symbol-macrolet
            ((x-1 (aref history (+ order -1)))
             (x-2 (aref history (+ order -2)))
             (x-8 (aref history (+ order -8))))
          (setf x-1 (ash x-1 -1)
                x-2 (ash x-2 -1)
                x-8 (ash x-8 -1)))))))
  entropy)

(defmethod predictor-decode (frame (version (eql 3950)))
  (declare (ignore version))
  (let ((orders   (nth (frame-fset frame) *filter-orders*))
        (fracbits (nth (frame-fset frame) *fracbits*)))
    (flet ((apply-filter-channels (order fracbits)
             (mapc (lambda (channel)
                     (apply-filter channel order fracbits))
                   (frame-output frame))))
      (mapc #'apply-filter-channels orders fracbits)))
  frame)
