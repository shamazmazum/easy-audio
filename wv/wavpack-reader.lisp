;; This is actually utility functions just like in flac/flac-reader.lisp
(in-package :easy-audio.wv)

(declaim (type (sa-ub 8) +exp2-table+))
(define-constant +exp2-table+
    (make-array (list 256)
                :element-type '(ub 8)
                :initial-contents
                '#.(flet ((calc (x)
                            (let* ((val (* 256 (1- (expt 2 (/ x 256.0)))))
                                   (int-val (floor val)))
                              (if (< (- val int-val)
                                     (- int-val val -1))
                                  int-val (1+ int-val)))))
                     (loop for x below 256 collect (calc x))))
  :test #'equalp)

(sera:-> exp2s ((ub 16))
         (values (sb 32) &optional))
(defun exp2s (val)
  (declare (optimize (speed 3)))
  (if (< val #x8000)
      (let ((m (logior (aref +exp2-table+
                             (logand val #xff))
                       #x100))
            (exp (ash val -8)))
        (ash m (- exp 9)))
      (- (exp2s (1+ (logxor #xffff val))))))

;; From flac reader
(sera:-> read-unary-coded-integer (reader &optional (or non-negative-fixnum null))
         (values non-negative-fixnum &optional))
(defun read-unary-coded-integer (bitreader &optional limit)
  "Read an unary coded integer from bitreader 1 bit is considered as
arithmetical 1, 0 bit signals termination."
  (declare (optimize (speed 3)))
  (loop for res fixnum from 0 by 1
        until (or (zerop (read-bit-bw bitreader))
                  (and limit (= res limit)))
        finally (return res)))

(sera:-> read-elias-code (reader)
         (values non-negative-fixnum &optional))
(defun read-elias-code (reader)
  (declare (optimize (speed 3)))
  (let ((ones-num (read-unary-coded-integer reader)))
    (declare (type (integer 0 32) ones-num))
    (if (zerop ones-num) 0
        (let ((shift (1- ones-num)))
          (logior (ash 1 shift)
                  (read-bits-bw shift reader))))))

(sera:-> read-code (reader non-negative-fixnum)
         (values non-negative-fixnum &optional))
(defun read-code (reader maxvalue)
  (declare (optimize (speed 3)))
  (cond
    ((= maxvalue 0) 0)
    ((= maxvalue 1)
     (read-bit-bw reader))
    (t
     (let* ((bits (integer-length maxvalue))
            (extra (- (ash 1 bits) maxvalue 1))
            (res (read-bits-bw (1- bits) reader)))
       (if (< res extra) res
           (+ (ash res 1) (read-bit-bw reader) (- extra)))))))
