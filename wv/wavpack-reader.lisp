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

;; Will always return fixnum on x86-64
(declaim (ftype (function (non-negative-integer reader) non-negative-fixnum) residual-read-bits))
(defun residual-read-bits (bits reader)
  (declare (optimize #+easy-audio-unsafe-code
                     (safety 0) (speed 3))
           (type non-negative-fixnum bits))

  (let ((result 0)
        (already-read 0))
    (declare (type non-negative-fixnum result already-read))

    (with-accessors ((ibit  easy-audio.bitreader::reader-ibit)
                     (ibyte easy-audio.bitreader::reader-ibyte)
                     (end   easy-audio.bitreader::reader-end))
        reader
      (dotimes (i (ceiling (+ bits ibit) 8))
        (if (= ibyte end) (error 'bitreader-eof :bitreader reader))
        (let ((bits-to-add (min bits (- 8 ibit))))
          (declare (type bit-counter bits-to-add))
          (setq result
                (logior result
                        (the non-negative-fixnum
                             (ash (ldb
                                   (byte bits-to-add ibit)
                                   (aref (easy-audio.bitreader::reader-buffer reader) ibyte))
                                  already-read)))
                bits (- bits bits-to-add)
                already-read (+ already-read bits-to-add))

          (incf ibit bits-to-add)
          (if (= ibit 8)
              (setf ibit 0
                    ibyte (1+ ibyte))))))
    result))

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
                  (residual-read-bits shift reader))))))

(sera:-> read-code (reader non-negative-fixnum)
         (values non-negative-fixnum &optional))
(defun read-code (reader maxvalue)
  (declare (optimize (speed 3)))
  (if (< maxvalue 2)
      (if (zerop maxvalue) 0 (read-bit-bw reader))
      (let* ((bits (integer-length maxvalue))
             (extra (- (ash 1 bits) maxvalue 1))
             (res (residual-read-bits (1- bits) reader)))
        (if (< res extra) res
            (+ (ash res 1) (read-bit-bw reader) (- extra))))))
