;; This is actually utility functions just like in flac/flac-reader.lisp
(in-package :easy-audio.wv)

;; FIXME: is there a better way to keep these magic numbers out of code?
;; Can we calculate them in place?
(declaim (type (sa-ub 8) +exp2-table+))
(defparameter +exp2-table+
  (make-array (list 256)
              :element-type '(ub 8)
              :initial-contents
              '#.(flet ((calc (x)
                          (let* ((val (* 256 (1- (expt 2 (/ x 256.0)))))
                                 (int-val (floor val)))
                            (if (< (- val int-val)
                                   (- int-val val -1))
                                int-val (1+ int-val)))))
                   (loop for x below 256 collect (calc x)))))

(declaim (ftype (function ((ub 16)) (sb 32)) exp2s))
(defun exp2s (val)
  (declare (optimize (speed 3))
           (type (ub 16) val))
  (if (< val #x8000)
      (let ((m (logior (aref +exp2-table+
                             (logand val #xff))
                       #x100))
            (exp (ash val -8)))
        (ash m (- exp 9)))
      (- (exp2s (1+ (logxor #xffff val))))))


;; Next two functions are just a KLUDGE and almost copy functionality of the bitreader.
;; Try to develop more flexible bitreader instead
(declaim (ftype (function (reader) bit) residual-read-bit))
(defun residual-read-bit (reader)
  (declare (optimize #+easy-audio-unsafe-code
                     (safety 0) (speed 3)))
  (with-accessors ((ibyte easy-audio.bitreader::reader-ibyte)
                   (ibit  easy-audio.bitreader::reader-ibit)
                   (end   easy-audio.bitreader::reader-end))
      reader
  (if (< ibyte end)
      (prog1
          (ldb (byte 1 ibit)
               (aref (easy-audio.bitreader::reader-buffer reader) ibyte))
        (if (= ibit 7)
            (setf ibit 0
                  ibyte (1+ ibyte))
            (incf ibit)))
      (error 'bitreader-eof :bitreader reader))))

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
(declaim (ftype (function (t &optional (or non-negative-fixnum null))
                          non-negative-fixnum)
		read-unary-coded-integer))
(defun read-unary-coded-integer (bitreader &optional limit)
  "Read an unary coded integer from bitreader
   1 bit is considered as arithmetical 1,
   0 bit signals termination"
  (declare (optimize (speed 3))
           (type (or non-negative-fixnum null) limit))
  (do ((res   0 (1+ res)))
      ((or (= (residual-read-bit bitreader) 0)
           (and limit (= res limit))) res)
    (declare (type non-negative-fixnum res)) ()))

(declaim (ftype (function (t) non-negative-fixnum) read-elias-code))
(defun read-elias-code (reader)
  (declare (optimize (speed 3)))
  (let ((ones-num (read-unary-coded-integer reader)))
    (declare (type (integer 0 32) ones-num))
    (if (/= ones-num 0)
        (let ((shift (the (integer 0 32) ; as to format limits
                          (1- ones-num))))
          (logior (ash 1 shift)
                  (residual-read-bits shift reader))) 0)))

(declaim (ftype (function (t non-negative-fixnum) non-negative-fixnum) read-code))
(defun read-code (reader maxvalue)
  (declare (optimize (speed 3))
           (type non-negative-fixnum maxvalue))
  (if (< maxvalue 2)
      (if (/= maxvalue 0) (residual-read-bit reader) 0)
      (let* ((bits (integer-length maxvalue))
             (extra (- (ash 1 bits) maxvalue 1))
             (res (residual-read-bits (1- bits) reader)))
        (declare (type non-negative-fixnum bits extra res)
                 (type (integer 0 32) bits))
        (if (< res extra) res
            (+ (ash res 1) (residual-read-bit reader) (- extra))))))
