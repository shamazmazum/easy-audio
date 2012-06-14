(in-package :cl-flac)

(defun bytes-to-integer-big-endian (array)
  (loop for i below (length array) sum
	(let ((mul (expt 2 (* 8 (- (length array) 1 i)))))
	 (* mul (aref array i)))))

(defun read-to-integer (stream num &optional (func #'bytes-to-integer-big-endian))
  (let ((buffer (make-array num :element-type 'u8)))
    (if (/= (read-sequence buffer stream) num) (error "Unexpected end of stream"))
    (funcall func buffer)))

(defun integer-to-array (val array size &key
			     signed
			     (len (length array))
			     (offset 0))
  "Converts value to array of integers of size bits each
   big endian
   definitely a bottleneck"
  (declare (type integer val size len offset))
  (loop for i from offset below len do
	(setf (aref array i)
	      (if signed (unsigned-to-signed
			  (ldb (byte size (* size (- len 1 i))) val)
			  size)
		(ldb (byte size (* size (- len 1 i))) val))))
  array)

(defun read-utf8-u32 (stream)
  "for reading frame number
   copy from libFLAC"
  (let ((x (read-byte stream))
	i v)
    (cond
     (( = 0 (logand x #x80))
      (setq v x i 0))
     
     ((and
       (= 0 (logand x #x20))
       (/= 0 (logand x #xC0)))
      (setq v (logand x #x1F) i 1))

     ((and
       (= 0 (logand x #x10))
       (/= 0 (logand x #xE0)))
      (setq v (logand x #x0F) i 2))

     ((and
       (= 0 (logand x #x08))
       (/= 0 (logand x #xF0)))
      (setq v (logand x #x07) i 3))

     ((and
       (= 0 (logand x #x04))
       (/= 0 (logand x #xF8)))
      (setq v (logand x #x03) i 4))

     ((and
       (= 0 (logand x #x02))
       (/= 0 (logand x #xFC)))
      (setq v (logand x #x01) i 5))
     
     (t (error "Error reading utf-8 coded 8-48bit value")))

    (loop for j from i downto 1 do
	  (setq x (read-byte stream))
	  (if (or
	       (= 0 (logand x #x80))
	       (/= 0 (logand x #x40)))
	      (error "Error reading utf-8 coded 8-48bit value"))
	  (setq v (ash v 6))
	  (setq v (logior v (logand x #x3F))))
    v))

(defun read-n-bits (stream n)
  "Reads n bits from stream ands returns these bits and remainder"
  ;; Calculate bytes we need to read and size of a remainder
  (multiple-value-bind (bytes rest) (ceiling (/ n 8))
    (setq rest (abs (* rest 8)))
    ;; Read data and store the remainder
    (let ((buffer (read-to-integer stream bytes)))
      ;; Return the data, the remainder, size of readed data
      ;; and t, if there is the remainder, nil otherwise
      (if (/= 0 rest)
	  (values (ldb (byte n rest) buffer)
		  (ldb (byte rest 0) buffer)
		  rest
		  t)
	(values (ldb (byte n rest) buffer)
		0
		0
		nil)))))

(defun make-bit-reader (stream)
  "For cases if block of data is not byte aligned.
  Returns closure around stream - bit reader function.
  Big endian"
  (let (filled chunk size res)
    (flet ((bit-reader% (n)
			"Returns readed bit and remainder to byte alignment"
			;; Currently there is no remainder
			(if (not filled)
			    (multiple-value-setq (res chunk size filled)
			      (read-n-bits stream n))
			  ;; There is a remainder
			  (if (<= n size)
			      ;; Remainder is not lesser than we need
			      (let ((oldchunk chunk))
				(if (= n size) (setq filled nil))
				(setq size (- size n))
				(setq chunk (ldb (byte size 0) oldchunk)
				      res (ldb (byte n size) oldchunk)))
			    ;; Remainder is lesser. Read from the stream
			    (progn
			      (setq res chunk)
			      (let (lowres (oldsize size))
				(multiple-value-setq (lowres chunk size filled)
				  (read-n-bits stream (- n size)))
				(setq res (+ lowres (ash res (- n oldsize))))))))
			(values res chunk)))
      #'bit-reader%)))

(defun unsigned-to-signed (byte len)
  ;; Slow implementation
  (let ((sign (ldb (byte 1 (1- len)) byte)))
    (if (= sign 0) byte (- 0 1 (logand (lognot byte) (1- (ash 1 len)))))))

(defun read-unary-coded-integer (bitreader &optional (one 0))
  "Read unary coded integer from bitreader
   By default 0 bit is considered as 1, 1 bit is terminator"
  (declare (type (integer 0 1) one))
  (loop for bit = (funcall bitreader 1)
	while (= bit one)
	sum 1))

(defun read-rice-signed (bitreader param)
  (declare (type integer param))
  (let* ((unary (read-unary-coded-integer bitreader))
	 (binary (funcall bitreader param))
	 (val (logior (ash unary param) binary)))
    
    (if (= (ldb (byte 1 0) val) 1)
	(- 0 (ash val -1) 1)
      (ash val -1))))
