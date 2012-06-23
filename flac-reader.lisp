(in-package :cl-flac)

(defun integer-to-array (val array size &key
			     signed
			     (len (length array))
			     (offset 0))
  "Converts value to array of integers of size bits each
   big endian
   definitely a bottleneck"
  (declare (type integer val size len offset))
  (let ((pos (* size (1- len))))
    (loop for i from offset below len do
	  (setf (aref array i)
		(if signed (unsigned-to-signed
			    (ldb (byte size pos) val)
			    size)
		  (ldb (byte size pos) val)))
	  (decf pos size)))
  array)

(defun read-utf8-u32 (stream)
  "for reading frame number
   copy from libFLAC"
  (let ((x (tbs:read-octet stream))
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
	  (setq x (tbs:read-octet stream))
	  (if (or
	       (= 0 (logand x #x80))
	       (/= 0 (logand x #x40)))
	      (error "Error reading utf-8 coded 8-48bit value"))
	  (setq v (ash v 6))
	  (setq v (logior v (logand x #x3F))))
    v))

(defun unsigned-to-signed (byte len)
  ;; Slow implementation
  (let ((sign (ldb (byte 1 (1- len)) byte)))
    (if (= sign 0) byte (- byte (ash 1 len)))))

(defun read-unary-coded-integer (bitreader &optional (one 0))
  "Read unary coded integer from bitreader
   By default 0 bit is considered as 1, 1 bit is terminator"
  (declare (type (integer 0 1) one))
  (loop for bit = (tbs:read-bit bitreader)
	while (= bit one)
	sum 1))

(defun read-rice-signed (bitreader param)
  (declare (type integer param))
  (let* ((unary (read-unary-coded-integer bitreader))
	 (binary (tbs:read-bits param bitreader))
	 (val (logior (ash unary param) binary)))
    
    (if (= (ldb (byte 1 0) val) 1)
	(- 0 (ash val -1) 1)
      (ash val -1))))
