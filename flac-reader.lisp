(in-package :cl-flac)

(defun bytes-to-integer-big-endian (array)
  (declare (type (simple-array u8) array))
  (loop for i below (length array) sum
	(let ((mul (expt 2 (* 8 (- (length array) 1 i)))))
	 (* mul (aref array i)))))

(defun read-to-integer (stream num &optional (func #'bytes-to-integer-big-endian))
  (let ((buffer (make-array num :element-type 'u8)))
    (read-sequence buffer stream)
    (funcall func buffer)))

(defun octets-to-n-bit-bytes (array new-array n)
  "n mod 8 must be 0
   big endian
   definitely a bottleneck"
  (declare
   (type (simple-array u8) array)
   (type integer n))
  (let ((scale (/ n 8)))
    
    (loop for i below (length new-array) do
	  (let ((start (* i scale)))
	    (setf (aref new-array i)
		  (loop for j from start to (+ start scale -1) sum
			(* (expt 2 (* 8 (+ start scale (- 0 1 j))))
			   (aref array j))))))
    new-array))

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
