;; Thanks to Love5an, author of trivial-bit-streams

(in-package :cl-flac)

(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))
(deftype bit-counter () '(integer 0 8))
(deftype ub4 () '(unsigned-byte 4))
(deftype ub8 () '(unsigned-byte 8))
(deftype simple-ub8-vector () '(simple-array ub8 (*)))

(defparameter *buffer-size* 4096)

(declaim (optimize (speed 3) (safety 0)))

(defstruct reader
  (ibit 0 :type bit-counter)
  (ibyte 0 :type non-negative-fixnum)
  (end 0 :type non-negative-fixnum)
  (buffer (make-array (list *buffer-size*)
		      :element-type 'ub8)
	  :type simple-ub8-vector)
  stream)

(declaim (inline reset-counters))
(defun reset-counters (reader)
  (declare (type reader reader))

  (with-accessors ((ibit reader-ibit)
		   (ibyte reader-ibyte))
		  reader

		  (setf ibit 0
			ibyte 0)))

(declaim (inline move-forward))
(defun move-forward (reader &optional (bits 1))
  (declare (type reader reader)
	   (type positive-fixnum bits))
  
  (with-accessors ((ibit reader-ibit)
		   (ibyte reader-ibyte))
		  reader

		  (incf ibit bits)
		  
		  (if (= ibit 8)
		      (progn
			(setf ibit 0)
			(incf ibyte)))))

(defun fill-buffer (reader)
  (declare (type reader reader))
  (reset-counters reader)
  (with-accessors ((end reader-end)
		   (stream reader-stream)
		   (buffer reader-buffer))
		  reader

		  (setf end
			(read-sequence buffer stream))

		  (if (= end 0) (error 'flac-eof))))

(declaim (inline can-not-read))
(defun can-not-read (reader)
  (declare (type reader reader))

  (if (< (reader-ibyte reader)
	 (reader-end reader)) nil t))

(declaim (ftype (function (reader) (integer 0 1)) read-bit))
(defun read-bit (reader)
  (declare (type reader reader))
  (if (can-not-read reader) (fill-buffer reader))

  (prog1
      (ldb (byte 1 (- 7 (reader-ibit reader)))
	   (aref (reader-buffer reader)
		 (reader-ibyte reader)))
    (move-forward reader)))

(declaim (ftype (function (positive-fixnum reader) non-negative-fixnum) read-bits))
(defun read-bits (bits reader)
  (declare (type reader reader)
	   (type positive-fixnum bits))
  (with-accessors ((ibit reader-ibit))
		  reader
  
		  (let ((iterations (ceiling (+ bits ibit) 8))
			(result 0))
		    (declare (type positive-fixnum iterations)
			     (type non-negative-fixnum result))
		    (dotimes (i iterations)
		      (if (can-not-read reader) (fill-buffer reader))
		      (let ((bits-to-add (min bits (- 8 ibit))))
			(declare (type bit-counter bits-to-add))
			(setq result
			      (logior result
				      (the non-negative-fixnum
					(ash
					 (ldb (byte bits-to-add (- 8 ibit bits-to-add))
					      (aref (reader-buffer reader)
						    (reader-ibyte reader)))
					 (the (unsigned-byte 32) (- bits bits-to-add)))))
			      bits (- bits bits-to-add))
			(move-forward reader bits-to-add)))
		    result)))

(declaim (ftype (function (positive-fixnum reader) (integer 0)) read-bits-bignum))
(defun read-bits-bignum (bits reader)
  (declare (type reader reader)
	   (type positive-fixnum bits)
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-accessors ((ibit reader-ibit))
		  reader
  
		  (let ((iterations (ceiling (+ bits ibit) 8))
			(result 0))
		    (declare (type positive-fixnum iterations)
			     (type (integer 0) result))
		    (dotimes (i iterations)
		      (if (can-not-read reader) (fill-buffer reader))
		      (let ((bits-to-add (min bits (- 8 ibit))))
			(declare (type bit-counter bits-to-add))
			(setq result
			      (logior result
				      (the non-negative-fixnum
					(ash
					 (ldb (byte bits-to-add (- 8 ibit bits-to-add))
					      (aref (reader-buffer reader)
						    (reader-ibyte reader)))
					 (the (unsigned-byte 32) (- bits bits-to-add)))))
			      bits (- bits bits-to-add))
			(move-forward reader bits-to-add)))
		    result)))

(declaim (ftype (function (reader) ub8) read-octet))
(defun read-octet (reader)
  "Reads current octet from reader
   Ignores ibit"
  (declare (type reader reader))
  (if (can-not-read reader) (fill-buffer reader))
  
  (with-accessors ((ibyte reader-ibyte))
		  reader

		  (prog1
		      (aref (reader-buffer reader) ibyte)
		    (incf ibyte))))

(declaim (ftype (function (simple-ub8-vector reader) non-negative-fixnum) read-octet-vector))
(defun read-octet-vector (array reader)
  ;; Stupid and maybe slow version.
  ;; Why not? I do not use this function often
  (declare (type simple-ub8-vector)
	   (type reader reader))
  
  (dotimes (i (length array))
    (if (can-not-read reader) (fill-buffer reader))
    (with-accessors ((ibyte reader-ibyte))
		    reader
		    
		    (prog1
			(setf (aref array i)
			      (aref (reader-buffer reader) ibyte))
		      (incf ibyte))))
  (length array))

(declaim (ftype (function (reader) non-negative-fixnum) read-to-byte-alignment))
(defun read-to-byte-alignment (reader)
  (declare (type reader reader))
  (with-accessors ((ibyte reader-ibyte)
		   (ibit reader-ibit))
		  reader

		  (if (= ibit 0) (return-from read-to-byte-alignment 0))
		  (prog1
		      (ldb (byte (- 8 ibit) 0)
			   (aref (reader-buffer reader) ibyte))
		    (setf ibit 0)
		    (incf ibyte))))

(defun close-reader (reader)
  (close (reader-stream reader)))

(declaim (ftype (function (reader &optional t) non-negative-fixnum) reader-position))
(defun reader-position (reader &optional val)
  "Returns or sets number of readed octets.
   Similar to file-position
   Sets ibit to zero if val is specified"
  (declare (type reader reader))
  (if val
      (progn
	(reset-counters reader)
	(file-position (reader-stream reader) val))
    (the non-negative-fixnum
      (+ (the non-negative-fixnum ;; Limit to 536 mb on x86 sbcl!
	   (file-position (reader-stream reader)))
	 (- (reader-end reader))
	 (reader-ibyte reader)))))

(declaim (ftype (function (reader (unsigned-byte 8)) (unsigned-byte 8)) peek-octet))
(defun peek-octet (reader octet)
  "Sets input to the first octet found in stream"
  (declare (type (unsigned-byte 8) octet)
	   (type reader reader))
  (setf (reader-ibyte reader)
	(loop for pos = (position octet (reader-buffer reader)
				  :start (reader-ibyte reader))
	      until pos
	      do
	      (fill-buffer reader)
	      (setf (reader-ibyte reader) 0)
	      finally (return pos)))
  octet)
