;; Copyright (c) 2012-2013, Vasily Postnicov
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Thanks to Love5an, author of trivial-bit-streams
;; Do the same thing trivial-bit-streams does but without CLOS
;; High performance is required here

(in-package :easy-music.bitreader)

(declaim (optimize (speed 3) (safety 0)))

(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))
(deftype bit-counter () '(integer 0 8))
(deftype ub8 () '(unsigned-byte 8))
(deftype simple-ub8-vector () '(simple-array ub8 (*)))

(defparameter *buffer-size* 4096)

(define-condition bitreader-eof ()
  ((bitreader :initarg :bitreader
              :reader bitreader-eof-bitreader)))

;; Might be restrictions for 32 bit systems
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
  "Resets ibit and ibyte in reader struct"
  (setf (reader-ibit reader) 0
        (reader-ibyte reader) 0))

(declaim (inline move-forward))
(defun move-forward (reader &optional (bits 1))
  "Moves position in READER bit reader in range [1; 8-ibit] BITS.
   Maximum value of ibit is 7."
  (declare (type positive-fixnum bits))
  
  (with-accessors
   ((ibit reader-ibit)
    (ibyte reader-ibyte)) reader

    (incf ibit bits)

    (cond
     ((= ibit 8)
      (setf ibit 0)
      (incf ibyte)))))

;; If stream is 300 Mb, there are (ceiling (* 300 10^6) 4096) =
;; 73243 calls to fill-buffer. Not many, but inline it anyway
(declaim (inline fill-buffer))
(defun fill-buffer (reader)
  "Fills internal buffer of READER"
  (reset-counters reader)
  (setf (reader-end reader)
        (read-sequence (reader-buffer reader)
                       (reader-stream reader)))
  
  (if (= (reader-end reader) 0) (error 'bitreader-eof :bitreader reader)))

(declaim (inline can-not-read))
(defun can-not-read (reader)
  "Checks if READER can be read without calling fill-buffer"
  (declare (type reader reader))

  (= (reader-ibyte reader)
     (reader-end reader)))


(declaim (ftype (function (reader) (integer 0 1)) read-bit))
(defun read-bit (reader)
  "Read a single bit from READER"
  (declare (type reader reader))
  (if (can-not-read reader) (fill-buffer reader))

  (prog1
      (ldb (byte 1 (- 7 (reader-ibit reader)))
	   (aref (reader-buffer reader)
		 (reader-ibyte reader)))
    (move-forward reader)))

(defmacro read-bits-loop ((reader bits return-type bits-to-add ibit
				 inc-form) &body body)
  (let ((result (gensym))
	(iter (gensym))
	(tot-iter (gensym)))
    `(with-accessors ((,ibit reader-ibit))
		     reader
		     
		     (let* ((,result 0)
			    (,tot-iter (ceiling (+ ,bits ,ibit))))
		       (declare (type ,return-type ,result)
				(type positive-fixnum ,tot-iter))
		       (dotimes (,iter ,tot-iter)
			 (if (can-not-read ,reader) (fill-buffer ,reader))
			 (let ((,bits-to-add (min ,bits (- 8 ,ibit))))
			   (declare (type bit-counter ,bits-to-add))
			   (setq ,result (logior ,result ,inc-form)
				 ,bits (- ,bits ,bits-to-add))
			   (move-forward ,reader ,bits-to-add)
			   ,@body))
		       ,result))))

(declaim (ftype (function (reader) ub8) read-octet))
(defun read-octet (reader)
  "Reads current octet from reader
   Ignores ibit"
  (if (can-not-read reader) (fill-buffer reader))
  
  (prog1
      (aref (reader-buffer reader) (reader-ibyte reader))
    (incf (reader-ibyte reader))))

(declaim (ftype (function (simple-ub8-vector reader) non-negative-fixnum) read-octet-vector))
(defun read-octet-vector (array reader)
  ;; Stupid and maybe slow version.
  ;; Why not? I do not use this function often
  (dotimes (i (length array))
    (if (can-not-read reader) (fill-buffer reader))
    (setf (aref array i)
          (aref (reader-buffer reader) (reader-ibyte reader)))
    (incf (reader-ibyte reader)))
  (length array))

(declaim (ftype (function (reader) non-negative-fixnum) read-to-byte-alignment))
(defun read-to-byte-alignment (reader)
  "Reads from READER to byte alignment.
   If already READER is already byte-aligned,
   returns 0."
  (with-accessors
   ((ibyte reader-ibyte)
    (ibit reader-ibit)) reader

    (if (= ibit 0) (return-from read-to-byte-alignment 0))
    (prog1
        (ldb (byte (- 8 ibit) 0)
             (aref (reader-buffer reader) ibyte))
      (setf ibit 0)
      (incf ibyte))))

(declaim (ftype (function (reader &optional t) non-negative-fixnum) reader-position))
(defun reader-position (reader &optional val)
  "Returns or sets number of readed octets.
   Similar to file-position
   Sets ibit to zero if val is specified"
  (cond
   (val
    (file-position (reader-stream reader) val)
    (fill-buffer reader))
   (t
    (the non-negative-fixnum
      (+ (the non-negative-fixnum ;; Limit to 536 mb on x86 sbcl!
	   (file-position (reader-stream reader)))
	 (- (reader-end reader))
	 (reader-ibyte reader))))))

(declaim (ftype (function (reader ub8) ub8) peek-octet))
(defun peek-octet (reader octet)
  "Sets input to the first octet found in stream"
  (declare (type ub8 octet))
  (setf (reader-ibyte reader)
	(loop for pos = (position octet (reader-buffer reader)
				  :start (reader-ibyte reader))
	      until pos
	      do
	      (fill-buffer reader)
	      (setf (reader-ibyte reader) 0)
	      finally (return pos)))
  octet)

(declaim (ftype (function (reader) non-negative-fixnum) reader-length))
(defun reader-length (reader)
  "Returns length of stream in octets or zero
   if the value is unknown"
  (declare (type reader reader))
  (let ((len (file-length (reader-stream reader))))
    (if len len 0)))

(in-package :easy-music.bitreader.be-fixnum)

(declaim (ftype (function (positive-fixnum reader) non-negative-fixnum) read-bits))
;; Must be a bit faster in this way, than with use of read-bits-loop
(defun read-bits (bits reader)
  (declare (type reader reader)
	   (type positive-fixnum bits))
  (with-accessors ((ibit reader-ibit)
		   (ibyte reader-ibyte))
		  reader

		  (if (can-not-read reader) (fill-buffer reader))
		  (cond
		   ((<= bits (- 8 ibit))
		    (prog1
			(ldb (byte bits (- 8 ibit bits))
			     (aref (reader-buffer reader)
				   (reader-ibyte reader)))
		      (move-forward reader bits)))

		   (t
		    (let* ((size (- 8 ibit))
			   (offset (- bits size))
			   (result (ash (ldb (byte size 0)
					     (aref (reader-buffer reader)
						   ibyte))
					offset)))
		      
		      (declare (type bit-counter size)
			       (type non-negative-fixnum result offset))
		      
		      (setf ibit 0)
		      (incf ibyte)

		      (do ()
			  ((< offset 8))
			(if (can-not-read reader) (fill-buffer reader))
			(decf offset 8)
			(setq result (logior result
					     (the non-negative-fixnum
					       (ash (aref (reader-buffer reader) ibyte)
						    offset))))
			(incf ibyte))

		      (if (/= offset 0)
			  (progn
			    (if (can-not-read reader) (fill-buffer reader))
			    (setq result
				  (logior result (the non-negative-fixnum
						   (ldb (byte (the bit-counter offset)
							      (- 8 offset))
						      (aref (reader-buffer reader) ibyte)))))
			    (incf ibit offset)))
		      result)))))

(in-package :easy-music.bitreader.be-bignum)

(declaim (ftype (function (positive-fixnum reader) (integer 0)) read-bits))
(defun read-bits (bits reader)
  (declare (type reader reader)
	   (type positive-fixnum bits)
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

  (read-bits-loop (reader bits (integer 0)
			  bits-to-add ibit
			  (ash
			   (ldb (byte bits-to-add (- 8 ibit bits-to-add))
				(aref (reader-buffer reader)
				      (reader-ibyte reader)))
			   (the (unsigned-byte 32) (- bits bits-to-add))))))

(in-package :easy-music.bitreader.le-fixnum)

(declaim (ftype (function (positive-fixnum reader) non-negative-fixnum) read-bits))
(defun read-bits (bits reader)
  (declare (type reader reader)
	   (type positive-fixnum bits))

  (let ((added-bits 0))
    (declare (type non-negative-fixnum added-bits))
    (read-bits-loop (reader bits non-negative-fixnum
			    bits-to-add ibit
			    (the non-negative-fixnum
			      (ash
			       (ldb (byte bits-to-add (- 8 ibit bits-to-add))
				    (aref (reader-buffer reader)
					  (reader-ibyte reader)))
			       (the (unsigned-byte 32) added-bits))))
		    (incf added-bits bits-to-add))))

(in-package :easy-music.bitreader.le-bignum)

(declaim (ftype (function (positive-fixnum reader) (integer 0)) read-bits))
(defun read-bits (bits reader)
  (declare (type reader reader)
	   (type positive-fixnum bits)
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

  (let ((added-bits 0))
    (declare (type non-negative-fixnum added-bits))
    (read-bits-loop (reader bits (integer 0)
			    bits-to-add ibit
			    (ash
			     (ldb (byte bits-to-add (- 8 ibit bits-to-add))
				  (aref (reader-buffer reader)
					(reader-ibyte reader)))
			     (the (unsigned-byte 32) added-bits)))
		    (incf added-bits bits-to-add))))
