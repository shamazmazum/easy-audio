(in-package :cl-flac)

(defmethod subframe-decode ((subframe subframe-constant) frame)
  (make-array (frame-block-size frame)
	      ;; FIXME: sample is signed in original libFLAC
	      :element-type '(signed-byte 32)
	      :initial-element (subframe-constant-value subframe)))

(defmethod subframe-decode ((subframe subframe-verbatim) frame)
  (declare (ignore frame))
  (subframe-verbatim-buffer subframe))

(defmethod subframe-decode ((subframe subframe-fixed) frame)
  ;; Decodes subframe destructively modifiying it
  (declare (optimize (speed 3)
		     (space 0)
		     (safety 0))
	   (ignore frame))
  (let* ((out-buf (subframe-out-buf subframe))
	 (order (subframe-order subframe))
	 (len (length out-buf)))
    (declare (type (simple-array (signed-byte 32)) out-buf)
	     (type fixnum order len))
    (cond
     ;; 0 - out-buf contains decoded data
     ((= order 1)
      (loop for i from 1 below len do
	    (incf (aref out-buf i)
		  (aref out-buf (1- i)))))
     ((= order 2)
      (loop for i from 2 below len do
	    (incf (aref out-buf i)
		  (- (ash (aref out-buf (1- i)) 1)
		     (aref out-buf (- i 2))))))
     
     ((= order 3)
      (loop for i from 3 below len do
	    (incf (aref out-buf i)
		  (+ (ash (- (aref out-buf (1- i))
			     (aref out-buf (- i 2))) 1)
		     
		     (- (aref out-buf (1- i))
			     (aref out-buf (- i 2)))
		     
		     (aref out-buf (- i 3))))))
			     
     ((= order 4)
      (loop for i from 4 below len do
	    (incf (aref out-buf i)
		  (- (ash (+ (aref out-buf (1- i))
			     (aref out-buf (- i 3))) 2)

		     (+ (ash (aref out-buf (- i 2)) 2)
			(ash (aref out-buf (- i 2)) 1))

		     (aref out-buf (- i 4)))))))
    out-buf))

(defmethod subframe-decode ((subframe subframe-lpc) frame)
  (declare (ignore frame))
  (let* ((out-buf (subframe-out-buf subframe))
	 (len (length out-buf))
	 (shift (subframe-lpc-coeff-shift subframe))
	 (order (subframe-order subframe))
	 (coeff (subframe-lpc-predictor-coeff subframe)))

    (loop for i from order below len do
	  (incf (aref out-buf i)
		(ash
		 (loop for j below order sum
		       (* (aref coeff j)
			  (aref out-buf (- i j 1))))
		 (- shift))))
    out-buf))

(defun frame-decode (frame)
  (declare (optimize (speed 3)
		     (safety 0)))

  (let ((decoded-subframes
	 (mapcar #'(lambda (subframe) (subframe-decode subframe frame))
		 (frame-subframes frame)))
  	(assignment (frame-channel-assignment frame)))

    (if (typep assignment 'integer) (return-from frame-decode decoded-subframes))
    (if (/= 2 (length decoded-subframes)) (error "Bad channel assignment/number of subframes"))

    (destructuring-bind (left right) decoded-subframes
      (declare (type (simple-array (signed-byte 32)) left right))
      (cond
       ((eq :left/side assignment)
	;; Maybe just a loop?
	(map-into right #'-
		  left right))
       
       ((eq :right/side assignment)
	(map-into left #'+
		  left right))
       
       ((eq :mid/side assignment)
	(let ((block-size (frame-block-size frame)))
	  (declare (type fixnum block-size))
	  (dotimes (i block-size)
	    (let* ((side (aref right i))
		   (mid (logior
			 (ash (aref left i) 1)
			 (logand side 1))))
	      
	      (setf (aref left i)
		    (ash (+ mid side) -1)
		    (aref right i)
		    (ash (- mid side) -1))))))
       (t (error "Wrong channel assignment"))))
    decoded-subframes))
