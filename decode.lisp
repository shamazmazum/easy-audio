(in-package :cl-flac)

(defmethod subframe-decode ((subframe subframe-constant) frame)
  (make-array (frame-block-size frame)
	      ;; FIXME: sample is signed in original libFLAC
	      :element-type (list 'signed-byte (frame-sample-size frame))
	      :initial-element (subframe-constant-value subframe)))

(defmethod subframe-decode ((subframe subframe-verbatim) frame)
  (declare (ignore frame))
  (subframe-verbatim-buffer subframe))

(defmethod subframe-decode ((subframe subframe-fixed) frame)
  ;; Decodes subframe destructively modifiying it
  (declare (ignore frame))
  (with-slots (order out-buf) subframe
	      (let ((order (subframe-order subframe))
		    (len (length out-buf)))
		(cond
		 ;; 0 - out-buf contains decoded data
		 ((= order 1)
		  (loop for i from 1 below len do
			(setf (aref out-buf i)
			      (+ (aref out-buf i)
				 (aref out-buf (1- i))))))
		 ((= order 2)
		  (loop for i from 2 below len do
			(setf (aref out-buf i)
			      (- (+ (aref out-buf i)
				    (ash (aref out-buf (1- i)) 1))
				 (aref out-buf (- i 2))))))
		 
		 ((= order 3)
		  (loop for i from 3 below len do
			(setf (aref out-buf i)
			      (- (+ (aref out-buf i)
				    (* 3 (aref out-buf (1- i)))
				    (aref out-buf (- i 3)))
				 (* 3 (aref out-buf (- i 2)))))))
		 
		 ((= order 4)
		  (loop for i from 4 below len do
			(setf (aref out-buf i)
			      (- (+ (aref out-buf i)
				    (ash (aref out-buf (1- i)) 2)
				    (ash (aref out-buf (- i 3)) 2))
				 
				 (* 6 (aref out-buf (- i 2)))
				 (aref out-buf (- i 4))))))))
	      out-buf))
