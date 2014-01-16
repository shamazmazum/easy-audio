;; Copyright (c) 2012, Vasily Postnicov
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

(in-package :easy-audio.flac)

(declaim (optimize
          #+easy-audio-unsafe-code
          (safety 0) (speed 3)))

(defmethod subframe-decode :after ((subframe subframe) frame)
  (declare (ignore frame))
  (let ((wasted-bits (subframe-wasted-bps subframe))
	(out-buf (subframe-out-buf subframe)))
    (declare (type non-negative-fixnum wasted-bits)
	     (type (simple-array (signed-byte 32)) out-buf))
    (if (/= wasted-bits 0)
	(map-into out-buf #'(lambda (sample)
			      (declare (type (signed-byte 32) sample))
			      (the fixnum (ash sample wasted-bits)))
		  out-buf))))

(defmethod subframe-decode ((subframe subframe-constant) frame)
  (declare (ignore frame))
  (let ((out-buf (subframe-out-buf subframe))
	(constant (subframe-constant-value subframe)))
    (declare (type (signed-byte 32) constant)
	     (type (simple-array (signed-byte 32)) out-buf))
    (dotimes (i (length out-buf))
      (setf (aref out-buf i) constant))
    out-buf))

(defmethod subframe-decode ((subframe subframe-verbatim) frame)
  (declare (ignore frame))
  (subframe-out-buf subframe))

(defmethod subframe-decode ((subframe subframe-fixed) frame)
  ;; Decodes subframe destructively modifiying it
  (declare (optimize (speed 3)
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

#|(defmethod subframe-decode ((subframe subframe-lpc) frame)
  (declare (ignore frame)
	   (optimize (speed 3) (safety 0)))
  (let* ((out-buf (the (simple-array (signed-byte 32))
		    (subframe-out-buf subframe)))
	 (len (length out-buf))
	 (shift (subframe-lpc-coeff-shift subframe))
	 (order (subframe-order subframe))
	 (coeff (subframe-lpc-predictor-coeff subframe)))
    (declare (type (simple-array (signed-byte 32)) out-buf coeff)
	     (type fixnum len order)
	     (type (signed-byte 32) shift))

    (do ((i order (1+ i)))
	((= i len))
      (declare (type fixnum i))
      (incf (aref out-buf i)
	    (the fixnum
	      (ash
	       (do ((j 0 (1+ j)) (sum 0))
		   ((= j order) sum)
		 (declare (type fixnum j sum))
		 (incf sum
		       (* (aref coeff j)
			  (aref out-buf (- i j 1)))))
	       (- shift)))))
    out-buf))|#

;; Expanded version from libFLAC
(defmethod subframe-decode ((subframe subframe-lpc) frame)
  (declare (ignore frame)
	   (optimize (speed 3)
		     (safety 0)))
  
  (let* ((out-buf (subframe-out-buf subframe))
	 (len (length out-buf))
	 (shift (subframe-lpc-coeff-shift subframe))
	 (order (subframe-order subframe))
	 (coeff (subframe-lpc-predictor-coeff subframe)))
    (declare (type (simple-array (signed-byte 32)) out-buf coeff)
	     (type fixnum len order)
	     (type (signed-byte 32) shift))
    
    (macrolet ((calc-out-buf (n)
			     (let ((idx (gensym))
				   (sum (gensym)))
			       `(do ((,idx ,n (1+ ,idx)))
				    ((= ,idx len))
				  (declare (type fixnum ,idx))
				  (let ((,sum 0))
				    (declare (type fixnum ,sum))
				    ,@(loop for j below n collect
					    `(incf ,sum
						   (* (aref coeff ,j)
						      (aref out-buf (- ,idx ,(1+ j))))))
				    (incf (aref out-buf ,idx)
					  (the fixnum
					    (ash ,sum (- shift)))))))))

      (cond
       ((<= order 12)
	(cond
	 ((> order 8)
	  (cond
	   ((> order 10)
	    (cond
	     ((= order 12)
	      (calc-out-buf 12))
	     (t (calc-out-buf 11))))
	   
	   (t
	    (cond
	     ((= order 10) (calc-out-buf 10))
	     (t (calc-out-buf 9))))))
	 
	 (t
	  (cond
	   ((> order 4)
	    (cond
	     ((> order 6)
	      (cond
	       ((= order 8) (calc-out-buf 8))
	       (t (calc-out-buf 7))))
	     (t
	      (cond
	       ((= order 6) (calc-out-buf 6))
	       (t (calc-out-buf 5))))))
	   (t
	    (cond
	     ((> order 2)
	      (cond
	       ((= order 4) (calc-out-buf 4))
	       (t (calc-out-buf 3))))
	     
	     (t
	      (cond
	       ((= order 2) (calc-out-buf 2))
	       (t (calc-out-buf 1))))))))))
       (t
	(do ((i order (1+ i)))
	    ((= i len))
	  (declare (type fixnum i))
	  (incf (aref out-buf i)
		(the fixnum
		  (ash
		   (do ((j 0 (1+ j)) (sum 0))
		       ((= j order) sum)
		     (declare (type fixnum j sum))
		     (incf sum
			   (* (aref coeff j)
			      (aref out-buf (- i j 1)))))
		   (- shift))))))))
  out-buf))  

(defun frame-decode (frame)
  "Decode a frame destructively modifying (and garbaging) all subframes within.
Returns list of decoded audio buffers (one buffer for each channel)."
  (declare (optimize (speed 3)
		     (safety 0)))

  (let ((decoded-subframes
	 (mapcar #'(lambda (subframe) (subframe-decode subframe frame))
		 (frame-subframes frame)))
  	(assignment (frame-channel-assignment frame)))
    (declare (type non-negative-fixnum assignment))

    (if (<= assignment +max-channels+) (return-from frame-decode decoded-subframes))
    (if (/= 2 (length decoded-subframes)) (error "Bad channel assignment/number of subframes"))

    (destructuring-bind (left right) decoded-subframes
      (declare (type (simple-array (signed-byte 32)) left right))
      (cond
       ((= +left-side+ assignment)
	;; Maybe just a loop?
	(map-into right #'-
		  left right))
       
       ((= +right-side+ assignment)
	(map-into left #'+
		  left right))
       
       ((= +mid-side+ assignment)
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
