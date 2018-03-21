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
	     (type (sa-sb 32) out-buf))
    (if (/= wasted-bits 0)
	(map-into out-buf (lambda (sample)
                            (declare (type (sb 32) sample))
                            (the fixnum (ash sample wasted-bits)))
		  out-buf))))

(defmethod subframe-decode ((subframe subframe-constant) frame)
  (declare (ignore frame))
  (let ((out-buf (subframe-out-buf subframe))
	(constant (subframe-constant-value subframe)))
    (declare (type (sb 32) constant)
	     (type (sa-sb 32) out-buf))
    (dotimes (i (length out-buf))
      (setf (aref out-buf i) constant))
    out-buf))

(defmethod subframe-decode ((subframe subframe-verbatim) frame)
  (declare (ignore frame))
  (subframe-out-buf subframe))

(defmethod subframe-decode ((subframe subframe-fixed) frame)
  ;; Decodes subframe destructively modifiying it
  (declare (ignore frame))
  (let* ((out-buf (subframe-out-buf subframe))
	 (order (subframe-order subframe))
	 (len (length out-buf)))
    (declare (type (sa-sb 32) out-buf)
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

;; This version is much like libFLAC expanded one.
;; The only difference is that we generate predictors of mostly useful orders first
;; and get them from hash table.

(defparameter *lpc-predictors* (make-hash-table)
  "Precalculated FIR linear predictors")

(defmacro gen-lpc-predictor (n)
  "Generate FIR linear predictor of order N"
  (let ((func-name (intern (format nil "LPC-PREDICTOR-~D" n))))
    `(flet ((,func-name (out-buf coeff shift)
              (declare (optimize
                        #+easy-audio-unsafe-code
                        (safety 0) (speed 3) (space 2))
                       (type (sa-sb 32) out-buf coeff)
                       (type (sb 32) shift))
                
              (loop for i fixnum from ,n below (length out-buf)
                 for sum fixnum = 0 do
                   ,@(loop for j fixnum below n collect
                          `(incf sum
                                 (* (aref coeff ,j)
                                    (aref out-buf (- i ,(1+ j))))))
                   (incf (aref out-buf i)
                         (the fixnum
                              (ash sum (- shift)))))
              
              out-buf))
       #',func-name)))

;; Populate hash table with predictors of orders from 1 to 12
;; (These are most useful).
(macrolet ((gen-lpc-predictors (n)
             `(progn
                ,@(loop for order fixnum from 1 to n collect
                        `(setf (gethash ,order *lpc-predictors*)
                               (gen-lpc-predictor ,order))))))

  (gen-lpc-predictors 12))

(defmethod subframe-decode ((subframe subframe-lpc) frame)
  (declare (ignore frame))
  (let* ((order (subframe-order subframe))
         (predictor (gethash order *lpc-predictors*)))
    
    (if (not predictor)
        ;; Funny stuff. If there is no desired predictor in hash table,
        ;; generate it on the fly.
        (setf predictor (eval (list 'gen-lpc-predictor order))
              (gethash order *lpc-predictors*) predictor))

    (funcall (the function predictor)
             (subframe-out-buf subframe)
             (subframe-lpc-predictor-coeff subframe)
             (subframe-lpc-coeff-shift subframe))))

(defun frame-decode (frame)
  "Decode a frame destructively modifying (and garbaging) all subframes within.
Returns list of decoded audio buffers (one buffer for each channel)."
  (let ((decoded-subframes
	 (mapcar #'(lambda (subframe) (subframe-decode subframe frame))
		 (frame-subframes frame)))
  	(assignment (frame-channel-assignment frame)))
    (declare (type non-negative-fixnum assignment))

    (if (<= assignment +max-channels+) (return-from frame-decode decoded-subframes))
    (if (/= 2 (length decoded-subframes))
        (error 'flac-error :message "Bad channel assignment/number of subframes"))

    (destructuring-bind (left right) decoded-subframes
      (declare (type (sa-sb 32) left right))
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
       (t (error 'flac-error :message "Wrong channel assignment"))))
    decoded-subframes))
