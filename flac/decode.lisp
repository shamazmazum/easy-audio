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

(sera:-> decode-subframe-postprocess (subframe-header (sa-sb 32))
         (values (sa-sb 32) &optional))
(defun decode-subframe-postprocess (header output)
  (declare (optimize (speed 3)))
  (let ((wasted-bits (subframe-header-wasted-bps header)))
    (if (not (zerop wasted-bits))
	(map-into output
                  (lambda (sample)
                    (ash sample wasted-bits))
		  output)
        output)))

(sera:-> decode-subframe-constant (subframe-constant)
         (values (sa-sb 32) &optional))
(defun decode-subframe-constant (subframe)
  (declare (optimize (speed 3)))
  (let* ((header (subframe-constant-header subframe))
         (out-buf (subframe-header-out-buf header))
	 (constant (subframe-constant-value subframe)))
    (decode-subframe-postprocess header (fill out-buf constant))))

(sera:-> decode-subframe-verbatim (subframe-verbatim)
         (values (sa-sb 32) &optional))
(defun decode-subframe-verbatim (subframe)
  (declare (optimize (speed 3)))
  (let ((header (subframe-verbatim-header subframe)))
    (decode-subframe-postprocess header (subframe-header-out-buf header))))

(sera:-> decode-subframe-fixed (subframe-fixed)
         (values (sa-sb 32) &optional))
(defun decode-subframe-fixed (subframe)
  ;; Decodes subframe destructively modifiying it
  (declare (optimize (speed 3)))
  (let* ((header (subframe-fixed-header subframe))
         (out-buf (subframe-header-out-buf header))
	 (order (subframe-fixed-order subframe))
	 (len (length out-buf)))
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
    (decode-subframe-postprocess header out-buf)))

(sera:-> decode-subframe-lpc (subframe-lpc)
         (values (sa-sb 32) &optional))
(defun decode-subframe-lpc (subframe)
  (declare (optimize (speed 3)))
  (let* ((header (subframe-lpc-header subframe))
         (out-buf (subframe-header-out-buf header))
	 (len (length out-buf))
	 (shift (subframe-lpc-coeff-shift subframe))
	 (order (subframe-lpc-order subframe))
	 (coeff (subframe-lpc-predictor-coeff subframe)))
    (do ((i order (1+ i)))
	((= i len))
      (incf (aref out-buf i)
	    (the fixnum
	      (ash
	       (do ((j 0 (1+ j)) (sum 0))
		   ((= j order) sum)
		 (declare (type fixnum sum))
		 (incf sum
		       (* (aref coeff j)
			  (aref out-buf (- i j 1)))))
	       (- shift)))))
    (decode-subframe-postprocess header out-buf)))

(sera:-> decode-subframe (subframe)
         (values (sa-sb 32) &optional))
(declaim (inline decode-subframe))
(defun decode-subframe (subframe)
  (etypecase subframe
    (subframe-verbatim (decode-subframe-verbatim subframe))
    (subframe-constant (decode-subframe-constant subframe))
    (subframe-fixed    (decode-subframe-fixed    subframe))
    (subframe-lpc      (decode-subframe-lpc      subframe))))

(defun frame-decode (frame)
  "Decode a frame destructively modifying (and garbaging) all subframes within.
Returns list of decoded audio buffers (one buffer for each channel)."
  (declare (optimize (speed 3)))
  (let ((decoded-subframes
	 (mapcar #'decode-subframe (frame-subframes frame)))
  	(assignment (frame-channel-assignment frame)))
    (declare (type non-negative-fixnum assignment))

    (when (<= assignment +max-channels+)
      (return-from frame-decode decoded-subframes))
    (when (/= 2 (length decoded-subframes))
      (error 'flac-error
             :format-control "Bad channel assignment/number of subframes"))

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
		    (ash (- mid side) -1))))))))
    decoded-subframes))
