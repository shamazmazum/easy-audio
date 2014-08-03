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

(declaim (optimize (speed 3)))

(easy-audio-early:defvar-unbound *out-buffer*
    "Output buffer for exactly one subframe")

(defun get-blocking-strategy (val)
  (declare (type fixnum val))
  (cond
    ((= val 0) :fixed)
    ((= val 1) :variable)
    (t (error 'flac-bad-frame
              :message "Blocking strategy must be 0 or 1"))))

(defun get-block-size (val)
  (declare (type positive-fixnum val))
  (cond
    ((= val 1) 192)               ; 0001
    ((<= val 5)                   ; 0010-0101
     (ash 576 (- val 2)))
    ((= val 6) :get-8-from-end)   ; 0110
    ((= val 7) :get-16-from-end)  ; 0111
    ((<= val 15)            ; 1000-1111
     (ash 1 val))
    (t (error 'flac-bad-frame
              :message "Frame block size is invalid"))))

(defun get-sample-rate (streaminfo val)
  (declare (type non-negative-fixnum val))
  (cond
    ((and (= val 0)
          streaminfo) (streaminfo-samplerate streaminfo))
    ((< val 15) (nth (1- val) +coded-sample-rates+))
    (t
     (error 'flac-bad-frame
            :message "Frame sample rate is invalid"))))

(defun get-channel-assignment (val)
  (declare (type non-negative-fixnum val))
  (cond ((<= val 10) (1+ val))
        (t (error 'flac-bad-frame
                  :message "Invalid channel assignment"))))

(defun get-sample-size (streaminfo val)
  (declare (type non-negative-fixnum val))
  (if (and (= val 0) streaminfo)
      (streaminfo-bitspersample streaminfo)
      (or (cdr (assoc val +coded-sample-sizes+))
          (error 'flac-bad-frame
                 :message "Invalid sample size"))))

;; Residual reader
(defun residual-reader (bit-reader subframe frame out)
  (let ((coding-method (read-bits 2 bit-reader)))
    (declare (type (ub 2) coding-method))
    (cond
     ((= coding-method 0) ; 00
      (residual-body-reader bit-reader subframe frame out
			    :param-len 4
			    :esc-code #b1111))
     ((= coding-method 1) ; 01
      (residual-body-reader bit-reader subframe frame out
			    :param-len 5
			    :esc-code #b11111))
     (t (error 'flac-bad-frame
	       :message "Invalid residual coding method")))))

(defun residual-body-reader (bit-reader subframe frame out &key param-len esc-code)
  (declare (type fixnum param-len esc-code)
	   (type (sa-sb 32) out))

  (let* ((part-order (the (ub 4)
		       (read-bits 4 bit-reader)))
	 (sample-idx (subframe-order subframe))
	 (blocksize (frame-block-size frame))
	 (predictor-order (subframe-order subframe))
	 (partition-samples (ash blocksize (- part-order))))
    (declare (type fixnum sample-idx
		   blocksize predictor-order
		   partition-samples))
    
    (loop for i below (ash 1 part-order) do
	  (let ((samples-num
		 (cond
		  ;; FIXME:: Check following lines
		  ((zerop i) (- partition-samples predictor-order))
		  (t partition-samples)))
		(rice-parameter (read-bits param-len bit-reader)))
	    (declare (type non-negative-fixnum rice-parameter samples-num))
	    
	    (cond
	     ((/= rice-parameter esc-code)
	      (loop repeat samples-num do
		    (setf (aref out sample-idx)
			  (read-rice-signed bit-reader rice-parameter))
		    (incf sample-idx)))
	     (t
	      ;; FIXME: read unencoded signed rice
	      ;; Do we need to store bps?
	      ;; Read bps:
	      (setq rice-parameter (read-bits 5 bit-reader))
	      (read-bits-array bit-reader out rice-parameter
			       :signed t
			       :offset sample-idx)
	      (incf sample-idx samples-num)))))
    out))

;; Subframe reader

(defmethod subframe-body-reader (bit-reader (subframe subframe-lpc) frame)
  (let* ((bps (subframe-actual-bps subframe))
	 (warm-up-samples (subframe-order subframe))
	 (out-buf (subframe-out-buf subframe))
	 (coeff-buf (make-array (list warm-up-samples)
				:element-type '(sb 32))))
    
    (read-bits-array bit-reader
		     out-buf bps :signed t :len warm-up-samples)
    
    (let ((precision (the fixnum
		       (1+ (read-bits 4 bit-reader)))))
      (if (= #b10000 precision)
	  (error 'flac-bad-frame
		 :message "lpc coefficients precision cannot be 16")
	(setf (subframe-lpc-precision subframe) precision))
      
      (setf (subframe-lpc-coeff-shift subframe)
	    (unsigned-to-signed (read-bits 5 bit-reader) 5))

      (setf (subframe-lpc-predictor-coeff subframe)
	    (read-bits-array bit-reader
			     coeff-buf precision :signed t)))

    (residual-reader bit-reader subframe frame out-buf)))

(defmethod subframe-body-reader (bit-reader (subframe subframe-fixed) frame)
  (let ((bps (subframe-actual-bps subframe))
	(warm-up-samples (subframe-order subframe))
	(out-buf (subframe-out-buf subframe)))
    
    (read-bits-array bit-reader out-buf bps :signed t :len warm-up-samples)

    (residual-reader bit-reader subframe frame out-buf)))

(defmethod subframe-body-reader (bit-reader (subframe subframe-constant) frame)
  (declare (ignore frame))
  (with-slots (actual-bps) subframe
	      (setf (subframe-constant-value subframe) ;; FIXME: value is signed in original libFLAC
		    (unsigned-to-signed
		     (read-bits actual-bps bit-reader)
		     actual-bps))))

(defmethod subframe-body-reader (bit-reader (subframe subframe-verbatim) frame)
  (declare (ignore frame))
  (let ((bps (subframe-actual-bps subframe)))

    (with-slots (out-buf) subframe
		(setf out-buf
		      (read-bits-array bit-reader out-buf bps :signed t)))))

(defun subframe-reader (stream frame actual-bps)
  (declare (type (integer 4 33) actual-bps))
  (if (/= (read-bit stream) 0) (error 'flac-bad-frame
				      :message "Error reading subframe"))
    (let* ((type-num (read-bits 6 stream))
	   (subframe
	    (cond
	     ((= type-num 0) (make-instance 'subframe-constant))         ; 000000
	     ((= type-num 1) (make-instance 'subframe-verbatim))         ; 000001
	     ((and
	       (>= type-num 8)
	       (<= type-num 12))
	      (make-instance 'subframe-fixed :order (logand type-num #b111)))     ; 001000-001100
	     ((and
	       (>= type-num 32)
	       (<= type-num 63))
	      (make-instance 'subframe-lpc :order (1+ (logand type-num #b11111)))) ; 100000-111111
	     (t (error 'flac-bad-frame
		       :message "Error subframe type"))))
           (wasted-bits
            (let ((lead-in-bit (read-bit stream)))
              (if (= lead-in-bit 1)
                  (1+ (read-unary-coded-integer stream))
                0)))
           (blocksize (frame-block-size frame)))
      (declare (type non-negative-fixnum wasted-bits))

      (setf (subframe-wasted-bps subframe) wasted-bits            
            (subframe-actual-bps subframe)
            (- actual-bps wasted-bits)

            (subframe-out-buf subframe)
            (if (and *out-buffer*
                     (= (length (the (sa-sb 32)
                                     *out-buffer*))
                        blocksize))
                *out-buffer*
                (make-array
                 (list blocksize)
                 :element-type '(sb 32))))

      (subframe-body-reader stream subframe frame)
      subframe))

(defmethod frame-reader :around (stream &optional streaminfo out-buffers)
  (restart-case
      (call-next-method)
      (skip-malformed-frame ()
        :report "Skip this frame and read the next one"
        (restore-sync stream streaminfo)
        (frame-reader stream streaminfo out-buffers))
      (stop-reading-frame ()
        :report "Do nothing and return dummy frame"
        (make-instance 'frame))))

(defmethod frame-reader (stream &optional streaminfo out-buffers)
  (let ((frame (make-instance 'frame)))
    #+easy-audio-check-crc
    (init-crc stream)
    (if (/= +frame-sync-code+ (read-bits 14 stream)) (error 'flac-bad-frame
							    :message "Frame sync code is not 11111111111110"))
    (if (/= 0 (read-bit stream)) (error 'flac-bad-frame
					:message "Error reading frame"))

    (with-slots (blocking-strategy
                 block-size
                 sample-rate
                 channel-assignment
                 sample-size
                 number) frame
      
      (setf blocking-strategy (get-blocking-strategy (read-bit stream))
            block-size (get-block-size (read-bits 4 stream))
            sample-rate (get-sample-rate streaminfo (read-bits 4 stream))
            channel-assignment (get-channel-assignment (read-bits 4 stream))
            sample-size (get-sample-size streaminfo (read-bits 3 stream)))
      
      (if (/= 0 (read-bit stream)) (error 'flac-bad-frame
                                          :message "Error reading frame"))

      (setf number
            (if (eq blocking-strategy :fixed)
                (read-utf8-u32 stream)
                (error 'flac-bad-frame
                       :message "Variable block size not implemented yet"))
    
            block-size
            (the fixnum
                 (cond
                   ((eq block-size :get-8-from-end)
                    (1+ (read-octet stream)))
                   ((eq block-size :get-16-from-end)
                    (1+ (read-bits 16 stream)))
                   (t block-size)))

            sample-rate
            (the fixnum
                 (cond
                   ((eq sample-rate :get-8-bit-from-end-khz)
                    (* 1000 (read-octet stream)))
                   ((eq sample-rate :get-16-bit-from-end-hz)
                    (read-bits 16 stream))
                   ((eq sample-rate :get-16-bit-from-end-tenshz)
                    (* 10 (read-bits 16 stream)))
                   (t sample-rate)))))
    
    (setf (frame-crc-8 frame) (read-octet stream))

    (let ((assignment (frame-channel-assignment frame))
          (sample-size (frame-sample-size frame)))
      (declare (type non-negative-fixnum assignment)
               (type (integer 4 32) sample-size))
      (setf (frame-subframes frame)
            (loop for sf fixnum below (if (<= assignment +max-channels+) assignment 2)
                  collect (let ((*out-buffer* (nth sf out-buffers)))
                            (subframe-reader
                             stream frame
                             ;; Do bps correction
                             (cond
                               ((or
                                 (and (= assignment +left-side+)
                                      (= sf 1))
                                 (and (= assignment +right-side+)
                                      (= sf 0))
                                 (and (= assignment +mid-side+)
                                      (= sf 1)))
                                (1+ sample-size))
                               (t sample-size)))))))
    
    ;; Check zero padding
    (if (/= (read-to-byte-alignment stream) 0) (error 'flac-bad-frame
						      :message "Padding to byte-alignment is not zero"))
    #+easy-audio-check-crc
    (let ((crc (get-crc stream)))
      (declare (type (ub 16) crc))
      (setf (frame-crc-16 frame) crc)
      (if (/= crc (read-bits 16 stream))
          (error 'flac-bad-frame
                 :message "Frame CRC mismatch")))
    #-easy-audio-check-crc
    (setf (frame-crc-16 frame) (read-bits 16 stream))
  frame))

;; Rather slow (and buggy) absolute sample seek
(defun seek-sample (bitreader streaminfo sample start
			      &optional seektable)
  "Seeks to interchannel sample.
   Sets input to new frame, which contains this sample
   Returns position of this sample in the frame"
  (declare (type reader bitreader)
	   (type streaminfo streaminfo)
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)) ; Since we have bignum arithmetic here

  ;; Init the bounds where desired sample must be
  (let ((start-pos start)
	(totalsamples (streaminfo-totalsamples streaminfo))
	(end-pos (reader-length bitreader)))

    (if (and (> sample totalsamples)
	     (/= totalsamples 0))
	(error 'flac-error
	       :message "Seek error. Desired sample number is bigger than
                         number of samples in stream"))
    
    ;; Now, if seektable is present, correct the bounds
    (if seektable
	(flet ((find-bounding-seekpoints (points)
	    "Returns bounding seekpoints, samplenum of one is less or equal
             than desired sample, samplenum of another is greater or equal.
             Second value is t if bounding seekpoints is equal"
            (let* ((pos (position-if #'(lambda (point)
					 (>= (seekpoint-samplenum point)
					     sample))
				     points))
		   (point (nth pos points)))
	      (if (= sample (seekpoint-samplenum point))
		  (values point point t)
		(values (nth (1- pos) points) point nil)))))

	  (multiple-value-bind (lowerpoint upperpoint pointseq)
	      (find-bounding-seekpoints (seektable-seekpoints seektable))
	    (if pointseq
		;; We are extremely lucky
		;; All we need to do is set input to new frame
		(progn
		  (reader-position bitreader
				   (+ start (seekpoint-offset lowerpoint)))
		  (return-from seek-sample 0)))
	    
	    (setq start-pos (+ start (seekpoint-offset lowerpoint))
		  end-pos (+ start (seekpoint-offset upperpoint))))))
      
    ;; Check implementation limitations

    (if (/= (the non-negative-fixnum (streaminfo-minblocksize streaminfo))
	    (the non-negative-fixnum (streaminfo-maxblocksize streaminfo)))
	(error 'flac-bad-metadata
	       :message "Cannot seek with variable blocksize"))

    (multiple-value-bind (needed-num remainder)
	(floor sample (streaminfo-maxblocksize streaminfo))
      (declare (type non-negative-fixnum needed-num remainder))

      (labels ((dichotomy-search (start end)
				 "Searches for desired frame num by
                                  dividing stream in half"
				 (let* ((first-half start)
					(second-half (floor (+ start end) 2))
					
					(firstnum (progn (reader-position bitreader first-half)
							 (restore-sync bitreader streaminfo)))
					
					(secondnum (progn (reader-position bitreader (1- second-half))
							  (restore-sync bitreader streaminfo))))

				   (if (< needed-num firstnum) (error 'flac-error
								       :message "Seek error"))

				   (cond 
				    ((< secondnum needed-num)
				     (dichotomy-search second-half end))
				    ((> secondnum needed-num)
				     (dichotomy-search start second-half))
				    (t
				     (values start end))))))
	
	(dichotomy-search start-pos end-pos)
	remainder))))
