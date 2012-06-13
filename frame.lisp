(in-package :cl-flac)

(defmethod (setf frame-blocking-strategy) (val (frame frame))
  (setf (slot-value frame 'blocking-strategy)
	(cond
	 ((= val 0) :fixed)
	 ((= val 1) :variable)
	 (t (error "Blocking strategy must be 0 or 1")))))

(defmethod (setf frame-block-size) (val (frame frame))
  (declare (type integer val))
  (setf (slot-value frame 'block-size)
	(cond
	 ((= val 1) 192)               ; 0001
	 ((and (> val 1)
	       (<= val 5))             ; 0010-0101
	  (* 576 (expt 2 (- val 2))))
	 ((= val 6) :get-8-from-end)   ; 0110
	 ((= val 7) :get-16-from-end)  ; 0111
	 ((and (> val 7)
	       (<= val 15))            ; 1000-1111
	  (* 256 (expt 2 (- val 8))))
	 (t (error "Frame block size is invalid")))))

(defmethod (setf frame-sample-rate) (val (frame frame))
  (declare (type integer val))
  (if (= val 15) (error "Frame sample rate is invalid"))
  (let ((sample-rates (list
		       (slot-value (frame-streaminfo frame) 'samplerate) ; 0000
		       88200   ; 0001
		       176400  ; 0010
		       192000  ; 0011
		       8000    ; 0100
		       16000   ; 0101
		       22050   ; 0110
		       24000   ; 0111
		       32000   ; 1000
		       44100   ; 1001
		       48000   ; 1010
		       96000   ; 1011
		       :get-8-bit-from-end-khz
		       :get-16-bit-from-end-hz
		       :get-16-bit-from-end-tenshz)))
		       
  (setf (slot-value frame 'sample-rate)
	(nth val sample-rates))))

(defmethod (setf frame-channel-assignment) (val (frame frame))
  (declare (type integer val))
  (setf (slot-value frame 'channel-assignment)
	(cond ((and (>= val 0)
		    (<= val 7)) val)   ; 0000-0111
	      ((= val 8) :left/side)   ; 1000
	      ((= val 9) :right/side)  ; 1001
	      ((= val 10) :mid/side)   ; 1001
	      (t (error "Invalid channel assignment")))))

(defmethod (setf frame-sample-size) (val (frame frame))
  (declare (type integer val))
  (let ((sample-sizes (list
		       (1+ (slot-value (frame-streaminfo frame) 'bitspersample-1)) ; 000
		       8            ; 001
		       12           ; 010
		       :reserved    ; 011
		       16           ; 100
		       20           ; 101
		       24           ; 110
		       :reserved))) ; 111
    (setf (slot-value frame 'sample-size) (nth val sample-sizes))))

;; Residual reader
(defun residual-reader (bit-reader subframe frame)
  (let ((coding-method (funcall bit-reader 2)))
    (cond
     ((= coding-method 0) ; 00
      (residual-body-reader bit-reader subframe frame
			    :param-len 4
			    :esc-code #b1111))
     ((= coding-method 1) ; 01
      (residual-body-reader bit-reader subframe frame
			    :param-len 5
			    :esc-code #b11111))
     (t (error "Invalid residual coding method")))))

(defun residual-body-reader (bit-reader subframe frame &key param-len esc-code)
  (let* ((order (funcall bit-reader 4))
	 (total-part (expt 2 order))
	 (residual-buf (make-array (- (frame-block-size frame)
				      (subframe-order subframe))))
	 (sample-idx 0))

    (loop for i below total-part do
	  (let ((samples-num
		 (cond
		  ;; FIXME:: Check following lines
		  ((zerop i) (- (/ (frame-block-size frame) total-part) (subframe-order subframe)))
		  (t (/ (frame-block-size frame) total-part))))
		 (rice-parameter (funcall bit-reader param-len)))
	    
	    (cond
	       ((< rice-parameter esc-code)
		(loop for sample below samples-num do
		      (setf (aref residual-buf sample-idx)
			    (read-rice-signed bit-reader rice-parameter))
		      (incf sample-idx)))
	       (t
		;; FIXME: read unencoded signed rice
		;; Do we need to store bps?
		;; Read bps:
		(let ((residual-partition
		       (make-array samples-num
				   :displaced-to residual-buf
				   :displaced-index-offset sample-idx)))
		  (setq rice-parameter (funcall bit-reader 5))
		  (integer-to-array (funcall bit-reader (* rice-parameter samples-num))
				    residual-partition rice-parameter :signed t)) ; Will be replaced with faster reader later
		(incf sample-idx samples-num)))))
    residual-buf))

;; Subframe reader
(defmethod subframe-body-reader (bit-reader (subframe subframe-fixed) frame)
  (let* ((bps (frame-sample-size frame))
	 (samples (subframe-order subframe))
	 (warm-up
	  (make-array samples
		     :element-type (list 'signed-byte bps)))
	 (chunk (funcall bit-reader (* samples bps))))
    (integer-to-array chunk warm-up bps :signed t)
    (setf (subframe-warm-up subframe) warm-up))
  (setf (subframe-residual subframe)
	(residual-reader bit-reader subframe frame)))

(defmethod subframe-body-reader (bit-reader (subframe subframe-constant) frame)
  (with-slots (sample-size) frame
	      (setf (subframe-constant-value subframe) ;; FIXME: value is signed in original libFLAC
		    (unsigned-to-signed
		     (funcall bit-reader sample-size)
		     sample-size))))

(defmethod subframe-body-reader (bit-reader (subframe subframe-verbatim) frame)
  (with-slots (sample-size block-size) frame
	      (let ((buf
		     (make-array block-size
				 ;; FIXME: value is signed in original libFLAC
				 :element-type (list 'signed-byte sample-size)))
		    (chunk (funcall bit-reader (* sample-size block-size))))

		(integer-to-array chunk buf sample-size :signed t)
		(setf (subframe-verbatim-buffer subframe) buf))))

(defun subframe-reader (bit-reader frame)
  (if (/= (funcall bit-reader 1) 0) (error "Error reading subframe"))
    (let* ((type-num (funcall bit-reader 6))
	   (type-args
	    (cond
	     ((= type-num 0) '(subframe-constant))         ; 000000
	     ((= type-num 1) '(subframe-verbatim))         ; 000001
	     ((and
	       (>= type-num 8)
	       (<= type-num 12))
	      (list 'subframe-fixed :order (- type-num 8)))     ; 001000-001100
	     ((and
	       (>= type-num 32)
	       (<= type-num 63))
	      (list 'subframe-lpc :order (1+ (- type-num 32)))) ; 100000-111111
	     (t (error "Error subframe type"))))
	   (wasted-bits (funcall bit-reader 1)))

      ;; FIXME: read wasted bits correctly
      (if (= wasted-bits 1) (error "Do not know what to do with wasted bits"))
      (let ((subframe (apply #'make-instance
			     (append type-args (list :wasted-bps wasted-bits)))))
	(subframe-body-reader bit-reader subframe frame)
	subframe)))

(defun frame-reader (stream streaminfo)
  (let ((frame (make-instance 'frame :streaminfo streaminfo)))
    (let ((chunk (read-to-integer stream 4)))
      (if (/= +frame-sync-code+ (ldb (byte 14 #.(- 32 14)) chunk)) (error "Frame sync code is not 11111111111110"))
      (if (/= 0 (ldb (byte 1 #.(- 32 14 1)) chunk)) (error "Error reading frame"))
      (setf (frame-blocking-strategy frame) (ldb (byte 1 #.(- 32 14 1 1)) chunk))

      (setf (frame-block-size frame) (ldb (byte 4 #.(- 32 14 1 1 4)) chunk))
      (setf (frame-sample-rate frame) (ldb (byte 4 #.(- 32 14 1 1 4 4)) chunk))

      (setf (frame-channel-assignment frame) (ldb (byte 4 #.(- 8 4)) chunk))
      (setf (frame-sample-size frame) (ldb (byte 3 #.(- 8 4 3)) chunk))
      (if (/= 0 (ldb (byte 1 0) chunk)) (error "Error reading frame"))

      (setf (frame-number frame)
	    (if (eql (frame-blocking-strategy frame) :fixed)
		(read-utf8-u32 stream)
	      (error "Variable block size not implemented yet")))

      (with-slots (block-size) frame
		 (setf block-size
		       (cond
			((eql block-size :get-8-from-end)
			 (1+ (read-byte stream)))
			((eql block-size :get-16-from-end)
			 (1+ (read-to-integer stream 2)))
			(t block-size))))

      (with-slots (sample-rate) frame
		 (setf sample-rate
		       (cond
			((eql sample-rate :get-8-bit-from-end-khz)
			 (* 1000 (read-byte stream)))
			((eql sample-rate :get-16-bit-from-end-hz)
			 (read-to-integer stream 2))
			((eql sample-rate :get-16-bit-from-end-tenshz)
			 (* 10 (read-to-integer stream 2)))
			(t sample-rate))))
      (setf (frame-crc-8 frame) (read-byte stream)))

    (let ((bit-reader (make-bit-reader stream)))
      (setf (frame-subframes frame)
	    ;; FIXME: maybe we should use channel-assignment?
	    (loop for sf below (1+ (slot-value streaminfo 'channels-1)) collect
		  (subframe-reader bit-reader frame)))
      ;; Check zero padding
      (multiple-value-bind (bit remainder) (funcall bit-reader 0)
	(declare (ignore bit))
	(if (/= remainder 0) (error "Padding to byte-alignment is not zero"))))
    (setf (frame-crc-16 frame) (read-to-integer stream 2))
    frame))
