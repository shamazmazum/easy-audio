(in-package :cl-flac)

(defmethod (setf frame-blocking-strategy) (val (frame frame))
  (declare (type (integer 0 1) val)
	   (optimize (speed 3)))
  (setf (slot-value frame 'blocking-strategy)
	(cond
	 ((= val 0) :fixed)
	 ((= val 1) :variable)
	 (t (error "Blocking strategy must be 0 or 1")))))

(defmethod (setf frame-block-size) (val (frame frame))
  (declare (type (unsigned-byte 4) val)
	   (optimize (speed 3)))
  (setf (slot-value frame 'block-size)
	(cond
	 ((= val 1) 192)               ; 0001
	 ((and (> val 1)
	       (<= val 5))             ; 0010-0101
	  (ash 576 (- val 2)))
	 ((= val 6) :get-8-from-end)   ; 0110
	 ((= val 7) :get-16-from-end)  ; 0111
	 ((and (> val 7)
	       (<= val 15))            ; 1000-1111
	  (ash 1 val))
	 (t (error "Frame block size is invalid")))))

(defmethod (setf frame-sample-rate) (val (frame frame))
  (declare (type (unsigned-byte 4) val)
	   (optimize (speed 3)))
  (if (= val 15) (error "Frame sample rate is invalid"))
  (let ((sample-rates (list
		       (streaminfo-samplerate (frame-streaminfo frame)) ; 0000
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
  (declare (type fixnum val)
	   (optimize (speed 3)))
  (setf (slot-value frame 'channel-assignment)
	(cond ((and (>= val 0)
		    (<= val 7)) (1+ val))   ; 0000-0111
	      ((= val 8) :left/side)   ; 1000
	      ((= val 9) :right/side)  ; 1001
	      ((= val 10) :mid/side)   ; 1001
	      (t (error "Invalid channel assignment")))))

(defmethod (setf frame-sample-size) (val (frame frame))
  (declare (type (unsigned-byte 3) val)
	   (optimize (speed 3)))
  (let ((sample-sizes (list
		       (streaminfo-bitspersample (frame-streaminfo frame)) ; 000
		       8            ; 001
		       12           ; 010
		       :reserved    ; 011
		       16           ; 100
		       20           ; 101
		       24           ; 110
		       :reserved))) ; 111
    (setf (slot-value frame 'sample-size) (nth val sample-sizes))))

;; Residual reader
(declaim (inline residual-reader))
(defun residual-reader (bit-reader subframe frame out)
  (declare (optimize (speed 3) (space 0)))
  (let ((coding-method (tbs:read-bits 2 bit-reader)))
    (declare (type (unsigned-byte 2) coding-method))
    (cond
     ((= coding-method 0) ; 00
      (residual-body-reader bit-reader subframe frame out
			    :param-len 4
			    :esc-code #b1111))
     ((= coding-method 1) ; 01
      (residual-body-reader bit-reader subframe frame out
			    :param-len 5
			    :esc-code #b11111))
     (t (error "Invalid residual coding method")))))

(defun residual-body-reader (bit-reader subframe frame out &key param-len esc-code)
  (declare (type fixnum param-len esc-code)
	   (type (simple-array (signed-byte 32)) out)
	   (optimize (speed 3)))

  (let* ((part-order (the (unsigned-byte 4)
		       (tbs:read-bits 4 bit-reader)))
	 (sample-idx (subframe-order subframe))
	 (blocksize (frame-block-size frame))
	 (predictor-order (subframe-order subframe)))
    (declare (type fixnum sample-idx blocksize predictor-order))
    
    (loop for i below (ash 1 part-order) do
	  (let ((samples-num
		 (cond
		  ;; FIXME:: Check following lines
		  ((zerop i) (- (ash blocksize (- part-order)) predictor-order))
		  (t (ash blocksize (- part-order)))))
		(rice-parameter (tbs:read-bits param-len bit-reader)))
	    (declare (type fixnum rice-parameter))
	    
	    (cond
	     ((< rice-parameter esc-code)
	      (loop for sample below samples-num do
		    (setf (aref out sample-idx)
			  (read-rice-signed bit-reader rice-parameter))
		    (incf sample-idx)))
	     (t
	      ;; FIXME: read unencoded signed rice
	      ;; Do we need to store bps?
	      ;; Read bps:
	      (setq rice-parameter (tbs:read-bits 5 bit-reader))
	      (read-bits-array bit-reader out rice-parameter
			       :signed t
			       :offset sample-idx)
	      (incf sample-idx samples-num)))))
    out))

;; Subframe reader

(defmethod subframe-body-reader (bit-reader (subframe subframe-lpc) frame)
  (declare (optimize (speed 3)))
  (let* ((bps (subframe-actual-bps subframe))
	 (warm-up-samples (subframe-order subframe))
	 (out-buf (subframe-out-buf subframe))
	 (coeff-buf (make-array (list warm-up-samples)
				:element-type '(signed-byte 32))))
    
    (read-bits-array bit-reader
		     out-buf bps :signed t :len warm-up-samples)
    
    (let ((precision (1+ (tbs:read-bits 4 bit-reader))))
      (if (= #b10000 precision)
	  (error "lpc coefficients precision cannot be 16")
	(setf (subframe-lpc-precision subframe) precision))
      
      (setf (subframe-lpc-coeff-shift subframe)
	    (unsigned-to-signed (tbs:read-bits 5 bit-reader) 5))

      (setf (subframe-lpc-predictor-coeff subframe)
	    (read-bits-array bit-reader
			     coeff-buf precision :signed t)))

    (residual-reader bit-reader subframe frame out-buf)))

(defmethod subframe-body-reader (bit-reader (subframe subframe-fixed) frame)
  (declare (optimize (speed 3)))
  (let ((bps (subframe-actual-bps subframe))
	(warm-up-samples (subframe-order subframe))
	(out-buf (subframe-out-buf subframe)))
    
    (read-bits-array bit-reader out-buf bps :signed t :len warm-up-samples)

    (residual-reader bit-reader subframe frame out-buf)))

(defmethod subframe-body-reader (bit-reader (subframe subframe-constant) frame)
  (declare (optimize (speed 3)))
  (with-slots (actual-bps) subframe
	      (setf (subframe-constant-value subframe) ;; FIXME: value is signed in original libFLAC
		    (unsigned-to-signed
		     (tbs:read-bits actual-bps bit-reader)
		     actual-bps))))

(defmethod subframe-body-reader (bit-reader (subframe subframe-verbatim) frame)
  (declare (optimize (speed 3)))
  (let ((bps (subframe-actual-bps subframe)))

    (with-slots (out-buf) subframe
		(setf out-buf
		      (read-bits-array bit-reader out-buf bps :signed t)))))

(defun subframe-reader (stream frame actual-bps)
  (declare (optimize (speed 3)))
  (if (/= (tbs:read-bit stream) 0) (error "Error reading subframe"))
    (let* ((type-num (tbs:read-bits 6 stream))
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
	   (wasted-bits (tbs:read-bit stream)))

      ;; FIXME: Do not know what to do with wasted bits
      (if (= wasted-bits 1)
	  (progn
	    (warn "Do not know what to do with wasted bits")
	    (setq wasted-bits (1+ (read-unary-coded-integer stream)))))
      
      (let ((subframe (apply #'make-instance
			     (append type-args (list :wasted-bps wasted-bits
						     :actual-bps actual-bps
						     :out-buf (make-array
							       (list (frame-block-size frame))
							       :element-type '(signed-byte 32)))))))
	(subframe-body-reader stream subframe frame)
	subframe)))

(defun frame-reader (stream streaminfo)
  (let ((frame (make-instance 'frame :streaminfo streaminfo)))
    (if (/= +frame-sync-code+ (tbs:read-bits 14 stream)) (error "Frame sync code is not 11111111111110"))
    (if (/= 0 (tbs:read-bit stream)) (error "Error reading frame"))
    (setf (frame-blocking-strategy frame) (tbs:read-bit stream))
    
    (setf (frame-block-size frame) (tbs:read-bits 4 stream))
    (setf (frame-sample-rate frame) (tbs:read-bits 4 stream))

    (setf (frame-channel-assignment frame) (tbs:read-bits 4 stream))
    (setf (frame-sample-size frame) (tbs:read-bits 3 stream))
    (if (/= 0 (tbs:read-bit stream)) (error "Error reading frame"))

    (setf (frame-number frame)
	  (if (eql (frame-blocking-strategy frame) :fixed)
	      (read-utf8-u32 stream)
	    (error "Variable block size not implemented yet")))
    
    (with-slots (block-size) frame
		(setf block-size
		      (cond
		       ((eql block-size :get-8-from-end)
			(1+ (tbs:read-octet stream)))
		       ((eql block-size :get-16-from-end)
			(1+ (tbs:read-bits 16 stream)))
		       (t block-size))))

    (with-slots (sample-rate) frame
		(setf sample-rate
		      (cond
		       ((eql sample-rate :get-8-bit-from-end-khz)
			(* 1000 (tbs:read-octet stream)))
		       ((eql sample-rate :get-16-bit-from-end-hz)
			(tbs:read-bits 16 stream))
		       ((eql sample-rate :get-16-bit-from-end-tenshz)
			(* 10 (tbs:read-bits 16 stream)))
		       (t sample-rate))))
    (setf (frame-crc-8 frame) (tbs:read-octet stream))

    (let ((assignment (frame-channel-assignment frame)))
      (setf (frame-subframes frame)
	    (typecase assignment
	      (fixnum
	       (loop for sf below (frame-channel-assignment frame) collect
		     (subframe-reader stream frame (frame-sample-size frame))))
	      (symbol
	       ;; Do bps correction
	       (loop for sf below 2 collect
		     (subframe-reader
		      stream frame
		      (cond
		       ((and (eq assignment :left/side)
			     (= sf 1))
			(1+ (frame-sample-size frame)))

		       ((and (eq assignment :right/side)
			     (= sf 0))
			(1+ (frame-sample-size frame)))

		       ((and (eq assignment :mid/side)
			     (= sf 1))
			(1+ (frame-sample-size frame)))
		       (t (frame-sample-size frame))))))
	    (t (error "Wrong channel assignment")))))

    ;; Check zero padding
    (if (/= (tbs:read-to-byte-alignment stream) 0) (error "Padding to byte-alignment is not zero"))
    (setf (frame-crc-16 frame) (tbs:read-bits 16 stream))
  frame))
