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

(defun subframe-reader (stream)
  (let ((chunk (read-byte stream)))
    (if (/= (ldb (byte 1 0) chunk) 0) (error "Error reading subframe"))
    (let* ((type-num (ldb (byte 6 1) chunk))
	   (type
	    (cond
	     ((= type-num 0) 'subframe-constant)         ; 000000
	     ((= type-num 1) 'subframe-verbatim)         ; 000001
	     ((and
	       (>= type-num 8)
	       (<= type-num 12))
	      (list 'subframe-fixed (- type-num 8)))     ; 001000-001100
	     ((and
	       (>= type-num 32)
	       (<= type-num 63))
	      (list 'subframe-lpc (1+ (- type-num 32)))) ; 100000-111111
	     (t (error "Error subframe type"))))

	   ))) t)
;	   (subframe (make-instance type)))

(defun frame-reader (stream streaminfo)
  (let ((frame (make-instance 'frame :streaminfo streaminfo)))
    (let ((chunk (read-to-integer stream 2)))
      (if (/= +frame-sync-code+ (ldb (byte 14 #.(- 16 14)) chunk)) (error "Frame sync code is not 11111111111110"))
      (if (/= 0 (ldb (byte 1 #.(- 16 14 1)) chunk)) (error "Error reading frame"))
      (setf (frame-blocking-strategy frame) (ldb (byte 1 #.(- 16 14 1 1)) chunk))

      (setq chunk (read-byte stream))
      (setf (frame-block-size frame) (ldb (byte 4 0) chunk))
      (setf (frame-sample-rate frame) (ldb (byte 4 4) chunk))

      (setq chunk (read-byte stream))
      (setf (frame-channel-assignment frame) (ldb (byte 4 0) chunk))
      (setf (frame-sample-size frame) (ldb (byte 3 4) chunk))
      (if (/= 0 (ldb (byte 1 7) chunk)) (error "Error reading frame"))
      ;; FIXME: How to read sample/frame number?
      (setq chunk (read-byte stream)) ; Do something
      (if (eql (frame-blocking-strategy frame) :fixed)
	  nil nil) ; Do something
      ;; /FIXME

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
      (setf (frame-crc-8 frame) (read-byte stream))
      (setf (frame-subframes frame)
	    ; FIXME: maybe we should use channel-assignment?
	    (loop for sf below (1+ (slot-value streaminfo 'channels-1)) collect
		  (subframe-reader stream))))
    frame))
