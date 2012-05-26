(in-package :cl-flac)

(deftype u8 () '(unsigned-byte 8))

(defclass metadata-header ()
  (last-block-p type length rawdata))

(defclass streaminfo (metadata-header)
  (minblocksize
   maxblocksize
   minframesize
   maxframesize
   samplerate
   channels-1
   bitspersample-1
   totalsamples
   md5))

(defgeneric metadata-body-reader (stream data))

(defclass subframe ()
  ((wasted-bps :accessor subframe-wasted-bps :initarg :wasted-bps)))

(defclass subframe-constant (subframe)
  ((constant-value :accessor subframe-constant-value)))

(defgeneric subframe-body-reader (stream subframe frame))

(defclass frame ()
  ((streaminfo :accessor frame-streaminfo :initarg :streaminfo)
   (blocking-strategy :accessor frame-blocking-strategy)
   (block-size :accessor frame-block-size)
   (sample-rate :accessor frame-sample-rate)
   (channel-assignment :accessor frame-channel-assignment)
   (sample-size :accessor frame-sample-size)
   (number :accessor frame-number)
   (crc-8 :accessor frame-crc-8)
   (subframes :accessor frame-subframes)
   (crc-16 :accessor frame-crc-16)))

(defparameter +block-name+ '(streaminfo padding application seektable vorbis-comment cuesheet picture)) ;; In case of using sbcl defconstant will give an error
(defconstant +frame-sync-code+ 16382) ; 11111111111110

;; Other stuff

(defun get-metadata-type (code)
  (if (= code 127) (error "Code 127 is invalid"))
  (nth code +block-name+))

(defun bytes-to-integer-big-endian (list)
  (let* ((mul (expt 2 (* 8 (1- (length list)))))
	 (position-value (* mul (car list))))
    (if (cdr list)
	(+ position-value (bytes-to-integer-big-endian (cdr list)))
      position-value)))

(defun read-to-integer (stream num &optional (func #'bytes-to-integer-big-endian))
  (let ((lst (make-list num)))
    (read-sequence lst stream)
    (funcall func lst)))
