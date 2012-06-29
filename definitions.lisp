(in-package :cl-flac)

(deftype u8 () '(unsigned-byte 8))

;; Metadata
(defclass metadata-header ()
  ((last-block-p :accessor metadata-last-block-p)
   (type :accessor metadata-type)
   (length :accessor metadata-length)
   rawdata))

(defclass streaminfo (metadata-header)
  ((minblocksize :accessor streaminfo-minblocksize)
   (maxblocksize :accessor streaminfo-maxblocksize)
   (minframesize :accessor streaminfo-minframesize)
   (maxframesize :accessor streaminfo-maxframesize)
   (samplerate :accessor streaminfo-samplerate)
   (channels-1 :accessor streaminfo-channels-1)
   (bitspersample-1 :accessor streaminfo-bitspersample-1)
   (totalsamples :accessor streaminfo-totalsamples)
   (md5 :accessor streaminfo-md5)))

(defclass padding (metadata-header) ())

(defgeneric metadata-body-reader (stream data))

;; Subframes
(defclass subframe ()
  ((wasted-bps :accessor subframe-wasted-bps
	       :initarg :wasted-bps
	       :type fixnum)))

(defclass subframe-constant (subframe)
  ((constant-value :accessor subframe-constant-value
		   :type (signed-byte 32))))

(defclass subframe-verbatim (subframe)
  ((buffer :accessor subframe-verbatim-buffer
	   :type (simple-array (signed-byte 32)))))

(defclass subframe-lpc (subframe)
  ((out-buf :accessor subframe-out-buf
	    :type (simple-array (signed-byte 32))) ;; Warm-up + residual
   (order :accessor subframe-order
	  :initarg :order
	  :type fixnum)
   (precision :accessor subframe-lpc-precision
	      :type fixnum)
   (predictor-coeff :accessor subframe-lpc-predictor-coeff)
   (coeff-shift :accessor subframe-lpc-coeff-shift)))

(defclass subframe-fixed (subframe)
  ((out-buf :accessor subframe-out-buf
	    :type (simple-array (signed-byte 32)))
   (order :accessor subframe-order
	  :initarg :order
	  :type fixnum)))

(defgeneric subframe-body-reader (bit-reader subframe frame))
(defgeneric subframe-decode (subframe frame))

;; Frame
(defclass frame ()
  ((streaminfo :accessor frame-streaminfo
	       :initarg :streaminfo)
   (blocking-strategy :accessor frame-blocking-strategy
		      :type symbol)
   (block-size :accessor frame-block-size)
   (sample-rate :accessor frame-sample-rate)
   (channel-assignment :accessor frame-channel-assignment)
   (sample-size :accessor frame-sample-size
		:type fixnum)
   (number :accessor frame-number)
   (crc-8 :accessor frame-crc-8
	  :type u8)
   (subframes :accessor frame-subframes
	      :type list)
   (crc-16 :accessor frame-crc-16
	   :type fixnum)))

(defparameter +block-name+ '(streaminfo padding application seektable vorbis-comment cuesheet picture)) ;; In case of using sbcl defconstant will give an error
(defconstant +frame-sync-code+ 16382) ; 11111111111110

;; Other stuff

(defun get-metadata-type (code)
  (if (= code 127) (error "Code 127 is invalid"))
  (nth code +block-name+))
