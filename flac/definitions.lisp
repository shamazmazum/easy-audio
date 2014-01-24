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

(define-condition flac-error ()
  ((message :initarg :message
	    :initform ""
	    :type string
	    :reader flac-error-message
            :documentation "Error message"))
  (:report (lambda (c s)
	     (format s "General flac error: ~A"
		     (flac-error-message c))))
  (:documentation "General (unspecified) flac error"))

(define-condition flac-bad-metadata (flac-error)
  ((metadata     :reader flac-metadata
		 :initarg :metadata))
  (:report (lambda (c s)
	     (format s "Bad metadata: ~A"
		     (flac-error-message c))))
  (:documentation "Flac metadata error"))

(define-condition flac-bad-frame (flac-error) ()
  (:report (lambda (c s)
	     (format s "Bad frame: ~A"
		     (flac-error-message c))))
  (:documentation "Bad flac frame"))

;; Metadata
(defclass metadata-header ()
  ((last-block-p    :initarg :last-block-p
                    :accessor metadata-last-block-p
		    :type boolean
                    :documentation "T if this metadata block is the last in file")
   (length          :initarg :length
                    :accessor metadata-length
                    :type positive-int
                    :documentation "Length of this metadata block in bytes (with exclusion of
header)")
   (rawdata         :initarg :rawdata
                    :type (simple-array ub8))
   (start-position  :initarg :start-position
                    :documentation "Strart position of metadata block"
		    :type non-negative-int
		    :accessor metadata-start-position))
  (:documentation "Class for storing flac metadata. Instance of this class means unknown
metadata type"))

(defclass streaminfo (metadata-header)
  ((minblocksize  :accessor streaminfo-minblocksize
		  :type non-negative-fixnum
                  :documentation "The minimum block size (in samples) used in the stream")
   (maxblocksize  :accessor streaminfo-maxblocksize
		  :type non-negative-fixnum
                  :documentation "The maximum block size (in samples) used in the stream")
   (minframesize  :accessor streaminfo-minframesize
		  :type non-negative-fixnum
                  :documentation "The minimum frame size (in bytes) used in the stream")
   (maxframesize  :accessor streaminfo-maxframesize
		  :type non-negative-fixnum
                  :documentation "The maximum frame size (in bytes) used in the stream. May be
0 to imply the value is not known.")
   (samplerate    :accessor streaminfo-samplerate
		  :type non-negative-fixnum
                  :documentation "Sample rate in Hz")
   (channels      :accessor streaminfo-channels
		  :type (integer 1 8)
                  :documentation "Number of channels in stream. May be from 1 to 8.")
   (bitspersample :accessor streaminfo-bitspersample
		  :type non-negative-fixnum
                  :documentation "Bits per sample (from 4 to 32)")
   (totalsamples  :accessor streaminfo-totalsamples
		  :type positive-int
                  :documentation "Total samples in stream. May be 0 if unknown.")
   (md5           :accessor streaminfo-md5
                  :documentation "MD5 checksum of the whole unencoded data"))
  (:documentation "Class for storing STREAMINFO metadata block"))

(defclass padding (metadata-header) ()
  (:documentation "Represents PADDING metadata block"))

(defstruct seekpoint
  "A seekpoint (entry in sdeektable)"
  (samplenum 0        :type (unsigned-byte 64))
  (offset 0           :type (unsigned-byte 64))
  (samples-in-frame 0 :type (unsigned-byte 16)))

(defclass seektable (metadata-header)
  ((seekpoints :accessor seektable-seekpoints
	       :type list
               :documentation "List of seekpoints"))
  (:documentation "SEEKTABLE metadata block"))

(defclass vorbis-comment (metadata-header)
  ((vendor-comment :type string
		   :accessor vorbis-vendor-comment
                   :documentation "Vendor comment")
   (user-comments  :type list
		   :accessor vorbis-user-comments
                   :documentation "List of user comments"))
  (:documentation "VORBIS_COMMENT metadata block"))

(defclass cuesheet (metadata-header)
  ((catalog-id     :type string
		   :accessor cuesheet-catalog-id
                   :documentation "Media catalog number")
   (lead-in        :accessor cuesheet-lead-in
                   :documentation "For CD-DA cuesheets, number of lead-in samples; 0 otherwise")
   (cdp            :accessor cuesheet-cdp
		   :type boolean
		   :documentation "t if cueshhet corresponds to Compact Disk")
   (tracks         :accessor cuesheet-tracks
                   :type list
                   :documentation "List of tracks")))

(defstruct cuesheet-track
  offset
  number
  isrc
  type
  pre-emphasis
  indices)

(defstruct cuesheet-index offset number)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +picture-types+ '(:other :file-icon :other-file-icon
                                  :cover-front :cover-back) ; Etc
    "Meaning of picture type codes"))
(deftype picture-type-id () `(or (integer 0 20) (member ,@+picture-types+)))

(defclass picture (metadata-header)
  ((picture-type   :type picture-type-id
                   :accessor picture-type
                   :documentation "One of 21 picture types (see flac format description)")
   (mime-type      :type string
                   :accessor picture-mime-type
                   :documentation "String with MIME type")
   (description    :type string
                   :accessor picture-description
                   :documentation "Picture description (UTF-8 coded string)")
   (width          :type positive-int
                   :accessor picture-width
                   :documentation "Picture width")
   (height         :type positive-int
                   :accessor picture-height
                   :documentation "Picture height")
   (depth          :type positive-int
                   :accessor picture-depth
                   :documentation "Picture color depth")
   (color-num      :type non-negative-int
                   :accessor picture-color-num
                   :documentation "Number of colors in indexed picture, 0 for non-indexed")
   (picture        :type simple-ub8-vector
                   :accessor picture-picture
                   :documentation "The picture itself as array of octets"))
  (:documentation "PICTURE metadata block"))
                   

(defgeneric metadata-body-reader (stream data)
  (:documentation "Reads a body of the metadata block DATA from STREAM. Can depend on slots
  common to all metadata blocks (which are in the header)."))

;; Subframes
(defclass subframe ()
  ((wasted-bps :accessor subframe-wasted-bps
	       :initarg :wasted-bps
	       :type non-negative-fixnum)
   (actual-bps :accessor subframe-actual-bps
	       :initarg :actual-bps
	       :type (integer 4 33))
   (out-buf    :accessor subframe-out-buf
	       :initarg :out-buf
	       :type (simple-array (signed-byte 32))))
  (:documentation "An ancestor of all 4 types of subframes. Is not instaneated."))

(defclass subframe-constant (subframe)
  ((constant-value :accessor subframe-constant-value
		   :type (signed-byte 32)
                   :documentation "The value of all samples"))
  (:documentation "Subframe with швутешсфд samples"))

(defclass subframe-verbatim (subframe) ()
  (:documentation "Unencoded audio data"))

(defclass subframe-lpc (subframe)
  ((order           :accessor subframe-order
	            :initarg :order
	            :type fixnum
                    :documentation "The predictor's order")
   (precision       :accessor subframe-lpc-precision
	            :type fixnum)
   (predictor-coeff :accessor subframe-lpc-predictor-coeff
		    :type (simple-array (signed-byte 32)))
   (coeff-shift     :accessor subframe-lpc-coeff-shift
		    :type (signed-byte 32)))
  (:documentation "Subframe with FIR linear predictor"))

(defclass subframe-fixed (subframe)
  ((order :accessor subframe-order
	  :initarg :order
	  :type fixnum
          :documentation "The predictor's order"))
  (:documentation "Subframe with fixed linear predictor"))

(defgeneric subframe-body-reader (bit-reader subframe frame)
  (:documentation "Read a SUBFRAME within given FRAME from BIT-READER. Can depend on slots
common to all subframes (which are in the header)."))
(defgeneric subframe-decode (subframe frame)
  (:documentation "Decode a SUBFRAME within current FRAME. Returns buffer of decoded data
  destructively modifying (and garbaging) the subframe."))

;; Add 1 to values described in FLAC specs
(defconstant +left-side+ #b1001)  ; 1000 in spec
(defconstant +right-side+ #b1010) ; 1001 in spec
(defconstant +mid-side+ #b1011)   ; 1010 in spec
(defconstant +max-channels+ 8)

;; Frame
(defclass frame ()
  ((blocking-strategy  :reader frame-blocking-strategy
		       :type (member :fixed :variable)
                       :documentation "Is the blocking strategy :FIXED (frame header contains the
frame number) or :VARIABLE (frame header contains the sample number)")
   (block-size         :reader frame-block-size
                       :type non-negative-fixnum
                       :documentation "Block size in samples")
   (sample-rate        :reader frame-sample-rate
                       :type non-negative-fixnum
                       :documentation "Sample rate")
   (channel-assignment :reader frame-channel-assignment
		       :documentation "Number of channels or one of
                                       :mid/side, :left/side, :right/side"
		       :type (integer 0 10))
   (sample-size        :reader frame-sample-size
		       :type (integer 4 32)
                       :documentation "Bits per sample")
   (number             :reader frame-number
                       :type unsigned-byte
                       :initform #.(ash 1 36)
                       :documentation "Frame/sample number")
   (crc-8              :accessor frame-crc-8
	               :type ub8
                       :documentation "CRC8 of a frame header (including the sync code)")
   (subframes          :accessor frame-subframes
	               :type list
                       :documentation "List of subframes (one for each channel)")
   (crc-16             :accessor frame-crc-16
	               :type fixnum
                       :documentation "CRC16 of the frame (back to and including the sync
code)"))
  (:documentation "Audio frame class"))

(defgeneric frame-reader (stream streaminfo &optional out-buffers)
  (:documentation "Read a frame from a stream"))

(defparameter +block-name+ '((0 . streaminfo)
                             (1 . padding)
                             (3 . seektable)
                             (4 . vorbis-comment)
                             (5 . cuesheet)
                             (6 . picture)))
(defconstant +frame-sync-code+ 16382) ; 11111111111110
(defconstant +seekpoint-placeholder+ #xFFFFFFFFFFFFFFFF)
(defparameter +coded-sample-rates+
  '(88200   ; 0001
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
    :get-16-bit-from-end-tenshz))
(defparameter +coded-sample-sizes+
  '((#b001 . 8)
    (#b010 . 12)
    (#b100 . 16)
    (#b101 . 20)
    (#b110 . 24)))

;; Other stuff
(defun get-metadata-type (code)
  "Get metadata type by code"
  (let ((mtype (assoc code +block-name+)))
    (if mtype (cdr mtype) 'metadata-header)))
