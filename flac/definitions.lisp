(in-package :easy-audio.flac)

(define-condition flac-error (error simple-condition) ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "General flac error: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "General (unspecified) flac error"))

(define-condition flac-bad-metadata (flac-error)
  ((metadata     :reader flac-metadata
                 :initarg :metadata
                 :documentation "Current metadata"))
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "Bad metadata: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "Flac metadata error"))

(define-condition flac-bad-frame (flac-error) ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "Bad frame: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "Bad flac frame"))

;; Metadata
(sera:defconstructor metadata-header
  "Metadata block header"
  (start-position non-negative-integer)
  (last-block-p   boolean)
  (type           (unsigned-byte 7))
  (length         (unsigned-byte 24)))

(sera:defconstructor unknown-metadata
  "Unknown metadata block"
  (rawdata (sa-ub 8)))

(sera:defconstructor streaminfo
  "STREAMINFO metadata block. The most important metadata block in the
file."
  (%minblocksize  (unsigned-byte 16))
  (%maxblocksize  (unsigned-byte 16))
  (%minframesize  (unsigned-byte 24))
  (%maxframesize  (unsigned-byte 24))
  (%samplerate    positive-fixnum)
  (%channels      (integer 1 8))
  (%bitspersample (integer 4 32))
  (%totalsamples  (unsigned-byte 36))
  (%md5           (sa-ub 8)))

(define-documented-accessors streaminfo
  (minblocksize  "The minimum block size (in samples) used in the stream.")
  (maxblocksize  "The maximum block size (in samples) used in the stream.")
  (minframesize  "The minimum frame size (in bytes) used in the stream.")
  (maxframesize  "The maximum frame size (in bytes) used in the stream.")
  (samplerate    "Sample rate in Hz.")
  (channels      "The number of channels in a stream. May be from 1 to 8.")
  (bitspersample "Bits per sample (from 4 to 32).")
  (totalsamples  "Total number of samples in a stream. May be 0 if unknown.")
  (md5           "MD5 checksum of the whole unencoded data."))

(deftype metadata ()
  '(or unknown-metadata streaminfo))

#|
(defclass padding (metadata-header) ()
  (:documentation "Represents PADDING metadata block"))

(sera:defconstructor seekpoint
  "A seekpoint (entry in a seektable)"
  (samplenum        (ub 64))
  (offset           (ub 64))
  (samples-in-frame (ub 16)))

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
                   :documentation "List of tracks"))
  (:documentation "CUESHEET metadata block"))

(sera:defconstructor cuesheet-track
  "Represents a track in a cuesheet metadata"
  (offset       (unsigned-byte 64))
  (number       (unsigned-byte 8))
  (isrc         string)
  (type         (member :audio :non-audio))
  (pre-emphasis boolean)
  (indices      list))

(sera:defconstructor cuesheet-index
  "Represents an index into a track in a cuesheet metadata"
  (offset (unsigned-byte 64))
  (number (unsigned-byte 8)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +picture-types+
      '(:other :file-icon :other-file-icon
        :cover-front :cover-back) ; Etc
    :documentation "Meaning of picture type codes"
    :test #'equalp))
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
   (width          :type positive-integer
                   :accessor picture-width
                   :documentation "Picture width")
   (height         :type positive-integer
                   :accessor picture-height
                   :documentation "Picture height")
   (depth          :type positive-integer
                   :accessor picture-depth
                   :documentation "Picture color depth")
   (color-num      :type non-negative-integer
                   :accessor picture-color-num
                   :documentation "Number of colors in indexed picture, 0 for non-indexed")
   (picture        :type (sa-ub 8)
                   :accessor picture-picture
                   :documentation "The picture itself as array of octets"))
  (:documentation "PICTURE metadata block"))


(defgeneric read-metadata-body (stream data)
  (:documentation "Reads a body of the metadata block DATA from STREAM. Can depend on slots
  common to all metadata blocks (which are in the header)."))
|#

;; Subframes
(deftype blocksize ()
  "Possible size of a frame in samples."
  '(and (unsigned-byte 16) (not (eql 0))))

(deftype subframe ()
  '(or subframe-constant subframe-verbatim subframe-fixed subframe-lpc))

(sera:defconstructor subframe-header
  (wasted-bps non-negative-fixnum)
  (actual-bps (integer 4 33))
  (block-size blocksize))

(sera:defconstructor subframe-constant
  (header subframe-header)
  (value  (sb 32)))

(sera:defconstructor subframe-verbatim
  (header subframe-header)
  (data   (sa-sb 32)))

(sera:defconstructor subframe-lpc
  (header subframe-header)
  (order           (integer 1 32))
  (precision       fixnum)
  (coeff-shift     (sb 32))
  (predictor-coeff (sa-sb 32))
  (residual        (sa-sb 32)))

(sera:defconstructor subframe-fixed
  (header   subframe-header)
  (order    (integer 0 4))
  (residual (sa-sb 32)))

;; Add 1 to values described in FLAC specs
(defconstant +left-side+ #b1001  ; 1000 in spec
  "The encoded frame contains data for the left channel + the
difference with the right channel.")
(defconstant +right-side+ #b1010 ; 1001 in spec
  "The encoded frame contains data for the right channel + the
difference with the left channel.")
(defconstant +mid-side+ #b1011   ; 1010 in spec
  "The encoded frame contains data for the left/right average + the
difference with the left/right channels.")
(defconstant +max-channels+ 8)

;; Frame
(sera:defconstructor frame
  "Atomic element of audio data in the FLAC stream"
  (%blocking-strategy (member :fixed :variable))
  (%block-size         blocksize)
  (%sample-rate        positive-fixnum)
  (%channel-assignment (integer 1 11))
  (%sample-size        (integer 4 32))
  (%number             unsigned-byte)
  (%crc-8              (ub 8))
  (%subframes          list)
  (%crc-16             (ub 16)))


;; Document accessors
(define-documented-accessors frame
  (blocking-strategy
   "Is the blocking strategy :FIXED (frame header contains the frame
number) or :VARIABLE (frame header contains the sample number)?")
  (block-size
   "Block size in samples.")
  (sample-rate
   "Block sample rate in Hertz.")
  (channel-assignment
   "Number of channels or one of @c(+mid-side+), @c(+left-side+), @c(+right-side+).")
  (sample-size
   "Bits per sample.")
  (number
   "Number of a frame or of the first sample in the frame.")
  (crc-8
   "CRC8 of a frame header (including the sync code).")
  (subframes
   "List of subframes (one for each channel).")
  (crc-16
   "CRC16 of the frame (back to and including the sync code)."))

(defconstant +frame-sync-code+ 16382) ; 11111111111110
(defconstant +seekpoint-placeholder+ #xFFFFFFFFFFFFFFFF)
(define-constant +coded-sample-rates+
    '(88200  ; 0001
      176400 ; 0010
      192000 ; 0011
      8000   ; 0100
      16000  ; 0101
      22050  ; 0110
      24000  ; 0111
      32000  ; 1000
      44100  ; 1001
      48000  ; 1010
      96000) ; 1011
  :test #'equalp)

(define-constant +coded-sample-sizes+
    '((#b001 . 8)
      (#b010 . 12)
      (#b100 . 16)
      (#b101 . 20)
      (#b110 . 24))
  :test #'equalp)
