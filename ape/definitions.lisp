(in-package :easy-audio.ape)

;; Conditions
(define-condition ape-error (error simple-condition) ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "Ape error: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "General APE error"))

(define-condition apev2-tag-error (ape-error) ()
  (:documentation "APEv2 tag error"))

;; Some constants
(defconstant +code-bits+ 32)
(defconstant +top-value+ (ash 1 (1- +code-bits+)))
(defconstant +shift-bits+ (- +code-bits+ 9))
(defconstant +extra-bits+ (1+ (rem (- +code-bits+ 2) 8)))
(defconstant +bottom-value+ (ash +top-value+ -8))

;; Structures
(defstruct metadata
  (version            0 :type (ub 16))
  (padding1           0 :type (ub 16))
  (desc-len           0 :type (ub 32))
  (header-len         0 :type (ub 32))
  (seektable-len      0 :type (ub 32))
  (wavheader-len      0 :type (ub 32))
  (audiodata-len      0 :type (ub 32))
  (audiodata-len-high 0 :type (ub 32))
  (wavtail-len        0 :type (ub 32))
  header-md5
  (compression-type   0 :type (ub 16))
  (format-flags       0 :type (ub 16))
  (blocks-per-frame   0 :type (ub 32))
  (final-frame-blocks 0 :type (ub 32))
  (total-frames       0 :type (ub 32))
  (bps                0 :type (ub 16))
  (channels           0 :type (ub 16))
  (samplerate         0 :type (ub 32))
  ;; Derived fields
  (first-frame        0 :type (ub 32))
  (total-samples      0 :type (ub 32))
  bittable
  seektable)

(defstruct rice-state
  (k    10    :type (integer 0 24))
  (ksum 16384 :type (ub 32)))

(defstruct range-coder
  (low    0 :type (ub 32))
  (range  (ash 1 +extra-bits+)
            :type (ub 32))
  (help   0 :type (ub 32))
  (buffer 0 :type (ub 32)))

(defstruct frame
  (version 0   :type (ub 16))
  (fset    0   :type (integer 0 4)) ; Compression level
  (samples 0   :type (ub 32))
  (bps     0   :type (ub 16))
  (flags   0   :type (ub 32))
  (buffer  0   :type (ub 8)) ; From rc structure. Why it is called buffer?!
  (output  nil :type list)
  crc)

;; Generic functions
(defgeneric read-metadata-header (reader ape-version)
  (:documentation "Read and fill METADATA-HEADER structure"))

(defgeneric read-bittable (reader ape-version)
  (:documentation "Read bittable from the beginning of APE file"))

(defgeneric entropy-decode (reader frame ape-version)
  (:documentation "Read entropy buffer in a frame"))

(defgeneric predictor-decode (frame ape-version channels)
  (:documentation "Apply filters to entropy"))

(defgeneric predictor-update (frame ape-version channels)
  (:documentation "Apply/Update predictor filters"))
