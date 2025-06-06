(in-package :easy-audio.ape)

;; Conditions
(define-condition ape-error (error simple-condition)
  ()
  (:report
   (lambda (c s)
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
(sera:defconstructor metadata
  (version            (ub 16))
  (desc-len           (ub 32))
  (header-len         (ub 32))
  (seektable-len      (ub 32))
  (wavheader-len      (ub 32))
  (audiodata-len      (ub 32))
  (audiodata-len-high (ub 32))
  (wavtail-len        (ub 32))
  (header-md5         (sa-ub 8))
  (compression-type   (ub 16))
  (format-flags       (ub 16))
  (blocks-per-frame   (ub 32))
  (final-frame-blocks (ub 32))
  (total-frames       (ub 32))
  (bps                (ub 16))
  (channels           (ub 16))
  (samplerate         (ub 32))
  (total-samples      (ub 32))
  (bittable           t)
  (seektable          (sa-ub 32)))

(defstruct rice-state
  (k    10    :type (integer 0 24))
  (ksum 16384 :type (ub 32)))

(defstruct range-coder
  (low    0 :type (ub 32))
  (range  (ash 1 +extra-bits+)
            :type (ub 32))
  (help   0 :type (ub 32))
  (buffer 0 :type (ub 32)))

(sera:defconstructor frame
  (version (ub 16))
  ;; Compression level
  (fset    (integer 0 4))
  (samples (ub 32))
  (bps     (ub 16))
  (flags   (ub 32))
  (buffer  (ub 8))
  (crc     (ub 32))
  (entropy list))
