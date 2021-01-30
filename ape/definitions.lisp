(in-package :easy-audio.ape)

(define-condition ape-error (error simple-condition) ()
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "Ape error: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "General APE error"))

(define-condition apev2-tag-error (ape-error) ()
  (:documentation "APEv2 tag error"))

(defstruct (metadata-header (:conc-name nil))
  (padding1           0 :type (ub 16))
  (desc-len           0 :type (ub 32))
  (header-len         0 :type (ub 32))
  (seektable-len      0 :type (ub 32))
  (waveheader-len     0 :type (ub 32))
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
  (samplerate         0 :type (ub 32)))

(defgeneric read-metadata-header (reader ape-version)
  (:documentation "Read and fill METADATA-HEADER structure"))
