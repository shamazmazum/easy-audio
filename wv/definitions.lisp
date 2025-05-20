(in-package :easy-audio.wv)

;; TODO: Figure out why I need this
(deftype maybe (type) `(or null ,type))

;; Conditions
(define-condition wavpack-condition (simple-condition) ()
  (:report (lambda (c s)
	     (apply #'format s
                    (concatenate 'string "WavPack: "
                                 (simple-condition-format-control c))
                    (simple-condition-format-arguments c))))
  (:documentation "General (unspecified) WavPack condition"))

(define-condition wavpack-error (wavpack-condition error) ()
  (:documentation "General WavPack error"))
(define-condition wavpack-warning (wavpack-condition warning) ()
  (:documentation "General WavPack warning"))

(define-condition block-error (wavpack-error) () ; A large part of currently generated conditions is of this type
  (:documentation "Error associated with block reader/decoder error"))

(define-condition lost-sync (block-error) ()
  (:documentation "Error signaled when sync is obviously lost
                   (e.g. first 4 bytes in block are not Wavpack ID).
                   Errors signalled when reading a block which cause
                   loss of sync are not of this type, but of @c(block-error).
                   Useful for skipping garbage in audio files"))

(define-condition unknown-metadata (wavpack-warning)
  ((metadata :reader unknown-metadata
             :initarg :metadata
             :documentation "Metadata object"))
  (:report (lambda (c s)
	     (format s "WavPack: cannot understand metadata (id=~d)"
                     (metadata-id (unknown-metadata c)))))
  (:documentation "The reader does not know how to read metadata"))
;; --------
;; Metadata
;; --------
(defclass metadata ()
  ((id          :accessor metadata-id
                :type (ub 8)
                :documentation "An ID number designating this metadata")
   (size        :accessor metadata-size
                :type (ub 24)
                :documentation "Size of this metadata on disk in bytes")
   (actual-size :accessor metadata-actual-size
                :type (ub 24)
                :documentation "Actual size of metadata. Can be size or size-1")
   (data        :accessor metadata-data
                :documentation "Raw metadata. Usually this slot is not bound"))
  (:documentation "General class for storing metadata. If instantiated, the metadata reader
                   will only read raw metadata to data slot"))

(defclass metadata-ignorable (metadata) ()
  (:documentation "Known metadata block for which we have no special primary reader method.
                   Not to be instantiated"))

(defclass metadata-decorr (metadata)
  ((decorr-passes :accessor metadata-decorr-passes))
  (:documentation "General class for everything (de)correlation-related.
                   This class is not instantiated"))

(defclass metadata-decorr-terms (metadata-decorr) ())
(defclass metadata-decorr-weights (metadata-decorr) ())
(defclass metadata-decorr-samples (metadata-decorr)
  ((decorr-samples :accessor metadata-decorr-samples)))

(defclass metadata-entropy (metadata)
  ((entropy-median :accessor metadata-entropy-median)))

;; We can do nothing with residual metadata block
;; in the moment when READ-METADATA-BODY is called,
;; so set it to -IGNORABLE
(defclass metadata-residual (metadata-ignorable)
  ((reader :type reader
           :accessor metadata-residual-reader)))
(defclass metadata-wv-residual (metadata-residual) ())

(defclass metadata-wvx-bits (metadata)
  ((bits :accessor metadata-bits))
  (:documentation "This block may be present when sample size is > 24"))

(defclass metadata-riff-header (metadata-ignorable) ()
  (:documentation "Contents the original RIFF header in DATA slot"))

(defclass metadata-riff-trailer (metadata-ignorable) ()
  (:documentation "Contents the original RIFF trailer in DATA slot"))

(defclass metadata-int32-info (metadata)
  ((sent-bits  :accessor metadata-sent-bits
               :type (ub 8))
   (zeros      :accessor metadata-zeros
               :type (ub 8))
   (ones       :accessor metadata-ones
               :type (ub 8))
   (dups       :accessor metadata-dups
               :type (ub 8)))
  (:documentation "This block is present when sample size is > 24"))

(defgeneric read-metadata-body (metadata reader))

;; Metadata id masks
(defconstant +meta-id-function+            #x1f)
(defconstant +meta-id-useless-for-decoder+ #x20)
(defconstant +meta-id-data-length--1+      #x40)
(defconstant +meta-id-large-block+         #x80)

;; Assigned metadata ids
(defconstant +meta-id-dummy+               #x0)
(defconstant +meta-id-decorr-terms+        #x2)
(defconstant +meta-id-decorr-weights+      #x3)
(defconstant +meta-id-decorr-samples+      #x4)
(defconstant +meta-id-entropy-vars+        #x5)
(defconstant +meta-id-hybrid-profile+      #x6)
(defconstant +meta-id-shaping-weights+     #x7)
(defconstant +meta-id-float-info+          #x8)
(defconstant +meta-id-int32-info+          #x9)
(defconstant +meta-id-wv-bitstream+        #xa)
(defconstant +meta-id-wvc-bitstream+       #xb)
(defconstant +meta-id-wvx-bitstream+       #xc)
(defconstant +meta-id-channel-info+        #xd)

;; Metadata ids of metadata needless for decoder
(defconstant +meta-id-riff-header+         #x21)
(defconstant +meta-id-riff-trailer+        #x22)
(defconstant +meta-id-config-block+        #x25)
(defconstant +meta-id-md5-checksum+        #x26)
(defconstant +meta-id-samplerate+          #x27)

;; --------------
;; WavPack blocks
;; --------------
(defconstant +wv-id+ #x7776706b)
(defconstant +wv-id/first-octet+ #x77)

(defstruct decorr-pass
  (term      0 :type (sb 32))
  (delta     0 :type (sb 32))
  (weight    (make-array 2 :element-type '(sb 32) :initial-element 0)
               :type (sa-sb 32))
  #|aweight sum|#)

(defstruct (wv-block (:conc-name block-)
                     (:print-function
                      (lambda (struct stream k)
                        (declare (ignore k))
                        (print-unreadable-object (struct stream
                                                  :type t :identity t)
                          (format stream "samples ~d..~d"
                                  (block-block-index struct)
                                  (+ (block-block-index struct)
                                     (block-block-samples struct)))))))
  "WavPack block structure"
  (id            0 :type (ub 32))
  (size          0 :type (ub 32))

  (version       0 :type (ub 16))
  (track-number  0 :type (ub 8))
  (index-number  0 :type (ub 8))
  (total-samples 0 :type (ub 32))
  (block-index   0 :type (ub 32))
  (block-samples 0 :type (ub 32))
  (flags         0 :type (ub 32))
  (crc           0 :type (ub 32))
  metadata
  ;; These are mostly copy of metadata values for easy access
  decorr-passes
  decorr-samples
  entropy-median
  residual
  int32-info
  wvx-bits)

(sera:defvar-unbound *current-block*
  "Bound to block currently being readed by block reader")

(defconstant +flags-1-byte/sample+     #x00000000)
(defconstant +flags-2-byte/sample+     #x00000001)
(defconstant +flags-3-byte/sample+     #x00000002)
(defconstant +flags-4-byte/sample+     #x00000003)

(defconstant +flags-mono-output+       #x00000004)
(defconstant +flags-hybrid-mode+       #x00000008)
(defconstant +flags-stereo-joint+      #x00000010)
(defconstant +flags-channels-decor+    #x00000020)
(defconstant +flags-noise-shaping+     #x00000040)
(defconstant +flags-data-float+        #x00000080)
(defconstant +flags-shifted-int+       #x00000100)
(defconstant +flags-hybrid-param/bitrate+  #x00000200)
(defconstant +flags-hybrid-noise-balanced+ #x00000400)
(defconstant +flags-initial-block+     #x00000800)
(defconstant +flags-final-block+       #x00001000)
(defconstant +flags-pseudo-stereo+     #x40000000)

(defconstant +flags-left-shift-amount-mask+ #x0003e000)
(defconstant +flags-left-shift-amount-shift+ -13)

(defconstant +flags-max-magnitude-mask+ #x007c0000)
(defconstant +flags-max-magnitude-shift+ -18)

(defconstant +flags-samplerate-mask+ #x07800000)
(defconstant +flags-samplerate-shift+ -23)

;; Bits 27-28 are ignored

(defconstant +flags-use-iir+       #x20000000)
(defconstant +flags-false-stereo+  #x40000000)
(defconstant +flags-reserved-zero+ #x80000000)

(defmacro define-get-value/shift+mask (name-spec)
  "Define value-getting function. This function will accept an integer
   number and extract a value using defined mask and shift values like
   so: (ash (logand number mask) shift).

   NAME-SPEC can be a list (NAME SYM) or just a symbol NAME. NAME is the
   name of the function to be defined. Mask and shift values used must
   have names +FLAGS-NAME-MASK+ and +FLAGS-NAME-SHIFT+ or
   +FLAGS-SYM-MASK+ and +FLAGS-SYM-SHIFT+ if SYM is supplied."
  (let* ((name (if (atom name-spec) name-spec (first name-spec)))
         (sym  (if (atom name-spec) name-spec (second name-spec)))
         (mask  (intern (concatenate 'string "+FLAGS-" (symbol-name sym) "-MASK+")))
         (shift (intern (concatenate 'string "+FLAGS-" (symbol-name sym) "-SHIFT+"))))
    `(defun ,name (wv-block)
       (ash (logand (block-flags wv-block) ,mask) ,shift))))

(define-get-value/shift+mask left-shift-amount)
(define-get-value/shift+mask max-magnitude)
(define-get-value/shift+mask (%block-samplerate samplerate))

(declaim (inline all-bits-set-p))
(defun all-bits-set-p (value bits)
  (= (logand value bits) bits))

(declaim (inline flag-mask-set-p))
(defun flag-mask-set-p (wv-block mask)
  (all-bits-set-p (block-flags wv-block) mask))

(declaim (inline some-bits-set-p))
(defun some-bits-set-p (value bits)
  (not (zerop (logand value bits))))

(declaim (inline flag-set-p))
(defun flag-set-p (wv-block mask)
  (some-bits-set-p (block-flags wv-block) mask))

;; Place these here too
(define-constant +samplerate-list+
    '(6000  8000  9600
      11025 12000 16000
      22050 24000 32000
      44100 48000 64000
      88200 96000 192000)
  :test #'equalp)

(defun block-samplerate (wv-block)
  "Return a sample rate of a block."
  (let ((samplerate (%block-samplerate wv-block)))
    (nth samplerate +samplerate-list+)))

(sera:-> block-bps (wv-block)
         (values (member 8 16 24 32) &optional))
(defun block-bps (wv-block)
  "Return bits per second of a block."
  (cond
    ((flag-mask-set-p wv-block +flags-4-byte/sample+) 32)
    ((flag-mask-set-p wv-block +flags-3-byte/sample+) 24)
    ((flag-mask-set-p wv-block +flags-2-byte/sample+) 16)
    (t 8)))

(sera:-> block-channels (wv-block)
         (values (integer 1 2) &optional))
(defun block-channels (wv-block)
  "Return a number of channels (a block can have 1 or 2 channels)."
  (if (flag-set-p wv-block +flags-mono-output+) 1 2))

(sera:-> block-data-channels (wv-block)
         (values (integer 1 2) &optional))
(defun block-data-channels (wv-block)
  "Return a number of channels (a block can have 1 or 2 channels)."
  (if (flag-set-p wv-block (logior +flags-pseudo-stereo+ +flags-mono-output+)) 1 2))
