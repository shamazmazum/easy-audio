(in-package :easy-audio.ape)

(defconstant +mono-silence+ 1)
(defconstant +stereo-silence+ 3)
(defconstant +pseudo-stereo+ 4)

(deftype octet-reader ()
  '(function () (values (ub 8) &optional)))

(define-constant +stereo-entropy-versions+
    '(3990 3930 3900 3860 0)
  :test #'equalp)

(define-constant +mono-entropy-versions+
    '(3990 3900 3860 0)
  :test #'equalp)

(define-constant +counts-3980+
    (make-array 22
                :element-type '(ub 16)
                :initial-contents '(0 19578 36160 48417 56323 60899 63265 64435
                                    64971 65232 65351 65416 65447 65466 65476 65482
                                    65485 65488 65490 65491 65492 65493))
  :test #'equalp)

;; TODO: calculate this from *counts*
(define-constant +counts-diff-3980+
    (make-array 21
                :element-type '(ub 16)
                :initial-contents (map 'list #'-
                                       (subseq +counts-3980+ 1) +counts-3980+))
  :test #'equalp)

(sera:-> make-swapped-reader (reader)
         (values octet-reader &optional))
(defun make-swapped-reader (reader)
  "This function generates a closure that read octets in strange
reversed order observed in ffmpeg (as if they are part of
little-endian values)."
  (declare (optimize (speed 3)))
  (let (octets)
    (lambda ()
      (when (null octets)
        (setq octets
              (reverse (loop repeat 4 collect (read-octet reader)))))
      (prog1
          (car octets)
        (setq octets (cdr octets))))))

(sera:-> read-32 (octet-reader)
         (values (ub 32) &optional))
(defun read-32 (reader)
  (declare (optimize (speed 3)))
  (logior
   (ash (funcall reader) 24)
   (ash (funcall reader) 16)
   (ash (funcall reader)  8)
   (ash (funcall reader)  0)))

(declaim (inline frame-start))
(defun frame-start (metadata n)
  (let* ((seektable (metadata-seektable metadata))
         (start (aref seektable n))
         (skip (logand (- start (aref seektable 0)) 3)))
    (values (- start skip)
            skip)))

(sera:-> range-dec-normalize (octet-reader range-coder)
         (values &optional))
(defun range-dec-normalize (reader range-coder)
  (declare (optimize (speed 3)))
  (with-accessors ((buffer range-coder-buffer)
                   (low    range-coder-low)
                   (range  range-coder-range))
      range-coder
    ;; Overflows can happen here, so values must be bringed back to
    ;; (ub 32) type.
    (loop while (<= range +bottom-value+) do
      (setf buffer (+ (logand (ash buffer 8)
                              #xffffffff)
                      (funcall reader))
            low (logior (logand (ash low 8)
                                #xffffffff)
                        (logand (ash buffer -1) #xff))
            range (logand (ash range 8)
                          #xffffffff))))
  (values))

(sera:-> range-decode-culshift (octet-reader range-coder (integer 0 32))
         (values (ub 16) &optional))
(defun range-decode-culshift (reader range-coder shift)
  (declare (optimize (speed 3)))
  (range-dec-normalize reader range-coder)
  (with-accessors ((help  range-coder-help)
                   (low   range-coder-low)
                   (range range-coder-range))
      range-coder
    (setf help (ash range (- shift)))
    (nth-value 0 (floor low help))))

(sera:-> range-decode-culfreq (octet-reader range-coder (ub 16))
         (values (ub 16) &optional))
(defun range-decode-culfreq (reader range-coder tot-f)
  (declare (optimize (speed 3)))
  (range-dec-normalize reader range-coder)
  (with-accessors ((help  range-coder-help)
                   (low   range-coder-low)
                   (range range-coder-range))
      range-coder
    (setf help (floor range tot-f))
    (nth-value 0 (floor low help))))

(sera:-> range-decode-update (range-coder (ub 16) (ub 16))
         (values &optional))
(defun range-decode-update (range-coder sy-f lt-f)
  (declare (optimize (speed 3)))
  (let ((help (range-coder-help range-coder)))
    (decf (range-coder-low range-coder)
          (* help lt-f))
    (setf (range-coder-range range-coder)
          (* help sy-f)))
  (values))

(sera:-> range-get-symbol (octet-reader range-coder (sa-ub 16) (sa-ub 16))
         (values (ub 16) &optional))
(defun range-get-symbol (reader range-coder counts counts-diff)
  (declare (optimize (speed 3)))
  (let ((cf (range-decode-culshift reader range-coder 16)))
    (cond
      ((> cf 65492)
       (range-decode-update range-coder 1 cf)
       (- cf (- 65535 63)))
      (t
       ;; Position never returns NIL here because cf is less than 65493
       (let ((symbol (max 0 (1- (position cf counts :test #'<)))))
         (range-decode-update
          range-coder
          (aref counts-diff symbol)
          (aref counts symbol))
         symbol)))))

(sera:-> range-decode-bits (octet-reader range-coder (integer 0 16))
         (values (ub 16) &optional))
(defun range-decode-bits (reader range-coder n)
  (declare (optimize (speed 3)))
  (let ((sym (range-decode-culshift reader range-coder n)))
    (range-decode-update range-coder 1 sym)
    sym))

(sera:-> update-rice (rice-state (ub 32))
         (values &optional))
(defun update-rice (rice-state x)
  (declare (optimize (speed 3)))
  (let* ((k     (rice-state-k    rice-state))
         (%ksum (rice-state-ksum rice-state))
         (lim (if (zerop k) 0 (ash 1 (+ 4 k))))
         (ksum (+ %ksum (ash (1+ x) -1)
                  (- (ash (+ %ksum 16) -5)))))
      (cond
        ((< ksum lim)
         (decf (rice-state-k rice-state)))
        ((and (< k 24)
              (>= ksum (ash 1 (+ k 5))))
         (incf (rice-state-k rice-state))))
      (setf (rice-state-ksum rice-state) ksum))
  (values))

(sera:-> entropy-decode/3990 (octet-reader frame)
         (values frame &optional))
(defun entropy-decode/3990 (reader frame)
  (declare (optimize (speed 3)))
  (let ((entropy (frame-entropy frame))
        (samples (frame-samples frame))
        (range-coder (make-range-coder
                      :buffer (frame-buffer frame)
                      :low (ash (frame-buffer frame)
                                (- +extra-bits+ 8)))))
    (flet ((read-value (rice-state)
             (let* ((%overflow (range-get-symbol
                                reader range-coder
                                +counts-3980+ +counts-diff-3980+))
                    (overflow (if (= %overflow 63)
                                  (let ((high (range-decode-bits reader range-coder 16))
                                        (low (range-decode-bits reader range-coder 16)))
                                    (logior (ash high 16) low))
                                  %overflow))
                    (pivot (max 1 (ash (rice-state-ksum rice-state) -5)))
                    (base (cond
                            ((< pivot #x10000)
                             (let ((base (range-decode-culfreq reader range-coder pivot)))
                               (range-decode-update range-coder 1 base)
                               base))
                            (t
                             (let* ((bbits (max 0 (- (integer-length pivot) 16)))
                                    (base-hi
                                      (let ((tmp (range-decode-culfreq
                                                  reader range-coder
                                                  (1+ (ash pivot (- bbits))))))
                                        (range-decode-update range-coder 1 tmp)
                                        tmp))
                                    (base-low
                                      (let ((tmp (range-decode-culfreq
                                                  reader range-coder
                                                  (ash 1 bbits))))
                                        (range-decode-update range-coder 1 tmp)
                                        tmp)))
                               (+ base-low (ash base-hi bbits))))))
                    (x (+ base (* overflow pivot))))
               (update-rice rice-state x)
               (1+ (logxor (ash x -1)
                           (1- (logand x 1)))))))

      (let ((rice-states (loop repeat (length entropy)
                               collect (make-rice-state))))
        (dotimes (i samples)
          (mapc
           (lambda (entropy rice-state)
             (declare (type (sa-sb 32) entropy))
             (setf (aref entropy i)
                   (read-value rice-state)))
           entropy rice-states)))))
  frame)

(sera:-> entropy-decode (octet-reader frame (member :mono :stereo))
         (values frame &optional))
(declaim (inline entropy-decode))
(defun entropy-decode (reader frame channels)
  (let ((version
         (find (frame-version frame)
               (ecase channels
                 (:mono   +mono-entropy-versions+)
                 (:stereo +stereo-entropy-versions+))
               :test #'>=)))
    (case version
      (3990 (entropy-decode/3990 reader frame))
      (t (error 'ape-error
                :format-control "Unsupported frame version ~d"
                :format-arguments (list (frame-version frame)))))))

(sera:-> read-stereo-entropy (octet-reader frame)
         (values frame &optional))
(defun read-stereo-entropy (reader frame)
  (let ((flags (frame-flags frame)))
    (if (all-bits-set-p flags +stereo-silence+) frame
        (entropy-decode
         reader frame :stereo))))

(sera:-> read-mono-entropy (octet-reader frame)
         (values frame &optional))
(defun read-mono-entropy (reader frame)
  (let ((flags (frame-flags frame)))
    ;; ffmpeg checks stereo silence here. Can 0x02 be set here?
    (if (some-bits-set-p flags +stereo-silence+) frame
      (entropy-decode
       reader frame :mono))))

(sera:-> read-crc-and-flags (octet-reader (ub 16))
         (values (ub 32) (ub 32) &optional))
(declaim (inline read-crc-and-flags))
(defun read-crc-and-flags (reader version)
  ;; What's the difference between bytestream_get_[b|l]e32() and
  ;; get_bits_long()?
  (let ((crc (read-32 reader)))
    (if (and (> version 3820)
             (not (zerop (ldb (byte 1 31) crc))))
        (values (logand crc (1- #x80000000))
                (read-32 reader))
        (values crc 0))))

(sera:-> %read-frame (octet-reader metadata &key (:last-frame-p boolean))
         (values frame &optional))
(defun %read-frame (reader metadata &key last-frame-p)
  (let ((version (metadata-version metadata))
        (bps (metadata-bps metadata))
        ;; Calculate compression level
        (fset (1- (floor
                   (metadata-compression-type metadata)
                   1000))))
    ;; Read CRC and frame flags
    (multiple-value-bind (crc flags)
        (read-crc-and-flags reader version)
      (let* ((buffer
              (cond
                ((>= version 3900)
                 ;; Drop the first 8 bits
                 (funcall reader)
                 (funcall reader))
                (t 0)))
             ;; Initialize output buffer
             (samples (if last-frame-p
                          (metadata-final-frame-blocks metadata)
                          (metadata-blocks-per-frame metadata)))
             (pseudo-stereo-p (some-bits-set-p flags +pseudo-stereo+))
             (entropy
              (loop repeat (if pseudo-stereo-p 1 (metadata-channels metadata))
                    collect
                    (make-array samples
                                :element-type '(signed-byte 32)
                                :initial-element 0)))
             (frame (frame version fset samples bps flags buffer crc entropy)))
        ;; Read entropy
        (if (or pseudo-stereo-p (= (metadata-channels metadata) 1))
            (read-mono-entropy reader frame)
            (read-stereo-entropy reader frame))))))

(sera:-> read-frame (reader metadata non-negative-fixnum)
         (values frame &optional))
(defun read-frame (reader metadata n)
  "Read the @c(n)-th audio frame from @c(reader). @c(metadata) is the
metadata structure for this audio file."
  (multiple-value-bind (start skip)
      (frame-start metadata n)
    ;; Seek to the start of a frame
    (reader-position reader start)
    ;; Make that peculiar swapped-bytes reader needed to read a frame
    (let ((swapped-reader (make-swapped-reader reader)))
      ;; Skip some bytes from the beginning
      (loop repeat skip do (funcall swapped-reader))
      ;; Read a frame
      (%read-frame
       swapped-reader metadata
       :last-frame-p (= n (1- (metadata-total-frames metadata)))))))
