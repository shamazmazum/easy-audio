(in-package :easy-audio.wv)

;; Metadata body readers
(defmethod read-metadata-body ((metadata metadata) reader)
  (let ((data (make-array (list (metadata-actual-size metadata))
                          :element-type '(ub 8))))
    (setf (metadata-data metadata)
          (read-octet-vector data reader)))
  (warn 'unknown-metadata :metadata metadata)
  metadata)

(defmethod read-metadata-body :after ((metadata metadata) reader)
  (unless (= (metadata-size metadata)
             (metadata-actual-size metadata))
      (read-octet reader)))

(defmethod read-metadata-body :before ((metadata metadata-decorr) reader)
  (let ((decorr-passes (block-decorr-passes *current-block*))
        (data-length (metadata-actual-size metadata)))
    (setf (metadata-decorr-passes metadata)
          (if decorr-passes decorr-passes
              (setf (block-decorr-passes *current-block*)
                    (loop repeat data-length collect (make-decorr-pass)))))))

(defmethod read-metadata-body ((metadata metadata-decorr-terms) reader)
  ;; NB: in wavpack lib data is filled in backward order. We do not do this.
  (let ((data-length (metadata-actual-size metadata)))
    (when (or (>  data-length 16)
              (/= data-length (length (metadata-decorr-passes metadata))))
      (error 'block-error
             :format-control "Size of metadata sub-block ~a is incorrect"
             :format-arguments (list metadata))))

  (loop for decorr-pass in (metadata-decorr-passes metadata) do
        (let* ((octet (read-octet reader))
               (term (- (logand octet #x1f) 5))
               (delta (logand (ash octet -5) #x7)))
          (when (or (= term 0)
                    (< term -3)
                    (and (> term 8) (< term 17))
                    (> term 18))
            (error 'block-error
                   :format-control "Invalid term in metadata sub-block ~a"
                   :format-arguments (list metadata)))
          (setf (decorr-pass-term decorr-pass) term
                (decorr-pass-delta decorr-pass) delta)))
  metadata)

(defmethod read-metadata-body ((metadata metadata-decorr-weights) reader)
  (declare (optimize (speed 3)))
  (let* ((data-size (metadata-actual-size metadata))
         (channels (block-data-channels *current-block*))
         (term-number (floor data-size channels))
         (decorr-passes (metadata-decorr-passes metadata)))
    (declare (type (ub 24) data-size)
             (type list decorr-passes))
    (when (> term-number (length decorr-passes))
      (error 'block-error
             :format-control "Size of metadata sub-block ~a is too big"
             :format-arguments (list metadata)))
    (flet ((restore-weight (weight)
             (if (< weight #x80)
                 (let ((val (ash weight 3)))
                   (+ val (ash (+ val 64) -7)))
                 (- (ash (- #x100 weight) 3)))))
      (loop for decorr-pass in decorr-passes
            repeat term-number do
            (loop for channel below channels do
                  (setf (aref (decorr-pass-weight decorr-pass) channel)
                        (restore-weight (read-octet reader)))))))
  metadata)

(defmethod read-metadata-body ((metadata metadata-decorr-samples) reader)
  (when (and (= (block-version *current-block*) #x402)
             (flag-set-p *current-block* +flags-hybrid-mode+))
    (error 'block-error :format-control "Hybrid encoding is not supported"))

  (let ((first-pass (first (metadata-decorr-passes metadata))))
    (when first-pass
      (let ((channels (block-data-channels *current-block*))
            (first-term (decorr-pass-term first-pass))
            (bytes-read 0))
        (when (and (< first-term 0)
                   (= channels 1))
          (error 'block-error :format-control "decorrelation term < 0 and mono audio"))
        (let ((decorr-samples
               (cond
                 ((> first-term 8)
                  (let ((decorr-samples
                         (loop repeat channels collect
                               (make-array (list 2) :element-type '(sb 32)))))
                    (loop for samples in decorr-samples do
                          (setf (aref samples 0)
                                (exp2s (read-octets 2 reader :endianness :little))
                                (aref samples 1)
                                (exp2s (read-octets 2 reader :endianness :little)))
                          (incf bytes-read 4))
                    decorr-samples))
                 ((< first-term 0)
                  (loop for i below channels do (incf bytes-read 2) collect
                        (exp2s (read-octets 2 reader :endianness :little))))
                 (t
                  (let ((decorr-samples
                         (loop repeat channels collect
                               (make-array (list first-term) :element-type '(sb 32)))))
                    (loop for i below first-term do
                          (loop for samples in decorr-samples do
                                (setf (aref samples i)
                                      (exp2s (read-octets 2 reader :endianness :little)))
                                (incf bytes-read 2)))
                    decorr-samples)))))

          (unless (= bytes-read (metadata-actual-size metadata))
            (error 'block-error
                   :format-control "Size of metadata sub-block ~a is invalid"
                   :format-arguments (list metadata)))
          (setf (metadata-decorr-samples metadata) decorr-samples
                (block-decorr-samples *current-block*) decorr-samples)))))
  metadata)

(defmethod read-metadata-body ((metadata metadata-entropy) reader)
  (let ((data-size (metadata-actual-size metadata))
        (channels (block-data-channels *current-block*)))
    (unless (= data-size (* 6 channels))
      (error 'block-error
             :format-control "Size of metadata sub-block ~a is invalid"
             :format-arguments (list metadata)))
    (setf (metadata-entropy-median metadata)
          (loop repeat channels collect
                (let ((median (make-array 3 :element-type '(ub 32))))
                  (loop for i below 3 do
                        (setf (aref median i)
                              (exp2s (read-octets 2 reader :endianness :little))))
                  median))
          (block-entropy-median *current-block*)
          (metadata-entropy-median metadata)))
  metadata)

(defmethod read-metadata-body ((metadata metadata-int32-info) reader)
  (let ((data-size (metadata-actual-size metadata)))
    (unless (= data-size 4)
      (error 'block-error
             :format-control "Size of metadata sub-block ~a is invalid"
             :format-arguments (list metadata))))
  (setf (metadata-sent-bits metadata) (read-octet reader)
        (metadata-zeros metadata) (read-octet reader)
        (metadata-ones metadata) (read-octet reader)
        (metadata-dups metadata) (read-octet reader)
        (block-int32-info *current-block*) metadata)
  metadata)

(defmethod read-metadata-body ((metadata metadata-wvx-bits) reader)
  (let ((int32-info (find 'metadata-int32-info (block-metadata *current-block*)
                          :key #'type-of)))
    (unless int32-info
      (error 'block-error :format-control "No int32-info prior to wvx-bitstream"))
    (let* ((block-samples (block-block-samples *current-block*))
           (channels (block-data-channels *current-block*))
           (sent-bits (metadata-sent-bits int32-info))
           (size (metadata-actual-size metadata))
           (bits-wasted (- (* 8 size) (* channels block-samples sent-bits))))
      (when (< bits-wasted 0)
        (error 'block-error :format-control "wvx-bitstream is too small"))
      (let ((bits (loop repeat channels collect
                        (make-array (list block-samples) :element-type '(sb 32)))))
        (loop for i below block-samples do
              (loop for j below channels do
                    (setf (aref (nth j bits) i)
                          (read-bits (metadata-sent-bits int32-info) reader
                                     :endianness :little))))
        (setf (metadata-bits metadata) bits
              ;; Make a copy for easy access
              (block-wvx-bits *current-block*) bits))
      (read-bits bits-wasted reader))) ;; Why there are wasted bits anyway?
  metadata)

(defmethod read-metadata-body :around ((metadata metadata-ignorable) reader)
  (declare (ignore reader))
  (handler-bind
      ((unknown-metadata #'muffle-warning))
    (call-next-method)))

;; We do not have primary reader method for residual block, as we cannot do
;; anything with it at the moment when READ-METADATA-BODY is called. We
;; only create additional residual reader in the following method.
(defmethod read-metadata-body :after ((metadata metadata-residual) reader)
  (declare (ignore reader))
  (setf (metadata-residual-reader metadata)
        (make-reader-from-buffer (metadata-data metadata))))

;; Metadata reader
(defreader (%read-metadata) ((make-instance 'metadata) metadata)
  (metadata-id (:octets 1))
  (metadata-size (:octets (if (all-bits-set-p (metadata-id metadata)
                                              +meta-id-large-block+)
                              3 1))
                 :endianness :little
                 :function (lambda (x) (* x 2))))

(defun read-metadata (reader)
  (let ((metadata (%read-metadata reader)))
    (setf (metadata-actual-size metadata)
          (let ((size (metadata-size metadata)))
            (if (all-bits-set-p (metadata-id metadata) +meta-id-data-length--1+)
                (1- size) size)))
    (let* ((id (metadata-id metadata))
           (useless (all-bits-set-p id +meta-id-useless-for-decoder+))
           (useful (not useless))
           (function (logand id (logior +meta-id-useless-for-decoder+
                                        +meta-id-function+))))
      (change-class
       metadata
       (cond
         ((and useful  (= function +meta-id-decorr-terms+)) 'metadata-decorr-terms)
         ((and useful  (= function +meta-id-decorr-weights+)) 'metadata-decorr-weights)
         ((and useful  (= function +meta-id-decorr-samples+)) 'metadata-decorr-samples)
         ((and useful  (= function +meta-id-entropy-vars+)) 'metadata-entropy)
         ((and useful  (= function +meta-id-wv-bitstream+)) 'metadata-wv-residual)
         ((and useful  (= function +meta-id-int32-info+)) 'metadata-int32-info)
         ((and useful  (= function +meta-id-wvx-bitstream+)) 'metadata-wvx-bits)
         ((and useless (= function +meta-id-riff-header+)) 'metadata-riff-header)
         ((and useless (= function +meta-id-riff-trailer+)) 'metadata-riff-trailer)
         (t 'metadata))))
    (read-metadata-body metadata reader)))
