(in-package :easy-audio.ape)

(defconstant +ape-id+ #x4d414320
  "4-byte value identifying APE file")

(defconstant +ape-min-version+ 3990
  "Minimal supported APE version")

(defconstant +ape-max-version+ 3990
  "Maximal supported APE version")

(defun open-ape (stream)
  (let ((reader (make-reader :stream stream)))
    (if (/= (read-octets 4 reader) +ape-id+)
        (error 'ape-error :format-control "Not an APE stream"))
    reader))

(defun metadata-promote-version (version)
  "Promote version to one suitable for call to READ-METADATA-HEADER"
  (cond
    ((< version 3980) 0)
    (t 3980)))

(defun bittable-promote-version (version)
  "Promote version to one suitable for call to READ-BITTABLE"
  (cond
    ((< version 3810) 0)
    (t 3810)))

(declaim (inline read-metadata-header/3980))
(defreader (read-metadata-header/3980 "Read header (version >= 3980)")
    ((make-metadata) metadata)
    (metadata-padding1           (:octets 2)
                                 :endianness :little)
    (metadata-desc-len           (:octets 4)
                                 :endianness :little)
    (metadata-header-len         (:octets 4)
                                 :endianness :little)
    (metadata-seektable-len      (:octets 4)
                                 :endianness :little)
    (metadata-wavheader-len      (:octets 4)
                                 :endianness :little)
    (metadata-audiodata-len      (:octets 4)
                                 :endianness :little)
    (metadata-audiodata-len-high (:octets 4)
                                 :endianness :little)
    (metadata-wavtail-len        (:octets 4)
                                 :endianness :little)
    (metadata-header-md5         (:octet-vector (make-array 16
                                                            :element-type '(ub 8))))
    (nil                         (:octet-vector (make-array (- (metadata-desc-len metadata) 52)
                                                            :element-type '(ub 8))))
    ;; Header data
    (metadata-compression-type   (:octets 2)
                                 :endianness :little)
    (metadata-format-flags       (:octets 2)
                                 :endianness :little)
    (metadata-blocks-per-frame   (:octets 4)
                                 :endianness :little)
    (metadata-final-frame-blocks (:octets 4)
                                 :endianness :little)
    (metadata-total-frames       (:octets 4)
                                 :endianness :little)
    (metadata-bps                (:octets 2)
                                 :endianness :little)
    (metadata-channels           (:octets 2)
                                 :endianness :little)
    (metadata-samplerate         (:octets 4)
                                 :endianness :little))

(defmethod read-metadata-header (reader (ape-version (eql 3980)))
  (declare (ignore ape-version))
  (read-metadata-header/3980 reader))

(defmethod read-bittable (reader (ape-version (eql 3810)))
  ;; No bittable in versions >= 3810
  nil)

(defun read-metadata (reader)
  (let ((version (read-octets 2 reader :endianness :little)))
    (if (or (< version +ape-min-version+)
            (> version +ape-max-version+))
        (error 'ape-error
               :format-control "Unsupported APE version ~d"
               :format-arguments (list version)))
    (let* ((metadata (read-metadata-header
                      reader (metadata-promote-version version)))
           (seektable (make-array (metadata-total-frames metadata)
                                  :element-type '(ub 32))))
      ;; Set auxiliary fields
      (setf (metadata-version metadata) version
            (metadata-total-samples metadata)
            (+ (metadata-final-frame-blocks metadata)
               (* (metadata-blocks-per-frame metadata)
                  (1- (metadata-total-frames metadata))))
            (metadata-seektable metadata) seektable)
      ;; A bit of sanity checks
      (let ((expected-len (ash (metadata-total-frames metadata) 2)))
        (if (/= (metadata-seektable-len metadata) expected-len)
            (error 'ape-error
                   :format-control "Unexpected seektable length (expected ~d, got ~d)"
                   :format-arguments (list expected-len (metadata-seektable-len metadata)))))
      ;; COMPRESSION-TYPE must be multiple of 1000
      (if (not (zerop (rem (metadata-compression-type metadata) 1000)))
          (error 'ape-error
                 :format-control "Compression type is not multiple of 1000: ~d"
                 :format-arguments (list (metadata-compression-type metadata))))
      ;; Read seektable
      (dotimes (i (length seektable))
        (setf (aref seektable i)
              (read-octets 4 reader :endianness :little)))
      ;; Read bittable (if any)
      (setf (metadata-bittable metadata)
            (read-bittable reader (bittable-promote-version version)))
      metadata)))

(defun frame-start (metadata n)
  (let* ((seektable (metadata-seektable metadata))
         (start (aref seektable n))
         (skip (logand (- start (aref seektable 0)) 3)))
    (values (- start skip)
            skip)))
