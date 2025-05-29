(in-package :easy-audio.ape)

(defconstant +ape-id+ #x4d414320
  "4-byte value identifying APE file")

(defconstant +ape-min-version+ 3990
  "Minimal supported APE version")

(defconstant +ape-max-version+ 3990
  "Maximal supported APE version")

(sera:-> open-ape (stream)
         (values reader &optional))
(defun open-ape (stream)
  "Open ape audio file and return bitreader for further operations"
  (let ((reader (make-reader :stream stream)))
    (when (/= (read-octets 4 reader) +ape-id+)
      (error 'ape-error :format-control "Not an APE stream"))
    reader))

(declaim (inline metadata-promote-version))
(defun metadata-promote-version (version)
  "Promote version to one suitable for call to READ-METADATA-HEADER"
  (cond
    ((< version 3980) 0)
    (t 3980)))

(declaim (inline bittable-promote-version))
(defun bittable-promote-version (version)
  "Promote version to one suitable for call to READ-BITTABLE"
  (cond
    ((< version 3810) 0)
    (t 3810)))

(declaim (inline read-metadata-header/3980))
(defreader* (read-metadata-header/3980 metadata (version) ())
  (padding1           (:octets 2)
                      :endianness :little
                      :ignore t)
  (desc-len           (:octets 4)
                      :endianness :little)
  (header-len         (:octets 4)
                      :endianness :little)
  (seektable-len      (:octets 4)
                      :endianness :little)
  (wavheader-len      (:octets 4)
                      :endianness :little)
  (audiodata-len      (:octets 4)
                      :endianness :little)
  (audiodata-len-high (:octets 4)
                      :endianness :little)
  (wavtail-len        (:octets 4)
                      :endianness :little)
  (header-md5         (:octet-vector 16))
  (padding2           (:octet-vector (- desc-len 52))
                      :ignore t)
  (compression-type   (:octets 2)
                      :endianness :little)
  (format-flags       (:octets 2)
                      :endianness :little)
  (blocks-per-frame   (:octets 4)
                      :endianness :little)
  (final-frame-blocks (:octets 4)
                      :endianness :little)
  (total-frames       (:octets 4)
                      :endianness :little)
  (bps                (:octets 2)
                      :endianness :little)
  (channels           (:octets 2)
                      :endianness :little)
  (samplerate         (:octets 4)
                      :endianness :little)
  (total-samples      (:expr (+ final-frame-blocks
                                (* blocks-per-frame (1- total-frames)))))
  (bittable           (:expr nil))
  (seektable          (:expr (make-array total-frames
                                         :element-type '(ub 32)))))

(defmethod read-metadata-header (reader version (promoted-version (eql 3980)))
  (read-metadata-header/3980 reader version))

(defmethod read-bittable (reader metadata (ape-version (eql 3810)))
  ;; No bittable in versions >= 3810
  (declare (ignore reader))
  metadata)

(sera:-> read-metadata (reader)
         (values metadata &optional))
(defun read-metadata (reader)
  "Read ape metadata using @c(reader) returned by @c(open-ape)"
  (let ((version (read-octets 2 reader :endianness :little)))
    (when (or (< version +ape-min-version+)
              (> version +ape-max-version+))
      (error 'ape-error
             :format-control "Unsupported APE version ~d"
             :format-arguments (list version)))
    (let ((metadata (read-metadata-header
                     reader version (metadata-promote-version version))))
      ;; A bit of sanity checks
      (let ((expected-len (* (metadata-total-frames metadata) 4)))
        (when (/= (metadata-seektable-len metadata) expected-len)
          (error 'ape-error
                 :format-control "Unexpected seektable length (expected ~d, got ~d)"
                 :format-arguments (list expected-len (metadata-seektable-len metadata)))))
      ;; COMPRESSION-TYPE must be multiple of 1000
      (unless (zerop (rem (metadata-compression-type metadata) 1000))
        (error 'ape-error
               :format-control "Compression type is not multiple of 1000: ~d"
               :format-arguments (list (metadata-compression-type metadata))))
      ;; Read seektable
      (let ((seektable (metadata-seektable metadata)))
        (dotimes (i (length seektable))
          (setf (aref seektable i)
                (read-octets 4 reader :endianness :little))))
      ;; Read bittable (if any)
      (read-bittable reader metadata (bittable-promote-version version)))))

(defmacro with-open-ape ((reader name) &body body)
  "Open ape file with the pathname @c(name) and creates @c(reader)
for that file. The file is closed when the control leaves body of this
macro."
  (let ((stream (gensym)))
    `(with-open-file (,stream ,name :element-type '(unsigned-byte 8))
       (let ((,reader (open-ape ,stream)))
         ,@body))))

(sera:-> seconds=>frame-number (metadata non-negative-fixnum)
         (values non-negative-fixnum non-negative-fixnum &optional))
(defun seconds=>frame-number (metadata seconds)
  "Return the number of a frame whose play time is @c(seconds) from
the beginning of file."
  (let ((samplerate (metadata-samplerate metadata))
        (total-samples (metadata-total-samples metadata))
        (frame-size (metadata-blocks-per-frame metadata)))
    (let ((sample-number (* seconds samplerate)))
      (when (>= sample-number total-samples)
        (error 'ape-error
               :format-control "Sample ~d is requested, but maximal value is ~d"
               :format-arguments (list sample-number total-samples)))
      (floor sample-number frame-size))))
