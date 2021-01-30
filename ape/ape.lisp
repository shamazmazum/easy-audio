(in-package :easy-audio.ape)

(defconstant +ape-id+ #x4d414320
  "4-byte value identifying APE file")

(defconstant +ape-min-version+ 3800
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

(declaim (inline read-metadata-header/3980))
(defreader (read-metadata-header/3980 "Read header (version >= 3980)")
    ((make-metadata-header) header)
    (padding1           (:octets 2)
                        :endianness :little)
    (desc-len           (:octets 4)
                        :endianness :little)
    (header-len         (:octets 4)
                        :endianness :little)
    (seektable-len      (:octets 4)
                        :endianness :little)
    (waveheader-len     (:octets 4)
                        :endianness :little)
    (audiodata-len      (:octets 4)
                        :endianness :little)
    (audiodata-len-high (:octets 4)
                        :endianness :little)
    (wavtail-len        (:octets 4)
                        :endianness :little)
    (header-md5         (:octet-vector (make-array 16
                                                   :element-type '(ub 8))))
    (nil                (:octet-vector (make-array (- (desc-len header) 52)
                                                   :element-type '(ub 8))))
    ;; Header data
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
                        :endianness :little))

(defmethod read-metadata-header (reader (ape-version (eql 3980)))
  (declare (ignore ape-version))
  (read-metadata-header/3980 reader))

(defun read-metadata (reader)
  (let ((version (read-octets 2 reader :endianness :little)))
    (if (or (< version +ape-min-version+)
            (> version +ape-max-version+))
        (error 'ape-error
               :format-control "Unsupported APE version ~d"
               :format-arguments (list version)))
    (read-metadata-header reader (metadata-promote-version version))))
