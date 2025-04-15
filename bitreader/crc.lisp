(in-package :easy-audio.bitreader)

;; Contains CRC functions used by easy-audio
;; Flac
(declaim (type (sa-ub 16) +crc-table-0-8005+))
(define-constant +crc-table-0-8005+
    (make-array 256
                :element-type '(ub 16)
                :initial-contents
                '#.(flet ((crc-for-byte (byte)
                            (labels ((%go (crc n)
                                       (if (zerop n) crc
                                           (%go
                                            (logand (if (zerop (logand #x8000 crc))
                                                        (ash crc 1)
                                                        (logxor #x8005 (ash crc 1)))
                                                    #xffff)
                                            (1- n)))))
                              (%go (ash byte 8) 8))))
                     (loop for i below 256 collect (crc-for-byte i))))
  :documentation "Precalculated CRC-16 table, starting with 0,
polynomial generator #x8005.  Used for FLAC."
  :test #'equalp)

(serapeum:-> crc-0-8005 ((sa-ub 8) (ub 16)
                         &key (:start t) (:end t))
             (values (ub 16) &optional))
(defun crc-0-8005 (array accum &key (start 0) end)
  "CRC checksum used in FLAC frames"
  (declare (optimize (speed 3)))
  (flet ((accumulate-crc (crc x)
                         (declare (type (ub 16) crc)
                                  (type (ub 8) x))
                         (logand #xffff
                                 (logxor (ash crc 8)
                                         (aref +crc-table-0-8005+
                                               (logxor x (ash crc -8)))))))
    (reduce #'accumulate-crc array :initial-value accum :start start :end end)))

;; OGG
(declaim (type (sa-ub 32) +crc-table-0-04c11db7+))
(define-constant +crc-table-0-04c11db7+
    (make-array 256
                :element-type '(ub 32)
                :initial-contents
                '#.(flet ((crc-for-byte (byte)
                            (labels ((%go (crc n)
                                       (if (zerop n) crc
                                           (%go
                                            (logand (if (zerop (logand #x80000000 crc))
                                                        (ash crc 1)
                                                        (logxor #x04c11db7 (ash crc 1)))
                                                    #xffffffff)
                                            (1- n)))))
                              (%go (ash byte 24) 8))))
                     (loop for i below 256 collect (crc-for-byte i))))
  :documentation "Precalculated CRC-32 table, starting with 0,
polynomial generator #x04c11db7. Used for OGG container."
  :test #'equalp)

(serapeum:-> crc-0-04c11db7 ((sa-ub 8) (ub 32)
                             &key (:start t) (:end t))
             (values (ub 32) &optional))
(defun crc-0-04c11db7 (array accum &key (start 0) end)
  "CRC checksum used in OGG container"
  (declare (optimize (speed 3)))
  (flet ((accumulate-crc (crc x)
                         (declare (type (ub 32) crc)
                                  (type (ub 8) x))
                         (logand #xffffffff
                                 (logxor (ash crc 8)
                                         (aref +crc-table-0-04c11db7+
                                               (logxor x (logand #xff (ash crc -24))))))))
    (reduce #'accumulate-crc array :initial-value accum :start start :end end)))
