(in-package :easy-audio.ape)

(defconstant +mono-silence+ 1)
(defconstant +stereo-silence+ 3)
(defconstant +pseudo-stereo+ 4)
(defconstant +history-size+ 512)
(defconstant +predictor-order+ 8)

(defconstant +ydelaya+ (+ 18 (* +predictor-order+ 4)))
(defconstant +ydelayb+ (+ 18 (* +predictor-order+ 3)))
(defconstant +xdelaya+ (+ 18 (* +predictor-order+ 2)))
(defconstant +xdelayb+ (+ 18 (* +predictor-order+ 1)))

(defconstant +yadaptcoeffsa+ 18)
(defconstant +xadaptcoeffsa+ 14)
(defconstant +yadaptcoeffsb+ 10)
(defconstant +xadaptcoeffsb+  5)

(defun read-crc-and-flags (reader version)
  ;; What's the difference between bytestream_get_[b|l]e32() and
  ;; get_bits_long()?
  (let ((crc (read-bits 32 reader :endianness :little)))
    (if (and (> version 3820)
             (not (zerop (ldb (byte 1 31) crc))))
        (values (logand crc #.(1- #x80000000))
                (read-bits 32 reader :endianness :little))
        (values crc 0))))

(defun read-frame (reader metadata n)
  (reader-position reader (tell-frame-start-and-size metadata n))
  (let ((version (metadata-version metadata))
        (frame (make-frame)))
    (multiple-value-bind (crc flags)
        (read-crc-and-flags reader version)
      (setf (frame-crc frame) crc
            (frame-flags frame) flags))
    ;; FIXME: Initialize rice status
    (when (>= version 3900)
      ;; Drop first 8 bits
      (read-octet reader)
      (setf (frame-buffer frame)
            (read-octet reader)))
    frame))
