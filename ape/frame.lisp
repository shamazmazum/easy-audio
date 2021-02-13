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

(defparameter *stereo-entropy-versions*
  '#.(reverse '(0 3860 3900 3930 3990)))

(defparameter *mono-entropy-versions*
  '#.(reverse '(0 3860 3900 3990)))

(defun read-crc-and-flags (reader frame)
  ;; What's the difference between bytestream_get_[b|l]e32() and
  ;; get_bits_long()?
  (let ((version (frame-version frame)))
    (with-accessors ((crc frame-crc)
                     (flags frame-flags))
        frame
      (setf crc (read-bits 32 reader :endianness :little))
      (when (and (> version 3820)
                 (not (zerop (ldb (byte 1 31) crc))))
        (setf crc (logand crc #.(1- #x80000000))
              flags (read-bits 32 reader :endianness :little))))))

(defun entropy-promote-version (version &key channels)
  (declare (type (member :mono :stereo) channels))
  (find version
        (ecase channels
          (:mono   *mono-entropy-versions*)
          (:stereo *stereo-entropy-versions*))
        :test #'>=))

(defun read-stereo-frame (reader frame)
  (let ((version (frame-version frame))
        (flags (frame-flags frame)))
    (when (not (zerop (logand flags +stereo-silence+)))
      (return-from read-stereo-frame frame))
    (entropy-decode
     reader frame
     (entropy-promote-version
      version
      :channels :stereo))))

(defun read-mono-frame (reader frame)
  (declare (ignore reader frame))
  t)

(defun read-frame (reader metadata n)
  (reader-position reader (tell-frame-start-and-size metadata n))
  (let* ((version (metadata-version metadata))
         ;; Copy version
         (frame (make-frame :version version)))
    ;; Read CRC and frame flags
    (read-crc-and-flags reader frame)
    ;; FIXME: Initialize rice status
    (when (>= version 3900)
      ;; Drop first 8 bits
      (read-octet reader)
      (setf (frame-buffer frame)
            (read-octet reader)))
    ;; Initialize output buffer
    (let ((samples (if (= n (1- (metadata-total-frames metadata)))
                       (metadata-final-frame-blocks metadata)
                       (metadata-blocks-per-frame metadata))))
      (setf (frame-samples frame) samples
            (frame-output frame)
            (loop repeat (metadata-channels metadata)
                  collect
                  (make-array samples
                              :element-type '(signed-byte 32)
                              :initial-element 0))))
    ;; Read entropy
    (if (and (> (metadata-channels metadata) 1)
             (zerop (logand +pseudo-stereo+
                            (frame-flags frame))))
        (read-stereo-frame reader frame)
        (read-mono-frame reader frame))
    frame))

(defmethod entropy-decode (reader frame (version (eql 3990)))
  (declare (ignore version))
  (let* ((outputs (frame-output frame))
         (samples (frame-samples frame))
         (rice-states (loop repeat samples collect (make-rice-state))))
    (dotimes (i samples)
      (loop for output in outputs
            for rice-state in rice-states do
              ;; Read rice coded value
              (print 'hi)))))
