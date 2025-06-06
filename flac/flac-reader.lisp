(in-package :easy-audio.flac)

(sera:-> unsigned-to-signed ((ub 32) (integer 0 32))
         (values (sb 32) &optional))
(declaim (inline unsigned-to-signed))
(defun unsigned-to-signed (byte len)
  (let ((sign-mask (ash 1 (1- len))))
    (if (< byte sign-mask)
        byte
        (- byte (ash sign-mask 1)))))

(sera:-> read-bits-array (reader (sa-sb 32) non-negative-fixnum &key
                                 (:signed boolean)
                                 (:len    non-negative-fixnum)
                                 (:offset non-negative-fixnum))
         (values (sa-sb 32) &optional))
(defun read-bits-array (stream array size &key
                               signed
                               (len (length array))
                               (offset 0))
  (declare (optimize (speed 3)))
  (loop for i from offset below len
        for val = (read-bits size stream) do
        (setf (aref array i)
              (if signed (unsigned-to-signed val size) val)))
  array)

;; TODO: rewrite
(sera:-> read-utf8-u32 (reader)
         (values (ub 32) &optional))
(defun read-utf8-u32 (stream)
  "Read frame number from a stream"
  (declare (optimize (speed 3)))
  (labels ((nbytes (x)
             (cond
               (( = 0 (logand x #x80))
                (values x 0))
               ((and
                 ( = 0 (logand x #x20))
                 (/= 0 (logand x #xC0)))
                (values (logand x #x1F) 1))
               ((and
                 ( = 0 (logand x #x10))
                 (/= 0 (logand x #xE0)))
                (values (logand x #x0F) 2))
               ((and
                 ( = 0 (logand x #x08))
                 (/= 0 (logand x #xF0)))
                (values (logand x #x07) 3))
               ((and
                 ( = 0 (logand x #x04))
                 (/= 0 (logand x #xF8)))
                (values (logand x #x03) 4))
               ((and
                 ( = 0 (logand x #x02))
                 (/= 0 (logand x #xFC)))
                (values (logand x #x01) 5))
               (t (error 'flac-bad-frame
                         :format-control "Error reading utf-8 coded value"))))
           (decode (v n)
             (if (zerop n) v
                 (let ((x (read-octet stream)))
                   (when (or (zerop (logand x #x80))
                             (not (zerop (logand x #x40))))
                     (error 'flac-bad-frame
                            :format-control "Error reading utf-8 coded value"))
                   (decode (logior (ash v 6) (logand x #x3f))
                           (1- n))))))
    (declare (ftype (function ((ub 32) (integer 0 5)) (values (ub 32) &optional)) decode))
    (multiple-value-call #'decode (nbytes (read-octet stream)))))

(sera:-> read-rice-signed (reader (integer 0 30))
         (values (sb 32) &optional))
(defun read-rice-signed (bitreader param)
  "Read signed rice-coded value"
  (declare (optimize (speed 3)))
  (let* ((unary  (count-zeros bitreader))
         (binary (read-bits param bitreader))
         (val    (logior (ash unary param) binary)))
    (declare (type (ub 32) unary))
    (if (zerop (logand val 1))
      (ash val -1)
      (- -1 (ash val -1)))))

(sera:-> restore-sync (reader &optional (or streaminfo null))
         (values unsigned-byte &optional))
(defun restore-sync (bitreader &optional streaminfo)
  "Restores lost sync and returns number of frame to be read"
  (declare (optimize (speed 3)))
  ;; Make sure, we are byte aligned. We must be, but anyway
  (read-to-byte-alignment bitreader)
  ;; Search first #xff octet
  (peek-octet bitreader #xff)
  (let ((pos (reader-position bitreader)))
    (handler-case
        (prog1
            (frame-number (read-frame bitreader streaminfo))
          (reader-position bitreader pos))
      (flac-bad-frame ()
        (reader-position bitreader (1+ pos))
        (restore-sync bitreader streaminfo)))))
