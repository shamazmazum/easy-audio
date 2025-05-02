(in-package :easy-audio.flac)

;; READ-OCTETS can read mostly 4 octets at once
(sera:-> read-eight-octets (reader)
         (values (unsigned-byte 64) &optional))
(declaim (inline read-eight-octets))
(defun read-eight-octets (stream)
  (logior (ash (read-octets 4 stream) 32)
          (ash (read-octets 4 stream) 0)))

(declaim (inline check-reserved-field))
(defun check-reserved-field (x metadata string)
  (unless (or (and (numberp x) (zerop x))
              (and (arrayp x) (every #'zerop x)))
    (error 'flac-bad-metadata
           :format-control string
           :metadata metadata))
  x)

(sera:-> read-metadata-header (reader)
         (values metadata-header &optional))
(defun read-metadata-header (stream)
  "Returns (values START-POSITION LAST-BLOCK-P TYPE LENGTH)"
  (metadata-header (reader-position stream)
                   (not (zerop (read-bit stream)))
                   (read-bits 7 stream)
                   (read-bits 24 stream)))

(sera:-> read-body-dummy (reader metadata-header)
         (values &optional))
(defun read-body-dummy (reader header)
  (declare (ignore reader))
  (error 'flac-bad-metadata
         :format-control "Unknown metadata block"
         :metadata header)
  (values))

(sera:-> read-body-unknown (reader metadata-header)
         (values unknown-metadata &optional))
(defun read-body-unknown (reader header)
  (unknown-metadata
   (read-octet-vector/new
    (metadata-header-length header)
    reader)))

(sera:-> read-body-streaminfo (reader metadata-header)
         (values streaminfo &optional))
(defreader* (read-body-streaminfo streaminfo () (header))
  (declare (ignore header))
  (streaminfo-minblocksize  (:octets 2))
  (streaminfo-maxblocksize  (:octets 2))
  (streaminfo-minframesize  (:octets 3))
  (streaminfo-maxframesize  (:octets 3))
  (streaminfo-samplerate    (:bits 20))
  (streaminfo-channels      (:bits 3) :function 1+)
  (streaminfo-bitspersample (:bits 5) :function 1+)
  (streaminfo-totalsamples  (:bits 36))
  (streaminfo-md5           (:octet-vector 16)))

(sera:-> read-body-padding (reader metadata-header)
         (values padding &optional))
(defreader* (read-body-padding padding () (header))
  (padding (:octet-vector (metadata-header-length header))
           :lambda ((x) (check-reserved-field
                         x header
                         "Padding bytes are not zero"))
           :ignore t))

(sera:-> read-body-vorbis-comment (reader metadata-header)
         (values vorbis-comment &optional))
(defun read-body-vorbis-comment (stream header)
  (declare (ignore header))
  (flet ((read-comment-string (stream)
          (let ((buffer (read-octet-vector/new
                         (read-bits 32 stream :endianness :little)
                         stream)))
            (flexi-streams:octets-to-string
             buffer :external-format :utf-8))))
    (vorbis-comment
     (read-comment-string stream)
     (let ((comments-num (read-bits 32 stream :endianness :little)))
       (loop for i below comments-num collect
             (read-comment-string stream))))))

(sera:-> read-body-seektable (reader metadata-header)
         (values seektable &optional))
(defun read-body-seektable (stream header)
  (flet ((read-seekpoint (stream)
           (let ((samplenum (read-eight-octets stream)))
             (if (/= samplenum +seekpoint-placeholder+)
                 (let ((offset (read-eight-octets stream))
                       (samples-in-frame (read-bits 16 stream)))
                   (seekpoint samplenum offset samples-in-frame))))))
    (multiple-value-bind (seekpoints-num remainder)
        (floor (metadata-header-length header) 18)
      (check-reserved-field remainder header "Bad seektable")
      (seektable
       (loop for i below seekpoints-num collect
             (read-seekpoint stream))))))

(sera:-> read-cuesheet-string ((sa-ub 8))
         (values string &optional))
(defun read-cuesheet-string (vector)
  (let ((pos (position 0 vector)))
    (flexi-streams:octets-to-string
     (subseq vector 0 pos))))

(defreader* (read-cuesheet-index cuesheet-index () (data))
  (offset   (:custom read-eight-octets))
  (number   (:octet))
  (reserved (:bits 24)
            :lambda ((x) (check-reserved-field x data "Bad cuesheet index"))
            :ignore t))

(defreader* (read-cuesheet-track cuesheet-track () (data))
  (offset       (:custom read-eight-octets))
  (number       (:octet))
  (isrc         (:octet-vector 12)
                :function read-cuesheet-string)
  (type         (:bit) :lambda ((x) (if (zerop x) :audio :non-audio)))
  (pre-emphasis (:bit) :lambda ((x) (not (zerop x))))
  (reserved1    (:bits 6)
                :lambda ((x) (check-reserved-field x data "Bad cuesheet track"))
                :ignore t)
  (reserved2    (:octet-vector 13)
                :lambda ((x) (check-reserved-field x data "Bad cuesheet track"))
                :ignore t)
  (indices      (:custom (lambda (r) (loop repeat (read-octet r) collect
                                           (read-cuesheet-index r data))))))

(sera:-> read-body-cuesheet (reader metadata-header)
         (values cuesheet &optional))
(defreader* (read-body-cuesheet cuesheet () (data))
  (catalog-id (:octet-vector 128)
              :function read-cuesheet-string)
  (lead-in    (:custom read-eight-octets))
  (cdp        (:bit)
              :lambda ((x) (not (zerop x))))
  (reserved1  (:bits 7)
              :lambda ((x) (check-reserved-field x data "Bad cuesheet"))
              :ignore t)
  (reserved2  (:octet-vector 258)
              :lambda ((x) (check-reserved-field x data "Bad cuesheet"))
              :ignore t)
  (tracks     (:custom (lambda (r) (loop repeat (read-octet r) collect
                                         (read-cuesheet-track r data))))))

(sera:-> read-body-picture (reader metadata-header)
         (values picture &optional))
(defreader* (read-body-picture picture () (header))
  (type            (:octets 4)
                   :lambda
                   ((x)
                    (unless (<= x 20)
                      (error 'flac-bad-metadata
                             :format-control "Bad picture type"
                             :metadata header))
                    x))
  (mime-type-len   (:octets 4) :skip t)
  (mime-type       (:octet-vector mime-type-len)
                   :lambda
                   ((xs)
                    (unless (every
                             #'(lambda (char)
                                 (and (>= char #x20)
                                      (<= char #x7e)))
                             xs)
                      (error 'flac-bad-metadata
                             :format-control "MIME type must be an ASCII string"
                             :metadata header))
                    (flexi-streams:octets-to-string xs)))
  (description-len (:octets 4) :skip t)
  (description     (:octet-vector description-len)
                   :lambda ((xs) (flexi-streams:octets-to-string
                                  xs :external-format :utf-8)))
  (width           (:octets 4))
  (height          (:octets 4))
  (depth           (:octets 4))
  (color-num       (:octets 4))
  (picture-len     (:octets 4) :skip t)
  (picture         (:octet-vector picture-len)))

(defparameter *block-readers*
  `((0 . ,#'read-body-streaminfo)
    (1 . ,#'read-body-padding)
    (3 . ,#'read-body-seektable)
    (4 . ,#'read-body-vorbis-comment)
    (5 . ,#'read-body-cuesheet)
    (6 . ,#'read-body-picture)))

(sera:-> get-metadata-reader (integer)
         (values (sera:-> (reader metadata-header) (values metadata &optional)) &optional))
(defun get-metadata-reader (code)
  "Get metadata reader by its type code"
  (let ((reader (assoc code *block-readers*)))
    (if reader (cdr reader) #'read-body-dummy)))

(sera:-> read-metadata-block (reader)
         (values metadata boolean &optional))
(defun read-metadata-block (stream)
  "Read one metadata block from STREAM"
  (let* ((header (read-metadata-header stream))
         (reader (get-metadata-reader (metadata-header-type header))))
    (values
     (funcall reader stream header)
     (metadata-header-last-block-p header))))

(serapeum:-> metadata-find-seektable (list)
             (values (or null seektable) &optional))
(defun metadata-find-seektable (metadata)
  "Return a seektable from metadata list if any"
  (find 'seektable metadata :key #'type-of))
