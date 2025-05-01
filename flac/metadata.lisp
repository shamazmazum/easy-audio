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

(defun read-metadata-header (stream)
  "Returns (values START-POSITION LAST-BLOCK-P TYPE LENGTH)"
  (values (reader-position stream)
          (/= 0 (read-bit stream))
          (read-bits 7 stream)
          (read-bits 24 stream)))

(defun read-metadata-block (stream)
  "Read one metadata block from STREAM"
  (multiple-value-bind (start-position last-block-p type length)
      (read-metadata-header stream)

    (let* ((mtype (get-metadata-type type))
           (data (make-instance mtype
                                :last-block-p last-block-p
                                :length length
                                :start-position start-position)))
      (read-metadata-body stream data))))

(defmethod read-metadata-body (stream (data padding))
  ;; Read zero padding bytes
  (let ((chunk (make-array (list (metadata-length data))
                           :element-type '(ub 8))))
    (read-octet-vector chunk stream)
    ;; Do sanity checks
    (check-reserved-field chunk data "Padding bytes is not zero"))
  data)

(defmethod read-metadata-body (stream (data vorbis-comment))
  (flet ((read-comment-string (stream)
          (let ((buffer (make-array (list (read-bits 32 stream :endianness :little))
                                           :element-type '(unsigned-byte 8))))
            (flexi-streams:octets-to-string
             (read-octet-vector buffer stream)
             :external-format :utf-8))))
    (setf (vorbis-vendor-comment data)
          (read-comment-string stream))
    (let ((comments-num (read-bits 32 stream :endianness :little)))
      (setf (vorbis-user-comments data)
            (loop for i below comments-num collect
                  (read-comment-string stream)))))
  data)

(defmethod read-metadata-body (stream (data seektable))
  (flet ((read-seekpoint (stream)
           (let ((samplenum (read-eight-octets stream)))
             (if (/= samplenum +seekpoint-placeholder+)
                 (let ((offset (read-eight-octets stream))
                       (samples-in-frame (read-bits 16 stream)))
                   (make-seekpoint :samplenum samplenum
                                   :offset offset
                                   :samples-in-frame samples-in-frame))))))
    (multiple-value-bind (seekpoints-num remainder)
        (floor (metadata-length data) 18)
      (check-reserved-field remainder data "Bad seektable")
      (setf (seektable-seekpoints data)
            (loop for i below seekpoints-num collect
                  (read-seekpoint stream)))))
  data)

(declaim (inline read-streaminfo-body))
(defreader (read-streaminfo-body) (t)
  (streaminfo-minblocksize  (:octets 2))
  (streaminfo-maxblocksize  (:octets 2))
  (streaminfo-minframesize  (:octets 3))
  (streaminfo-maxframesize  (:octets 3))
  (streaminfo-samplerate    (:bits 20))
  (streaminfo-channels      (:bits 3) :function 1+)
  (streaminfo-bitspersample (:bits 5) :function 1+)
  (streaminfo-totalsamples  (:bits 36))
  (streaminfo-md5           (:octet-vector 16)))

(defmethod read-metadata-body (stream (data streaminfo))
  (read-streaminfo-body stream data)
  data)

(defun read-cuesheet-string (stream length)
  (let ((buffer (make-array (list length) :element-type '(ub 8))))
    (read-octet-vector buffer stream)
    (let ((pos (position 0 buffer)))
      (setq buffer
            (if pos (subseq buffer 0 pos) buffer)))
    (flexi-streams:octets-to-string buffer)))

(defreader* (read-cuesheet-index cuesheet-index () (data))
  (offset   (:custom read-eight-octets))
  (number   (:octet))
  (reserved (:bits 24)
            :lambda ((x) (check-reserved-field x data "Bad cuesheet index"))
            :ignore t))

(defreader* (read-cuesheet-track cuesheet-track () (data))
  (offset       (:custom read-eight-octets))
  (number       (:octet))
  (isrc         (:custom (lambda (r) (read-cuesheet-string r 12))))
  (type         (:bit) :lambda ((x) (if (zerop x) :audio :non-audio)))
  (pre-emphasis (:bit) :lambda ((x) (not (zerop x))))
  (reserved1    (:bits 6)
                :lambda ((x) (check-reserved-field x data "Bad cuesheet track"))
                :ignore t)
  (reserved2    (:octet-vector 13)
                :lambda ((x) (check-reserved-field x data "Bad cuesheet track"))
                :ignore t)
  (indices      (:custom (lambda (r) (loop for track below (read-octet r) collect
                                           (read-cuesheet-index r data))))))

(defmethod read-metadata-body (stream (data cuesheet))
  (setf (cuesheet-catalog-id data)
        (read-cuesheet-string stream 128)
        (cuesheet-lead-in data)
        (read-eight-octets stream)
        (cuesheet-cdp data)
        (not (zerop (read-bit stream))))
  (check-reserved-field (read-bits 7 stream)
                        data "Bad cuesheet")
  (check-reserved-field (read-octet-vector/new 258 stream)
                        data "Bad cuesheet")
  (let ((number-of-tracks (read-octet stream)))
    (setf (cuesheet-tracks data)
          (loop for track below number-of-tracks collect
                (read-cuesheet-track stream data))))
  data)

(defmethod read-metadata-body (stream (data picture))
  (let ((picture-type (nth (read-bits 32 stream) +picture-types+)))
    (unless (typep picture-type 'picture-type-id)
      (error 'flac-bad-metadata
             :format-control "Bad picture type"
             :metadata data))
    (setf (picture-type data) picture-type))

  (let* ((mime-type-len (read-bits 32 stream))
         (mime-type-seq (make-array (list mime-type-len)
                                    :element-type '(unsigned-byte 8))))
    (read-octet-vector mime-type-seq stream)
    (when (notevery
           #'(lambda (char)
               (and (>= char #x20)
                    (<= char #x7e)))
           mime-type-seq)
      (error 'flac-bad-metadata
             :format-control "MIME type must be an ASCII string"
             :metadata data))
    (setf (picture-mime-type data)
          (flexi-streams:octets-to-string mime-type-seq)))

  (let* ((description-len (read-bits 32 stream))
         (description-seq (make-array (list description-len)
                                      :element-type '(unsigned-byte 8))))
    (setf (picture-description data)
          (flexi-streams:octets-to-string (read-octet-vector description-seq stream))))

  (setf (picture-width data) (read-bits 32 stream)
        (picture-height data) (read-bits 32 stream)
        (picture-depth data) (read-bits 32 stream)
        (picture-color-num data) (read-bits 32 stream))

  (let* ((picture-len (read-bits 32 stream))
         (picture-seq (make-array (list picture-len)
                                  :element-type '(unsigned-byte 8))))
    ;; FIXME: artifical sanity check: Picture can be less than 10 MiB
    (when (> picture-len #.(ash 10 20))
      (error 'flac-bad-metadata
             :format-control "It's strange, but picture size is too long (~D bytes)"
             :format-arguments (list picture-len)
             :metadata data))
    (setf (picture-picture data) (read-octet-vector picture-seq stream)))
  data)

(defmethod read-metadata-body (stream (data metadata-header))
  (error 'flac-bad-metadata
         :format-control "Unknown metadata block"
         :metadata data))

(defun metadata-find-seektable (metadata)
  "Return a seektable from metadata list if any"
  (find 'seektable metadata
        :key #'type-of))
