(in-package :easy-audio.flac)

(defconstant +flac-ogg-id+ #x464c4143
  "`FLAC' signature")

(defun open-ogg-flac (stream)
  "Return BITREADER handler of ogg-encapsulated flac stream"
  (ogg:open-ogg stream))

(defmacro with-open-ogg-flac ((reader name &rest options) &body body)
  "A helper macro like WITH-OPEN-FILE. READER can be used as an
   argument to READ-OGG-METADATA or READ-OGG-FRAME inside this macro."
  (let ((stream (gensym)))
    `(let* ((,stream (apply #'open ,name :element-type '(ub 8) ,options))
            (,reader (open-ogg-flac ,stream)))
       (unwind-protect (progn ,@body) (close ,stream)))))

(defun read-ogg-metadata (reader)
  "Return list of metadata in ogg-encapsulated stream"
  (let ((non-audio-packets 0)
        metadata)
    (let* ((packet (ogg:read-packet reader))
           (packet-reader (make-reader-from-buffer packet)))
      (when (or (not (ogg:ogg-bos reader))
                (/= #x7f (read-octet packet-reader))
                (/= +flac-ogg-id+ (read-octets 4 packet-reader)))
        (error 'flac-error :format-control "The first page of stream is invalid"))
      (read-octets 2 packet-reader) ; Major and minor versions of the mapping
      (setq non-audio-packets (read-octets 2 packet-reader))
      (when (/= +flac-id+ (read-octets 4 packet-reader))
        (error 'flac-error :format-control "The stream is not a flac stream"))
      (setq metadata (read-metadata-block packet-reader)))

    (unless (ogg:fresh-page reader)
      (error 'flac-error :format-control "There are other packets on the first page"))

    (setq metadata
          (cons metadata
                (loop repeat non-audio-packets collect
                     (let* ((packet (ogg:read-packet reader))
                            (packet-reader (make-reader-from-buffer packet)))
                       (read-metadata-block packet-reader)))))

    (unless (ogg:fresh-page reader)
      (error 'flac-error :format-control "Audio data must begin with a fresh page"))

    metadata))

(defun read-ogg-frame (reader &optional streaminfo)
  "Read flac frame from ogg container"
  (let* ((packet (ogg:read-packet reader))
         (packet-reader (make-reader-from-buffer packet
                                                 #+easy-audio-check-crc
                                                 :crc-fun
                                                 #+easy-audio-check-crc
                                                 #'crc-0-8005)))
    (read-frame packet-reader streaminfo)))
