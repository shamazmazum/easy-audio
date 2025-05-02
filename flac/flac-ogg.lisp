(in-package :easy-audio.flac)

(defconstant +flac-ogg-id+ #x464c4143
  "`FLAC' signature")

(defun open-ogg-flac (stream)
  "Return a @c(bitreader) handler of an ogg-encapsulated flac stream."
  (ogg:open-ogg stream))

(defmacro with-open-ogg-flac ((reader name &rest options) &body body)
  "A helper macro like @c(with-open-file). @c(reader) can be used as
an argument to @c(read-ogg-metadata) or @c(read-ogg-frame) inside this
macro."
  (let ((stream (gensym)))
    `(let* ((,stream (apply #'open ,name :element-type '(ub 8) ,options))
            (,reader (open-ogg-flac ,stream)))
       (unwind-protect (progn ,@body) (close ,stream)))))

(defun read-ogg-metadata (reader)
  "Return a list of metadata in an ogg-encapsulated stream."
  (let* ((packet (ogg:read-packet reader))
         (packet-reader (make-reader-from-buffer packet)))
    (unless (and (ogg:ogg-bos reader)
                 (= #x7f (read-octet packet-reader))
                 (= +flac-ogg-id+ (read-octets 4 packet-reader)))
      (error 'flac-error :format-control "The first page of stream is invalid"))
    ;; Major and minor versions of the mapping
    (read-octets 2 packet-reader)
      (let ((non-audio-packets (read-octets 2 packet-reader)))
        (unless (= +flac-id+ (read-octets 4 packet-reader))
          (error 'flac-error :format-control "The stream is not a flac stream"))
        (let ((first-metadata (read-metadata-block packet-reader)))
          (unless (ogg:fresh-page reader)
            (error 'flac-error :format-control "There are other packets on the first page"))
          (let ((rest-metadata
                 (loop repeat non-audio-packets
                       for packet = (ogg:read-packet reader)
                       for packet-reader = (make-reader-from-buffer packet)
                       collect (read-metadata-block packet-reader))))
            (unless (ogg:fresh-page reader)
              (error 'flac-error :format-control "Audio data must begin with a fresh page"))
            (cons first-metadata rest-metadata))))))

(defun read-ogg-frame (reader &optional streaminfo)
  "Read a flac frame from an ogg container."
  (let* ((packet (ogg:read-packet reader))
         (packet-reader (make-reader-from-buffer
                         packet
                         #+easy-audio-check-crc
                         :crc-fun
                         #+easy-audio-check-crc
                         #'crc-0-8005)))
    (read-frame packet-reader streaminfo)))
