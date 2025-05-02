(in-package :easy-audio.flac)

(defconstant +flac-id+ #x664C6143) ; "fLaC"

(defun fix-stream-position/start (bitreader header)
  "Set stream position to end of the malformed metadata block"
  (reader-position
   bitreader
   (+ (metadata-header-start-position header) 4)))

(defun fix-stream-position/end (bitreader header)
  "Set stream position to end of the malformed metadata block"
  (reader-position
   bitreader
   (+ (metadata-header-start-position header)
      (metadata-header-length         header)
      4)))

(defun open-flac (stream)
  "Return @c(bitreader) handler of flac stream"
  (make-reader :stream stream
               #+easy-audio-check-crc
               :crc-fun
               #+easy-audio-check-crc
               #'crc-0-8005))

(defmacro with-open-flac ((reader name &rest options) &body body)
  "A helper macro like WITH-OPEN-FILE. READER can be used as an
   argument to READ-METADATA or READ-FRAME inside this macro."
  (let ((stream (gensym)))
    `(let* ((,stream (open ,name :element-type '(ub 8) ,@options))
            (,reader (open-flac ,stream)))
       (unwind-protect (progn ,@body) (close ,stream)))))

(defun read-metadata (bitreader)
  "Return list of metadata blocks in the stream"
  ;; Checking if stream is a flac stream
  (unless (= +flac-id+ (read-octets 4 bitreader))
    (error 'flac-error :format-control "This stream is not a flac stream"))

  (do (last-block metadata-list)
      (last-block (reverse metadata-list))
    (setq last-block
          (with-interactive-debug
            (restart-case
                (multiple-value-bind (metadata last-block-p)
                    (read-metadata-block bitreader)
                  (push metadata metadata-list)
                  last-block-p)

              (skip-malformed-metadata (c)
                :interactive (lambda () (list *current-condition*))
                :report "Skip malformed metadata"
                (let ((header (flac-metadata c)))
                  (fix-stream-position/end bitreader header)
                  (metadata-header-last-block-p header)))

              (read-raw-block (c)
                :interactive (lambda () (list *current-condition*))
                :report "Interprete as unknown metadata block"
                (let ((header (flac-metadata c)))
                  (fix-stream-position/start bitreader header)
                  (push (read-body-unknown bitreader header) metadata-list)
                  (metadata-header-last-block-p header))))))))
