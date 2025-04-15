(in-package :easy-audio.flac)

(defconstant +flac-id+ #x664C6143) ; "fLaC"

(defun read-block-and-fix (bitreader metadata)
  "Read malformed metadata block in RAWDATA slot (for debugging)"
  (reader-position bitreader
                   (+ (metadata-start-position metadata)
                      4))
  (let ((chunk (make-array (list (metadata-length metadata))
                           :element-type '(ub 8))))
    (setf (slot-value metadata 'rawdata)
          (read-octet-vector chunk bitreader))))

(defun fix-stream-position (bitreader metadata)
  "Set stream position to end of the malformed metadata block"
  (reader-position bitreader
                   (+ (metadata-start-position metadata)
                      (metadata-length metadata)
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
  (when (/= +flac-id+ (read-octets 4 bitreader))
    (error 'flac-error :format-control "This stream is not a flac stream"))

  (do (last-block metadata-list)
      (last-block (reverse metadata-list))
    (setq last-block
          (with-interactive-debug
            (restart-case
                (let ((metadata (read-metadata-block bitreader)))
                  (push metadata metadata-list)
                  (metadata-last-block-p metadata))

              (skip-malformed-metadata (c)
                :interactive (lambda () (list *current-condition*))
                :report "Skip malformed metadata"
                (let ((metadata (flac-metadata c)))
                  (fix-stream-position bitreader metadata)
                  (metadata-last-block-p metadata)))

              (read-raw-block (c)
                :interactive (lambda () (list *current-condition*))
                :report "Interprete as unknown metadata block"
                (let ((metadata (flac-metadata c)))
                  (read-block-and-fix bitreader metadata)
                  (push metadata metadata-list)
                  (metadata-last-block-p metadata))))))))
