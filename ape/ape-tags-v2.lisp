(in-package :easy-audio.ape)

(define-constant +apev2-preamble+
    (map 'vector #'char-code "APETAGEX")
  :test #'equalp)

(defparameter *apev2-external-format* '(:utf-8 :eol-style :crlf)
  "External format used in human-readable APEv2 items. Defaults to UTF-8, CR/LF EOL style.")

(defconstant +flag-has-header+ (ash 1 31))
(defconstant +flag-has-footer+ (ash 1 30))
(defconstant +flag-h/f-type+   (ash 1 29))
(defconstant +flag-read-only+  (ash 1  0))

(sera:-> has-footer-p ((ub 32))
         (values boolean &optional))
(declaim (inline has-footer-p))
(defun has-footer-p (flags)
  (not (some-bits-set-p flags +flag-has-footer+)))

(sera:-> has-header-p ((ub 32))
         (values boolean &optional))
(declaim (inline has-header-p))
(defun has-header-p (flags)
  (some-bits-set-p flags +flag-has-header+))

(sera:-> h/f-type ((ub 32))
         (values (member :footer :header) &optional))
(declaim (inline h/f-type))
(defun h/f-type (flags)
  (if (some-bits-set-p flags +flag-h/f-type+)
      :header :footer))

(sera:-> check-bits-3...28 ((ub 32))
         (values (ub 32) &optional))
(declaim (inline check-bits-3...28))
(defun check-bits-3...28 (flags)
  (unless (zerop (logand #x1ffffff8 flags))
    (error 'apev2-tag-error :format-control "Invalid tag/item flags"))
  flags)

(sera:-> apev2-item-content-type ((ub 32))
         (values (member :utf-8 :binary :external) &optional))
(declaim (inline apev2-item-content-type))
(defun apev2-item-content-type (flags)
  "Return content type of an item: either UTF-8, :BINARY or
:EXTERNAL. :UTF-8 and :EXTERNAL are strings (the latter is in a
special format) and :BINARY is a vector of octets."
  (case (ldb (byte 2 1) flags)
    (0 :utf-8)
    (1 :binary)
    (2 :external)
    (3 (error 'apev2-tag-error :format-control "Invalid tag item content type"))))

(declaim (inline check-preamble))
(defun check-preamble (preamble)
  (unless (every #'= preamble +apev2-preamble+)
    (error 'apev2-tag-error :format-control "Not an APEv2 tag"))
  preamble)

(declaim (inline check-h/f-reserved))
(defun check-h/f-reserved (reserved)
  (unless (zerop reserved)
    (error 'apev2-tag-error :format-control "Header/footer reserved slot is not zero"))
  reserved)

(sera:defconstructor apev2-tag-block
  (version (ub 32))
  (size    (ub 32))
  (items   (ub 32))
  (flags   (ub 32)))

(defreader* (read-tag-block apev2-tag-block () ())
  (preamble  (:octet-vector (length +apev2-preamble+)) :function check-preamble :ignore t)
  (version   (:octets 4) :endianness :little)
  (size      (:octets 4) :endianness :little)
  (items     (:octets 4) :endianness :little)
  (flags     (:octets 4) :endianness :little :function check-bits-3...28)
  (reserved1 (:octets 4) :function check-h/f-reserved :ignore t)
  (reserved2 (:octets 4) :function check-h/f-reserved :ignore t))

(sera:defconstructor apev2-tag-item
  "An item (key/value pair) in apev2 tag block"
  (key       string)
  (value     t)
  (read-only boolean))

(sera:-> read-item (reader)
         (values apev2-tag-item &optional))
(defun read-item (reader)
  (let ((value-size (read-octets 4 reader :endianness :little))
        (flags (check-bits-3...28 (read-octets 4 reader :endianness :little)))
        (position (reader-position reader))
        (new-position (progn
                        (peek-octet reader 0)
                        (reader-position reader))))
    (reader-position reader position)
    (let ((key
            (prog1 (map 'string #'code-char
                        (read-octet-vector/new (- new-position position) reader))
              ;; Read zero terminator
              (read-octet reader)))
          (value
            ;; FIXME: Value list is not supported now, but support may be
            ;; easily added later.
            (let ((array (read-octet-vector/new value-size reader)))
              (case (apev2-item-content-type flags)
                (:utf-8 (flexi-streams:octets-to-string
                         array :external-format *apev2-external-format*))
                (:binary array)
                (t (error 'apev2-tag-error :format-control "Unknown content type"))))))
      (apev2-tag-item key value (some-bits-set-p flags +flag-read-only+)))))

(sera:-> read-apev2-tag (reader)
         (values list &optional))
(defun read-apev2-tag (reader)
  "Read APEv2 tag from the reader"
  (let* ((header (read-tag-block reader))
         (items (loop repeat (apev2-tag-block-items header)
                      collect (read-item reader))))
    (when (has-footer-p (apev2-tag-block-flags header))
      (read-tag-block reader))
    items))

(defun read-apev2-tag-from-end (reader)
  "Helper function to read APEv2 tag from the end of the reader's
stream.  Needs APEv2 tag with a footer."
  (let ((length (reader-length reader)))
    (when (< length 32)
      (error 'apev2-tag-error :format-control "Stream is too short to be an APEv2 tag"))
    (reader-position reader (- length 32))
    (let* ((footer (read-tag-block reader))
           (flags (apev2-tag-block-flags footer))
           (size  (apev2-tag-block-size  footer)))
      (unless (and (has-header-p flags)
                   (has-footer-p flags) ; Sanity check
                   (eq (h/f-type flags) :footer))
        (error 'apev2-tag-error :format-control "Cannot read APEv2 tag from the end of stream"))
      (when (< length (+ size 32))
        (error 'apev2-tag-error :format-control "Stream is too short to be an APEv2 tag"))
      (reader-position reader (- length 32 size))
      (read-apev2-tag reader))))
