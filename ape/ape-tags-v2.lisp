(in-package :easy-audio.ape)

(defparameter *apev2-preamble* #.(map 'vector #'char-code "APETAGEX"))
(defparameter *apev2-external-format* '(:utf-8 :eol-style :crlf)
  "External format used in human-readable APEv2 items")

(defun has-header (flags)
  (declare (type fixnum flags))
  (not (zerop (logand flags (ash 1 31)))))

(defun has-footer (flags)
  (declare (type fixnum flags))
  (zerop (logand flags (ash 1 30))))

(defun h/f-type (flags)
  (declare (type fixnum flags))
  (if (zerop (logand (ash 1 29) flags))
      :footer :header))

(defun check-bits-3...28 (flags)
  (unless (zerop (logand #x1ffffff8 flags))
    (error 'apev2-tag-error :format-control "Invalid tag/item flags"))
  flags)

(defun content-type (flags)
  (case (ldb (byte 2 1) flags)
    (0 :utf-8)
    (1 :binary)
    (2 :external)
    (3 (error 'apev2-tag-error :format-control "Invalid tag item content type"))))

(defun content-r/w-p (flags)
  (zerop (logand 1 flags)))

(defun check-preamble (preamble)
  (unless (equalp preamble *apev2-preamble*)
    (error 'apev2-tag-error :format-control "Not an APEv2 tag"))
  preamble)

(defun check-h/f-reserved (reserved)
  (unless (zerop reserved)
    (error 'apev2-tag-error :format-control "Header/footer reserved slot is not zero"))
  reserved)

(defstruct (header/footer
             (:conc-name :h/f-))
  preamble
  (version     0 :type (ub 32))
  (size        0 :type (ub 32))
  (items-count 0 :type (ub 32))
  (flags       0 :type (ub 32)))

(defreader (read-header/footer) ((make-header/footer))
  (h/f-preamble (:octet-vector (make-array (length *apev2-preamble*)
                                           :element-type '(ub 8)))
                :function check-preamble)
  (h/f-version  (:octets 4)
                :endianness :little)
  (h/f-size     (:octets 4)
                :endianness :little)
  (h/f-items-count
                (:octets 4)
                :endianness :little)
  (h/f-flags    (:octets 4)
                :endianness :little
                :function check-bits-3...28)
  (()           (:octets 4)
                :function check-h/f-reserved)
  (()           (:octets 4)
                :function check-h/f-reserved))

(defstruct item
  (value-size 0 :type (ub 32))
  (flags      0 :type (ub 32))
  key
  value)

(defreader (read-item%) ((make-item))
  (item-value-size (:octets 4)
                   :endianness :little)
  (item-flags      (:octets 4)
                   :endianness :little
                   :function check-bits-3...28))

;; More user-friendly representation of an item
(defun item-as-list (item)
  "Convert ITEM structure to user-friendly list representation in the form
(key value :TYPE content-type :r/w is-item-writable)."
  (let ((flags (item-flags item)))
    (list
     (item-key item)
     (item-value item)
     :type
     (content-type flags)
     :r/w
     (content-r/w-p flags))))

(defun read-item (reader)
  "Read APEv2 item from reader as an ITEM structure"
  (let ((item (read-item% reader))
        (position (reader-position reader))
        (new-position (progn
                        (peek-octet reader 0)
                        (reader-position reader))))
    (reader-position reader position)
    (setf (item-key item)
          (let ((array (make-array (- new-position position)
                                   :element-type '(unsigned-byte 8))))
            (map 'string #'code-char
                 (read-octet-vector array reader))))
    (read-octet reader)
    ;; FIXME: Value list is not supported now, but support may be
    ;; easily added later.
    (setf (item-value item)
          (let ((array (make-array (item-value-size item)
                                   :element-type '(unsigned-byte 8))))
            (read-octet-vector array reader)
            (case (content-type (item-flags item))
              (:utf-8 (flexi-streams:octets-to-string
                       array :external-format *apev2-external-format*))
              (:binary array)
              (t (error 'apev2-tag-error :format-control "Unknown content type")))))
    item))

(defun read-tag (reader)
  "Read APEv2 tag from reader"
  (let* ((header (read-header/footer reader))
         (items (loop repeat (h/f-items-count header) collect
                     (item-as-list (read-item reader)))))
    (if (has-footer (h/f-flags header))
        (read-header/footer reader))
    items))

(defun read-tag-from-end (reader)
  "Helper function to read APEv2 tag from end of reader's stream.
Changes reader's position. Needs APEv2 tag to contain a footer."
  (let ((length (reader-length reader)))
    (when (< length 32)
      (error 'apev2-tag-error :format-control "Stream is too short to be an APEv2 tag"))
    (reader-position reader (- length 32))
    (let* ((footer (read-header/footer reader))
           (flags (h/f-flags footer)))
      (unless (and (has-header flags)
                   (has-footer flags) ; Sanity check
                   (eq (h/f-type flags) :footer))
        (error 'apev2-tag-error :format-control "Cannot read APEv2 tag from the end of stream"))
      (when (< length (+ 32 (h/f-size footer)))
        (error 'apev2-tag-error :format-control "Stream is too short to be an APEv2 tag"))
      (reader-position reader
                       (- length 32 (h/f-size footer)))
      (read-tag reader))))
