;; Copyright (c) 2012-2014, Vasily Postnicov
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 

;; 1. Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution. 

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :easy-audio.ogg)

(define-condition ogg-error ()
  ((message :initarg :message
	    :initform ""
	    :type string
	    :reader ogg-error-message
            :documentation "Error message"))
  (:report (lambda (c s)
	     (format s "General ogg error: ~A"
		     (ogg-error-message c))))
  (:documentation "General (unspecified) ogg error"))

(defconstant +ogg-page-id+ #x4f676753
  "OggS page identificator")
(defconstant +continued-packet+ #b1
  "Continued packet flag")
(defconstant +begining-of-stream+ #b10
  "First page of logical bitstream")
(defconstant +end-of-stream+ #b100
  "Last page of logical bitstream")

(defstruct (ogg-reader (:conc-name "OGG-"))
  (is-continued      nil :type boolean)
  (bos               nil :type boolean)
  (eos               nil :type boolean)
  (granule-position  0   :type (or (integer -1 -1)
                                   non-negative-int))
  (stream-serial     0   :type (ub 32))
  (page-number       0   :type (ub 32))
  (crc               0   :type (ub 32))
  (segment-table     nil :type list)
  (will-be-continued nil :type boolean)
  (reader-position   0   :type (ub 8)))

(defun read-ogg-segment-table (reader segments)
  (loop with continued = nil
        with segment = 0
        while (< segment segments)
        collect
        (loop for lacing-val = (read-octet reader)
              for continued-packet = (= lacing-val 255)
              sum lacing-val
              do (incf segment)
              while continued-packet
              finally (setq continued continued-packet)) into packet-sizes
       finally (return (values packet-sizes continued))))

(defun read-page-header (reader ogg-reader)
  #+easy-audio-check-crc
  (init-crc reader)
  (if (/= (read-octets 4 reader)
          +ogg-page-id+)
      (error 'ogg-error :message "Wrong page ID"))
  (if (/= (read-octet reader) 0)
      (error 'ogg-error :message "Wrong stream structure version"))
  (let* ((flags (read-octet reader))
         (is-continued (/= 0 (logand flags +continued-packet+)))
         (bos (/= 0 (logand flags +begining-of-stream+)))
         (eos (/= 0 (logand flags +end-of-stream+))))

    (setf (ogg-is-continued ogg-reader) is-continued
          (ogg-bos ogg-reader) bos
          (ogg-eos ogg-reader) eos
          
          (ogg-granule-position ogg-reader)
          (logior (ash (read-octets 2 reader :endianness :little) 0)
                  (ash (read-octets 2 reader :endianness :little) 16)
                  (ash (read-octets 2 reader :endianness :little) 24)
                  (ash (read-octets 2 reader :endianness :little) 32))

          (ogg-stream-serial ogg-reader) (read-octets 4 reader :endianness :little)
          (ogg-page-number ogg-reader) (read-octets 4 reader :endianness :little)
          (ogg-crc ogg-reader) (let ((*read-with-zeroing* t))
                                 (read-octets 4 reader :endianness :little))
          (ogg-reader-position ogg-reader) 0))

  (let ((segments (read-octet reader)))
    (multiple-value-bind (segment-table will-be-continued)
        (read-ogg-segment-table reader segments)
      (setf (ogg-segment-table ogg-reader) segment-table
            (ogg-will-be-continued ogg-reader) will-be-continued))))

(defun read-packet-pages (reader ogg-reader &optional previous-page-num pages)
  (with-accessors ((segment-table ogg-segment-table)
                   (position ogg-reader-position)) ogg-reader

    (when (= position (length segment-table))
      (read-page-header reader ogg-reader))

    (if (and previous-page-num
             (= (- (ogg-page-number ogg-reader)
                   previous-page-num) 1)
             (ogg-is-continued ogg-reader))
        (error 'ogg-error :message "Lost sync"))

    (let ((packet (make-array (nth position segment-table) :element-type '(ub 8))))
      (read-octet-vector packet reader)
      (incf position)
      (if (and (= position (length segment-table))
               (/= (ogg-crc ogg-reader) (get-crc reader)))
          (error 'ogg-error :message "CRC mismatch"))
      (if (or (not (ogg-will-be-continued ogg-reader))
              (< position
                 (1- (length segment-table))))
          (cons packet pages)
          (read-packet-pages reader ogg-reader
                             (ogg-page-number ogg-reader)
                             (cons packet pages))))))

(defun read-packet (reader ogg-reader)
  (let ((segments (read-packet-pages reader ogg-reader)))
    (if (= (length segments) 1) (car segments)
        (error 'ogg-error :message "Cannot read packets which span many pages"))))
