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
      (if (or (not (ogg:ogg-bos reader))
              (/= #x7f (read-octet packet-reader))
              (/= +flac-ogg-id+ (read-octets 4 packet-reader)))
          (error 'flac-error :format-control "The first page of stream is invalid"))
      (read-octets 2 packet-reader) ; Major and minor versions of the mapping
      (setq non-audio-packets (read-octets 2 packet-reader))
      (if (/= +flac-id+ (read-octets 4 packet-reader))
          (error 'flac-error :format-control "The stream is not a flac stream"))
      (setq metadata (read-metadata-block packet-reader)))

    (if (not (ogg:fresh-page reader))
        (error 'flac-error :format-control "There are other packets on the first page"))

    (setq metadata
          (cons metadata
                (loop repeat non-audio-packets collect
                     (let* ((packet (ogg:read-packet reader))
                            (packet-reader (make-reader-from-buffer packet)))
                       (read-metadata-block packet-reader)))))

    (if (not (ogg:fresh-page reader))
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
