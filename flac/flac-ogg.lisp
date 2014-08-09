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
  "Open flac stream in ogg container"
  (let ((reader (ogg:open-ogg stream))
        (non-audio-packets 0)
        metadata)
    (let* ((packet (ogg:read-packet reader))
           (packet-reader (make-reader-from-buffer packet)))
      (if (or (not (ogg:ogg-bos reader))
              (/= #x7f (read-octet packet-reader))
              (/= +flac-ogg-id+ (read-octets 4 packet-reader)))
          (error 'flac-error :message "The first page of stream is invalid"))
      (read-octets 2 packet-reader) ; Major and minor versions of the mapping
      (setq non-audio-packets (read-octets 2 packet-reader))
      (if (/= +flac-id+ (read-octets 4 packet-reader))
          (error 'flac-error :message "The stream is not a flac stream"))
      (push (metadata-reader packet-reader) metadata))

    (if (not (ogg:fresh-page reader))
        (error 'flac-error :message "There are other packets on the first page"))

    ;; FIXME: skip headers for now
    (loop repeat non-audio-packets do (ogg:read-packet reader))

    (if (not (ogg:fresh-page reader))
        (error 'flac-error :message "Audio data must begin with a fresh page"))
    
    (values metadata reader)))

(defun ogg-frame-reader (reader &rest args)
  "Read flac frame from ogg container"
  (let* ((packet (ogg:read-packet reader))
         (packet-reader (make-reader-from-buffer packet
                                                 #+easy-audio-check-crc
                                                 :crc-fun
                                                 #+easy-audio-check-crc
                                                 #'crc-0-8005)))
    (apply #'frame-reader packet-reader args)))
