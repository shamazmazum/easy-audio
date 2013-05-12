;; Copyright (c) 2012, Vasily Postnicov
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

(defun skip-malformed-metadata (c)
  (invoke-restart 'skip-malformed-metadata c))

(defun read-raw-block (c)
  (invoke-restart 'read-raw-block c))

(defconstant +flac-id+ #x664C6143) ; "fLaC"

(defun read-block-and-fix (bitreader metadata)
  "Read malformed metadata block in RAWDATA slot (for debugging)"
  (reader-position bitreader
                   (+ (metadata-start-position metadata)
                      4))
  (let ((chunk (make-array (list (metadata-length metadata))
			   :element-type '(unsigned-byte 8))))
    (read-octet-vector chunk bitreader)
    (setf (slot-value metadata 'rawdata) chunk)))

(defun fix-stream-position (bitreader metadata)
  "Set stream position to end of the malformed metadata block"
  (reader-position bitreader
                   (+ (metadata-start-position metadata)
                      (metadata-length metadata)
                      4)))

(defun open-flac (stream)
  "Reads flac file from STREAM and returns two values:
   list of metadata blocks and bitreader wrapper around stream"
  ;; Checking if stream is flac stream
  (let ((bitreader (make-reader :stream stream)))
    (if (/= +flac-id+ #+x86_64 (read-bits 32 bitreader)
                      #-x86_64 (bitreader.be-bignum:read-bits 32 bitreader))
        (error 'flac-error :message "Stream is not flac stream"))

    (let (metadata-list)
     (do (last-block)
         (last-block)
       
       (setq last-block
             (restart-case
              (let ((metadata (metadata-reader bitreader)))
                (push metadata metadata-list)
                (metadata-last-block-p metadata))
              (skip-malformed-metadata (c)
                                       (let ((metadata (flac-metadata c)))
                                         (fix-stream-position bitreader metadata)
                                         (metadata-last-block-p metadata)))
              (read-raw-block (c)
                              (let ((metadata (flac-metadata c)))
                                (read-block-and-fix bitreader metadata)
                                (push metadata metadata-list)
                                (metadata-last-block-p metadata))))))
     (values
      (reverse metadata-list)
      bitreader))))
