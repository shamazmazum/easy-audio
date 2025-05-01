(defpackage easy-audio.bitreader
  (:use #:cl #:easy-audio.core #:alexandria)
  (:export
   ;; Conditions
   #:bitreader-eof

   ;; Reader structure and accessors
   #:reader
   #:make-reader ; Obsolete
   #:make-reader-from-stream
   #:make-reader-from-buffer

   ;; "End user" functions
   #:read-bit  #:read-bit-bw
   #:read-bits #:read-bits-bw
   #:read-octet
   #:read-octet-vector
   #:read-octets
   #:read-to-byte-alignment
   #:count-zeros
   #:reader-position
   #:peek-octet
   #:reader-length

   #:*read-with-zeroing*
   #:with-crc
   #:with-skipping-crc

   #:defreader
   #:defreader*

   #+easy-audio-check-crc
   #:init-crc
   #+easy-audio-check-crc
   #:get-crc
   #+easy-audio-check-crc
   #:crc-0-8005
   #+easy-audio-check-crc
   #:crc-0-04c11db7))
