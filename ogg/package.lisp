(defpackage easy-audio.ogg
  (:use #:cl
        #:easy-audio.core
        #:easy-audio.bitreader
        #:alexandria)
  (:export #:read-packet
           #:fresh-page
           #:open-ogg
           #:restore-sync

           #:ogg-is-continued
           #:ogg-bos
           #:ogg-eos
           #:ogg-granule-position
           #:ogg-stream-serial
           #:ogg-page-number
           #:ogg-will-be-continued))
