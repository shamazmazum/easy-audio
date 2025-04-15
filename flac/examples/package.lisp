(defpackage easy-audio.flac-examples
  (:use #:cl
        #:easy-audio.flac
        #:easy-audio.wav
        #:easy-audio.core)
  (:nicknames #:flac-examples)
  (:export #:flac2wav
           #:ogg2wav))
