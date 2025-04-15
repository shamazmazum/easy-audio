(defpackage easy-audio.wav-examples
  (:use #:cl
        #:easy-audio.wav
        #:easy-audio.core
        #:easy-audio.general)
  (:nicknames #:wav-examples)
  (:export #:decode-wav))
