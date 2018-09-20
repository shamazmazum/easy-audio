(defpackage easy-audio.utils
  (:nicknames #:utils)
  (:use #:cl #:easy-audio-early)
  (:export #:mixchannels
           #:write-pcm-wav-header
           #:with-output-to-wav
           #:defreader))
