(defpackage easy-audio.utils
  (:nicknames #:utils)
  (:use #:cl #:easy-audio-early)
  (:export #:mixchannels
           #:write-pcm-wav-header
           #:defreader))

(defpackage easy-audio.utils-sbcl
  (:nicknames #:utils-sbcl)
  (:use #:cl #:easy-audio-early)
  (:import-from #:sb-vm
                #:signed-reg #:signed-num #:descriptor-reg #:any-reg
                #:simple-array-signed-byte-32
                #:punpckldq #:movd #:movntdq
                #:int-sse-reg #:simd-pack-int #:simd-pack
                #:flushable #:foldable)
  (:import-from #:sb-c
                #:defknown
                #:define-vop
                #:inst #:move)
  (:export #:make-simd-pack-sb32 #:store-simd-pack-to-sa-sb32))
