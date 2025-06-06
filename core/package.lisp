(defpackage easy-audio.core
  (:use #:cl)
  (:local-nicknames (:sera :serapeum))
  (:export
   ;; Restarts & conditions
   #:*current-condition*
   #:with-interactive-debug

   ;; Types
   #:bit-counter
   #:ub
   #:sb
   #:sa-ub
   #:sa-sb

   ;; Utility functions
   #:define-documented-accessor
   #:define-documented-accessors
   #:interleave-channels
   #:all-bits-set-p
   #:some-bits-set-p))
