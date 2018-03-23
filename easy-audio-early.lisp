;; Copyright (c) 2012-2013, Vasily Postnicov
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

(defpackage easy-audio-early
  (:use :cl)
  (:export #:*current-condition*
           #:with-interactive-debug
           #:defvar-unbound

           ;; Types
           #:non-negative-fixnum
           #:positive-fixnum
           #:non-negative-int
           #:positive-int
           #:bit-counter
           #:ub
           #:sb
           #:sa-ub
           #:sa-sb
           #:bit-value))

(in-package :easy-audio-early)

(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))
(deftype non-negative-int () '(integer 0))
(deftype positive-int () '(integer 1))
(deftype bit-counter () '(integer 0 8))
(deftype ub (n) `(unsigned-byte ,n))
(deftype sb (n) `(signed-byte ,n))
(deftype sa-ub (n) `(simple-array (ub ,n) (*)))
(deftype sa-sb (n) `(simple-array (sb ,n) (*)))
(deftype bit-value () '(integer 0 1))

;; Definition of documented unbound variables
(defmacro defvar-unbound (var &optional doc-string)
  "Defines special unbound variable with defvar,
   also assigning documentation string if supported."
  `(progn
     (defvar ,var)
     ,@(if doc-string `((setf (documentation ',var 'variable) ,doc-string)))))

;; For interactive restarts
(defvar-unbound *current-condition*
  "*CURRENT-CONDITION* is bound to signaled contition
   when debugger is invoked while within WITH-INTERACTIVE-DEBUG")

(defmacro with-interactive-debug (&body body)
  "If any condition is signaled and the debugger is invoked while
   within this macro, *CURRENT-CONDITION* will be bound to the
   condition signaled"
  (let ((debugger-hook (gensym)))
    `(let ((,debugger-hook *debugger-hook*))
       (flet ((,debugger-hook (condition me)
                (declare (ignore me))
                (let ((*debugger-hook* ,debugger-hook)
                      (*current-condition* condition))
                  (invoke-debugger condition))))

         (let ((*debugger-hook* #',debugger-hook))
           ,@body)))))
