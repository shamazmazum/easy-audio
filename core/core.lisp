(in-package :easy-audio.core)

;; Types
(deftype bit-counter () '(integer 0 8))
(deftype ub (n) `(unsigned-byte ,n))
(deftype sb (n) `(signed-byte ,n))
(deftype sa-ub (n) `(simple-array (ub ,n) (*)))
(deftype sa-sb (n) `(simple-array (sb ,n) (*)))

;; For interactive restarts
(sera:defvar-unbound *current-condition*
  "*CURRENT-CONDITION* is bound to signaled contition when debugger is
invoked while within WITH-INTERACTIVE-DEBUG.")

(defmacro with-interactive-debug (&body body)
  "If any condition is signaled and the debugger is invoked while
within this macro, *CURRENT-CONDITION* will be bound to the condition
signaled."
  (let ((debugger-hook (gensym)))
    `(let ((,debugger-hook *debugger-hook*))
       (flet ((,debugger-hook (condition me)
                (declare (ignore me))
                (let ((*debugger-hook* ,debugger-hook)
                      (*current-condition* condition))
                  (invoke-debugger condition))))

         (let ((*debugger-hook* #',debugger-hook))
           ,@body)))))

;; Utility functions
(sera:-> interleave-channels-n (list)
         (values (sa-sb 32) &optional))
(defun interleave-channels-n (buffers)
  (declare (optimize (speed 3)))
  (let* ((channels (length buffers))
         (first-buffer (first buffers))
         (samples (length first-buffer))
         (output (make-array (* samples channels) :element-type '(sb 32))))
    (declare (type (sa-sb 32) first-buffer))
    (loop for s fixnum below samples
          for idx fixnum from 0 by channels do
          (loop for buffer of-type (sa-sb 32) in buffers
                for c fixnum from 0 by 1 do
	        (setf (aref output (+ idx c))
	              (aref buffer s))))
    output))

(sera:-> interleave-channels-2 ((sa-sb 32) (sa-sb 32))
         (values (sa-sb 32) &optional))
(defun interleave-channels-2 (channel1 channel2)
  (declare (optimize (speed 3)))
  (loop with samples = (length channel1)
        with output = (make-array (* samples 2) :element-type '(sb 32))
        for i below samples
        for j from 0 by 2 do
        (setf (aref output j)
              (aref channel1 i)
              (aref output (1+ j))
              (aref channel2 i))
        finally (return output)))

(sera:-> interleave-channels (list)
         (values (sa-sb 32) &optional))
(defun interleave-channels (channels)
  "Interleave samples from separate channels into one buffer."
  (declare (optimize (speed 3)))
  (case (length channels)
    (2 (interleave-channels-2 (first channels) (second channels)))
    (t (interleave-channels-n channels))))

(defmacro define-documented-accessor (structure slot docstring)
  (let ((accessor (intern (format nil "~a-%~a" structure slot)
                          (symbol-package structure)))
        (wrapper  (intern (format nil "~a-~a" structure slot)
                          (symbol-package structure))))
    `(progn
       (declaim (inline ,wrapper))
       (defun ,wrapper (,structure)
         ,docstring
         (,accessor ,structure)))))

(defmacro define-documented-accessors (structure &body slots)
  `(progn
     ,@(loop for (slot docstring) in slots collect
             `(define-documented-accessor ,structure ,slot ,docstring))))

(declaim (inline all-bits-set-p))
(defun all-bits-set-p (value bits)
  (= (logand value bits) bits))

(declaim (inline some-bits-set-p))
(defun some-bits-set-p (value bits)
  (not (zerop (logand value bits))))
