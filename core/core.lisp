(in-package :easy-audio.core)

;; Types
(deftype bit-counter () '(integer 0 8))
(deftype ub (n) `(unsigned-byte ,n))
(deftype sb (n) `(signed-byte ,n))
(deftype sa-ub (n) `(simple-array (ub ,n) (*)))
(deftype sa-sb (n) `(simple-array (sb ,n) (*)))

;; For interactive restarts
(sera:defvar-unbound *current-condition*
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

;; Utility functions
(defun mixchannels-n (out buffers)
  (declare (type list buffers)
	   (type (simple-array (signed-byte 32)) out)
	   (optimize #+easy-audio-unsafe-code
                     (safety 0) (speed 3)))
  (let ((offset (length buffers))
	(size (length (the (simple-array (signed-byte 32))
			   (nth 0 buffers))))
	(idx 0))
    (declare (type fixnum offset size idx))
    (dotimes (i size)
      (dotimes (j offset)
	(declare (type fixnum i j))
	(setf (aref out (+ idx j))
	      (aref (the (simple-array (signed-byte 32))
		         (nth j buffers))
                    i)))
      (incf idx offset))
    out))

(defun mixchannels-2 (output channel1 channel2)
  (declare (optimize #+easy-audio-unsafe-code
                     (safety 0) (speed 3))
           (type (sa-sb 32) output channel1 channel2))
  (loop for i below (length channel1)
        for j from 0 by 2 do
       (setf (aref output j)
             (aref channel1 i)
             (aref output (1+ j))
             (aref channel2 i)))
  output)

(defun mixchannels (out buffers)
  "Maps a list of @c(buffers) (each one for each channel) into one buffer @c(out)
writing sequentially the first sample of the first channel then the first sample of
second channel and so on until final channel is reached. When process repeats for
second sample of each channel until all data is written"
  (declare (optimize (speed 3))
           (type list buffers))
  (case (length buffers)
    (2 (mixchannels-2 out
                      (first buffers)
                      (second buffers)))
    (t (mixchannels-n out buffers))))

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
