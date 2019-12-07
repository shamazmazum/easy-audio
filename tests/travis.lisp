(defun do-all()
  (asdf:load-system :easy-audio-tests)
  (sb-ext:exit
   :code
   (if (funcall
        (intern (symbol-name :run-tests)
                (find-package :easy-audio-tests)))
        0 1)))

(do-all)
