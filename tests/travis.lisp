(defun do-all()
  (handler-case
      (asdf:load-system :easy-audio/tests)
    (error () (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "easy-audio-tests:run-tests")
        0 1)))

(do-all)
