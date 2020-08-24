(defun do-all()
  (ql:quickload :easy-audio/tests)
  (uiop:quit
   (if (uiop:call-function "easy-audio-tests:run-tests")
        0 1)))

(do-all)
