(defsystem :easy-audio-tests
  :name :easy-audio-tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "tests/package")
               (:file "tests/tests" :depends-on ("tests/package")))
  :depends-on (:easy-audio :fiveam :flexi-streams))
