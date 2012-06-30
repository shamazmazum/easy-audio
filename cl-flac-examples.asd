(defsystem :cl-flac-examples
  :name :cl-flac-examples
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :pathname #-asdf2 (merge-pathnames "examples/" *load-truename*)
            #+asdf2 "examples/"
  :components ((:file "package")
	       (:file "flac2wav" :depends-on ("package")))
  :depends-on (:babel :cl-flac))
