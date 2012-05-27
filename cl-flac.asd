(defsystem :cl-flac
  :name :cl-flac
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "package")
	       (:file "definitions" :depends-on ("package"))
	       (:file "metadata" :depends-on ("package"))
	       (:file "frame" :depends-on ("package"))
	       (:file "decode" :depends-on ("package"))
	       (:file "flac-reader" :depends-on ("package"))
	       (:file "flac" :depends-on ("package")))
  :depends-on (:babel))
