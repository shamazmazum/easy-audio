(defsystem :easy-audio
  :name :easy-audio
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "bitreader/packages")
	       (:file "bitreader/bitreader" :depends-on ("bitreader/packages"))
	       (:file "flac/package" :depends-on ("bitreader/packages"))
	       (:file "flac/definitions" :depends-on ("flac/package"))
	       (:file "flac/flac-reader" :depends-on ("flac/package"))
	       (:file "flac/metadata" :depends-on ("flac/package"))
	       (:file "flac/frame" :depends-on ("flac/package"))
	       (:file "flac/decode" :depends-on ("flac/package"))
	       (:file "flac/flac" :depends-on ("flac/package")))
  :depends-on (:babel))
