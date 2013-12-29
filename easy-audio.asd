(defsystem :easy-audio
  :name :easy-audio
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "reader-macro")
               
               (:file "general-decodecs/package")
	       (:file "general-decodecs/g.711" :depends-on ("general-decodecs/package"))

               (:file "bitreader/package")
	       (:file "bitreader/bitreader" :depends-on ("bitreader/package"))
	       (:file "flac/package" :depends-on ("bitreader/package"))
	       (:file "flac/definitions" :depends-on ("flac/package"))
	       (:file "flac/flac-reader" :depends-on ("flac/package"))
	       (:file "flac/metadata" :depends-on ("flac/package"))
	       (:file "flac/frame" :depends-on ("flac/package"))
	       (:file "flac/decode" :depends-on ("flac/package"))
	       (:file "flac/flac" :depends-on ("flac/package"))

               (:file "wav/package" :depends-on ("bitreader/package"))
               (:file "wav/definitions" :depends-on ("wav/package"))
               (:file "wav/wav" :depends-on ("wav/package"))

               (:file "utils/package" :depends-on ("wav/package"))
               (:file "utils/utils" :depends-on ("utils/package")))
  :depends-on (:babel))
