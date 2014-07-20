(defsystem :easy-audio
  :name :easy-audio
  :version #.(with-open-file (input (merge-pathnames "version.lisp-expr" *load-truename*))
               (read input))
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "easy-audio-early")
               
               (:file "general-decoders/package")
	       (:file "general-decoders/g.711" :depends-on ("general-decoders/package"))

               (:file "bitreader/package")
	       (:file "bitreader/bitreader" :depends-on ("bitreader/package"))
               (:file "bitreader/crc" :depends-on ("bitreader/package"))
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
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (asdf:load-system :easy-audio-tests)
                    (funcall
                     (intern "RUN-TESTS" (find-package "EASY-AUDIO-TESTS"))))
  :depends-on (:babel))
