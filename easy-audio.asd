;; Comment it out if you do not want restrictions
(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :easy-audio-unsafe-code *features*)
  #+nil
  (pushnew :easy-audio-check-crc   *features*))

(defsystem :easy-audio
  :name :easy-audio
  :version #.(with-open-file (input (merge-pathnames "version.lisp-expr" *load-truename*))
               (read input))
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :description "A pack of audio decoders for FLAC, WavPack and other formats"
  :licence "2-clause BSD"
  :components ((:file "easy-audio-early")
               
               (:file "general-decoders/package")
	       (:file "general-decoders/g.711" :depends-on ("general-decoders/package"))

               (:file "bitreader/package")
	       (:file "bitreader/bitreader" :depends-on ("bitreader/package"))
               #+easy-audio-check-crc
               (:file "bitreader/crc" :depends-on ("bitreader/package"))

               (:file "ogg/package" :depends-on ("bitreader/package"))
	       (:file "ogg/ogg" :depends-on ("ogg/package"))

               (:file "utils/package" :depends-on ("bitreader/package"))
               (:file "utils/utils" :depends-on ("wav/definitions"))

	       (:file "flac/package" :depends-on ("utils/package"))
	       (:file "flac/definitions" :depends-on ("flac/package"))
	       (:file "flac/flac-reader" :depends-on ("flac/package"))
	       (:file "flac/metadata" :depends-on ("flac/package"))
	       (:file "flac/frame" :depends-on ("flac/package"))
	       (:file "flac/decode" :depends-on ("flac/package"))
	       (:file "flac/flac" :depends-on ("flac/package"))
               (:file "flac/flac-ogg" :depends-on ("flac/package"))

               (:file "wav/package" :depends-on ("utils/package"))
               (:file "wav/definitions" :depends-on ("wav/package"))
               (:file "wav/wav" :depends-on ("wav/package"))

               (:file "ape/package" :depends-on ("utils/package"))
               (:file "ape/definitions" :depends-on ("ape/package"))
               (:file "ape/ape" :depends-on ("ape/package"))
               (:file "ape/decode" :depends-on ("ape/package"))
               (:file "ape/apev2" :depends-on ("ape/package"))

               (:file "wv/package" :depends-on ("utils/package"))
               (:file "wv/definitions" :depends-on ("wv/package"))
               (:file "wv/wavpack-reader" :depends-on ("wv/package"))
               (:file "wv/metadata" :depends-on ("wv/package"))
               (:file "wv/wv-block" :depends-on ("wv/package"))
               (:file "wv/wv-blocks-multichannel" :depends-on ("wv/package"))
               (:file "wv/decode" :depends-on ("wv/package")))
  :in-order-to ((test-op (load-op "easy-audio/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (intern (symbol-name '#:run-tests)
                             (find-package '#:easy-audio-tests))))
  :depends-on (:flexi-streams))

(defsystem :easy-audio/tests
  :name :easy-audio/tests
  :version #.(with-open-file (input (merge-pathnames "version.lisp-expr" *load-truename*))
               (read input))
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "tests/package")
               (:file "tests/tests" :depends-on ("tests/package")))
  :depends-on (:easy-audio :fiveam :flexi-streams))

(defsystem :easy-audio/examples
  :name :easy-audio/examples
  :version #.(with-open-file (input (merge-pathnames "version.lisp-expr" *load-truename*))
               (read input))
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "flac/examples/package")
               (:file "flac/examples/flac2wav" :depends-on ("flac/examples/package"))

               (:file "wav/examples/package")
               (:file "wav/examples/decode" :depends-on ("wav/examples/package"))

               (:file "wv/examples/package")
               (:file "wv/examples/wv2wav" :depends-on ("wv/examples/package")))
  :depends-on (:easy-audio))
