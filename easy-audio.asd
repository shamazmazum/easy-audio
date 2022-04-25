;; Comment it out if you do not want restrictions
(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :easy-audio-unsafe-code *features*)
  #+nil
  (pushnew :easy-audio-check-crc   *features*))

(defsystem :easy-audio/core
  :name :easy-audio/core
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "core/"
  :depends-on (:serapeum)
  :components ((:file "package")
               (:file "core")))

(defsystem :easy-audio/general-decoders
  :name :easy-audio/general-decoders
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "general-decoders/"
  :components ((:file "package")
               (:file "g.711"))
  :depends-on (:easy-audio/core))

(defsystem :easy-audio/bitreader
  :name :easy-audio/bitreader
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "bitreader/"
  :components ((:file "package")
               (:file "bitreader")
               #+easy-audio-check-crc
               (:file "crc")
               (:file "macros"))
  :depends-on (:easy-audio/core
               :alexandria))

(defsystem :easy-audio/ogg
  :name :easy-audio/ogg
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "ogg/"
  :components ((:file "package")
               (:file "ogg"))
  :depends-on (:easy-audio/core
               :easy-audio/bitreader
               :alexandria))

(defsystem :easy-audio/flac
  :name :easy-audio/flac
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "flac/"
  :components ((:file "package")
               (:file "definitions")
               (:file "flac-reader")
	       (:file "metadata")
	       (:file "frame")
	       (:file "decode")
	       (:file "flac")
               (:file "flac-ogg"))
  :depends-on (:easy-audio/core
               :easy-audio/bitreader
               :alexandria
               :serapeum
               :flexi-streams))

(defsystem :easy-audio/wav
  :name :easy-audio/wav
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "wav/"
  :components ((:file "package")
               (:file "definitions")
               (:file "wav")
               (:file "write-header"))
  :depends-on (:easy-audio/core
               :easy-audio/bitreader
               :easy-audio/general-decoders
               :flexi-streams))

(defsystem :easy-audio/ape
  :name :easy-audio/ape
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "ape/"
  :components ((:file "package")
               (:file "definitions")
               (:file "ape")
               (:file "frame")
               (:file "decode")
               (:file "ape-tags-v2"))
  :depends-on (:easy-audio/core
               :easy-audio/bitreader
               :alexandria
               :flexi-streams))

(defsystem :easy-audio/wv
  :name :easy-audio/wv
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "wv/"
  :components ((:file "package")
               (:file "definitions")
               (:file "wavpack-reader")
               (:file "metadata")
               (:file "wv-block")
               (:file "wv-blocks-multichannel")
               (:file "decode"))
  :depends-on (:easy-audio/core
               :easy-audio/bitreader
               :alexandria
               :serapeum))

(defsystem :easy-audio
  :name :easy-audio
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A pack of audio decoders for FLAC, WavPack and other formats"
  :licence "2-clause BSD"
  :in-order-to ((test-op (load-op "easy-audio/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :easy-audio-tests '#:run-tests))
  :depends-on (:easy-audio/core
               :easy-audio/ogg
               :easy-audio/flac
               :easy-audio/wav
               :easy-audio/ape
               :easy-audio/wv))

(defsystem :easy-audio/examples
  :name :easy-audio/examples
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :components ((:file "flac/examples/package")
               (:file "flac/examples/flac2wav" :depends-on ("flac/examples/package"))
               (:file "flac/examples/ogg2wav" :depends-on ("flac/examples/package"))

               (:file "wav/examples/package")
               (:file "wav/examples/decode" :depends-on ("wav/examples/package"))

               (:file "ape/examples/package")
               (:file "ape/examples/ape2wav" :depends-on ("ape/examples/package"))

               (:file "wv/examples/package")
               (:file "wv/examples/wv2wav" :depends-on ("wv/examples/package")))
  :depends-on (:easy-audio))

(defsystem :easy-audio/tests
  :name :easy-audio/tests
  :version "1.0"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:easy-audio/examples
               :fiveam
               :md5))
