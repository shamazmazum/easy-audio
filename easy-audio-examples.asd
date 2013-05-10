(defsystem :easy-audio-examples
  :name :easy-audio-examples
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum at gmail dot com>"
  :components ((:file "flac/examples/package")
               (:file "flac/examples/flac2wav" :depends-on ("flac/examples/package"))

               (:file "wav/examples/package")
               (:file "wav/examples/decode" :depends-on ("wav/examples/package")))
  :depends-on (:easy-audio))
