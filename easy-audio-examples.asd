(defsystem :easy-audio-examples
  :name :easy-audio-examples
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
