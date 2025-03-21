(defsystem "muse"
  :description "A high-performance music synthesis and composition system written in Common Lisp."
  :author "Mignonne Patterson <your-email@example.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("cl-ppcre" "cl-sdl2" "trivial-garbage" "cl-math")
  :components ((:file "packages")
               (:file "utils")
               (:file "synthesis")
               (:file "composition")
               (:file "main"))
  :in-order-to ((test-op (load-op "muse.test"))))
