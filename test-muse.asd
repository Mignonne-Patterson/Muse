;; test-MUSE ASDF System Definition File for Muse Tests
;; (C) 2023 Mignonne Patterson. All rights reserved.

(defsystem "test-muse"
  :description "A testing framework and examples using Common Lisp's unittesting library to verify the correct functionality of various modules in the Muse system."
  :components ((:module "tests" :serial t))
  :depends-on ("fiveam" "cl-ppcre"))

(defmodule test-oscillator ()
  "Tests for oscillator components in the Muse system.")
