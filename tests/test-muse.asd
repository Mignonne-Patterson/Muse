// test-MUSE ASDF System Definition File for Muse Tests // (C) 2023 Mignonne Patterson. All rights reserved.

(defsystem "test-MUSE"
 :description #P"A testing framework and examples using Common Lisp's unittesting library to verify the correct functionality of various modules in Muse system." 
 ;; System components, including test scripts for different parts: synthesis engines, algorithmic tools etc,
tests:	((#":module tests") (":serial t") (":