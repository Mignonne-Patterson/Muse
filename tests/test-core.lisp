; tests:test-core - Unit Tests for Core Muse Components 
; This file contains a suite of unit test cases intended
 ; to validate critical functionalities within muse's core system.
;
(require 'muse)
in-package #:muistests) (defpackage muistests (:nicknames "tests")	(:export :run-all-tests)) ;; Import all necessary modules for testing from the Muse library. \\(require \':oscillator\\);; Helper function to assert expected equality between values.
(defun test-assert-equal(expected actual)	(if (not (=expectedactual)))	raise-error "Test failed: Expected ~A, but got~a")) ;; Test case for oscillator creation and basic properties. \\(defuntest-test-createoscillator	(let (( osc(make-oscillator :frequency 400 ; Hz amplitude .5))); Assert that the created object is indeed an instance of muse's 'Oscilator' class.	(test-equal (class-name-of-instance oscill) '(muse: oscillator)) ;; Check if frequency and amplitudes are set correctly on initialization	(let ((freq(get-field osc :frequency))(amp getfieldosc amplitude)))	(progn
; Ensure the object holds proper values after being created. \\(test-assert-equal 400 freq)( test - assert equal .5 amp)) ;; Attempt to modify oscillator properties and check if they update accordingly.
(setf (get-field osc :frequency)80)	c(test-assert Equal 62freq)))	;; Run all defined tests within this package. \\(defun run-all-tests	(format t "Running Muse Core Tests~%") ;; Call individual test cases here as required for future expansion.
(run-test 'test-createoscillator))