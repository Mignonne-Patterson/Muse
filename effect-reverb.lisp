; Effect Reverb Module for Muse Sound Synthesis System
(defpackage :muse.effect-reverb
  (:use :cl)
  (:export #:make-reverb-effect
           #:set-reverb-time
           #:set-damping
           #:set-mix
           #:process-buffer))

(in-package :muse.effect-reverb)

;; **Reverb Effect Module for Muse Sound Synthesis System**
;;
;; This module provides advanced reverb functionality designed to be integrated
;; into real-time audio processing within our high-performance synthesis and
;; composition platform, *Muse*. The `REVERB-EFFECT` class implements the core
;; reverb effect with customizable parameters.

;; **Key Features:**
;; The reverb feature can be customized with the following parameters:
;; - **Reverb Time**: The duration of the reverb tail in seconds, allowing precise control
;;   over the decay of the reverb effect.
;; - **Damping**: A value between `0` (no damping) and `1.0`, indicating how much the
;;   high frequencies are reduced in the reverb tail, shaping its tone.
;; - **Mix**: Control slider that ranges from `0%` (only original sound) up to `100%`
;;   (pure reverb), enabling creative blending of dry versus wet signals.

(defclass reverb-effect ()
  ((reverb-time :accessor reverb-time :initarg :reverb-time :initform 1.0)
   (damping :accessor damping :initarg :damping :initform 0.5)
   (mix :accessor mix :initarg :mix :initform 0.5)
   (left-buffer :accessor left-buffer :initform (make-array 48000 :fill-pointer 0))
   (right-buffer :accessor right-buffer :initform (make-array 48000 :fill-pointer 0))))

(defun make-reverb-effect (&key (reverb-time 1.0) (damping 0.5) (mix 0.5))
  "Create a new reverb effect processor."
  (make-instance 'reverb-effect :reverb-time reverb-time :damping damping :mix mix))

(defun set-reverb-time (reverb-effect time)
  "Set the reverb time for the effect."
  (setf (reverb-time reverb-effect) time))

(defun set-damping (reverb-effect value)
  "Set the damping level for the effect."
  (setf (damping reverb-effect) value))

(defun set-mix (reverb-effect value)
  "Set the wet/dry mix for the effect."
  (setf (mix reverb-effect) value))

(defmethod process-buffer ((effect reverb-effect) input-buffer output-buffer)
  "Process the input buffer and write to the output buffer."
  (let ((reverb-time (reverb-time effect))
        (damping (damping effect))
        (mix (mix effect))
        (left-buffer (left-buffer effect))
        (right-buffer (right-buffer effect)))
    (loop for i from 0 below (length input-buffer)
          do (let* ((in-sample (aref input-buffer i))
                    (reverb-sample (aref left-buffer (mod i (length left-buffer))))
                    (out-sample (+ (* mix reverb-sample) (* (- 1 mix) in-sample))))
               ;; Update the buffer with reverb sample
               (setf (aref left-buffer (mod i (length left-buffer)))
                     (+ (* damping reverb-sample) in-sample))
               ;; Write to output buffer
               ▋
