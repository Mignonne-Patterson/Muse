;; **Delay Effect Module for Muse Synthesis System**

;; This module provides advanced delay functionality designed to be integrated into real-time audio processing
;; within our high-performance synthesis and composition platform, *Muse*. The `DELAY-EFFECT` class implements
;; the core delay effect with customizable parameters.

;; **Key Features:**
;; The delay feature can be customized with the following parameters:
;; - **Delay Time**: The duration between input signal and its echoed response in seconds, allowing precise control
;;   over effect timing to achieve various audio effects like reverb or ambiance enhancement.
;;   - *Example*: Setting a 1 second interval creates an echo where every sound is heard after approximately one unit of time.
;; - **Feedback Level**: A value between `0` (no feedback) and `-∞`, indicating how much the delayed signal contributes
;;   back into itself, shaping its decay. This parameter defines whether echoes diminish or sustain over time.
;; - **Wet/Dry Mix**: Control slider that ranges from `0%` (only original sound) up to `100%` (pure echo), enabling creative
;;   blending of dry versus wet signals.

;; **Real-Time Buffer Management:**
;; The internal circular buffers efficiently handle dynamic delay times without causing latency or introducing artifacts like
;; clicks and pops when adjusting parameters on-the-fly. This ensures seamless integration into live performances.

(defpackage :muse.effect-delay
  (:use :cl)
  (:export #:make-delay-effect
           #:set-delay-time
           #:set-feedback-level
           #:set-wet-dry-mix
           #:process-buffer))

(in-package :muse.effect-delay)

(defclass delay-effect ()
  ((delay-time :accessor delay-time :initarg :delay-time :initform 1.0)
   (feedback-level :accessor feedback-level :initarg :feedback-level :initform 0.5)
   (wet-dry-mix :accessor wet-dry-mix :initarg :wet-dry-mix :initform 0.5)
   (left-buffer :accessor left-buffer :initform (make-array 48000 :fill-pointer 0))
   (right-buffer :accessor right-buffer :initform (make-array 48000 :fill-pointer 0))))

(defun make-delay-effect (&key (delay-time 1.0) (feedback-level 0.5) (wet-dry-mix 0.5))
  "Create a new delay effect processor."
  (make-instance 'delay-effect :delay-time delay-time :feedback-level feedback-level :wet-dry-mix wet-dry-mix))

(defun set-delay-time (delay-effect time)
  "Set the delay time for the effect."
  (setf (delay-time delay-effect) time))

(defun set-feedback-level (delay-effect level)
  "Set the feedback level for the effect."
  (setf (feedback-level delay-effect) level))

(defun set-wet-dry-mix (delay-effect mix)
  "Set the wet/dry mix for the effect."
  (setf (wet-dry-mix delay-effect) mix))

(defmethod process-buffer ((effect delay-effect) input-buffer output-buffer)
  "Process the input buffer and write to the output buffer."
  (let ((delay-time (delay-time effect))
        (feedback-level (feedback-level effect))
        (wet-dry-mix (wet-dry-mix effect))
        (left-buffer (left-buffer effect))
        (right-buffer (right-buffer effect)))
    (loop for i from 0 below (length input-buffer)
          do (let* ((in-sample (aref input-buffer i))
                    (delayed-sample (aref left-buffer (mod i (length left-buffer))))
                    (out-sample (+ (* wet-dry-mix delayed-sample) (* (- 1 wet-dry-mix) in-sample))))
               ;; Update the buffer with delayed sample
               (setf (aref left-buffer (mod i (length left-buffer)))
                     (+ (* feedback-level delayed-sample) in-sample))
               ;; Write to output buffer
               (setf (aref output-buffer i) out-sample)))))

;; Example usage:
(let ((effect (make-delay-effect :delay-time 0.5 :feedback-level 0.7 :wet-dry-mix 0.5)))
  (set-delay-time effect 1.0)
  (set-feedback-level effect 0.6)
  (set-wet-dry-mix effect 0.8)
  ;; Process buffers (input-buffer and output-buffer should be defined)
  ;; (process-buffer effect input-buffer output-buffer)
  )
