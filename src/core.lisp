;; core.lisp - Core components for Muse music synthesis system in Common Lisp.

(defpackage :muse.core
  (:use :cl)
  (:export 
    generate-sine-wave
    generate-square-wave
    generate-sawtooth-wave
    generate-triangle-wave
    apply-envelope
  ))

(in-package :muse.core)

#+sb-cold
(declaim (sb-ext:muffle-conditions sb-ext:compiler-warning))

(defun generate-sine-wave (frequency amplitude duration sample-rate)
  "Generate a sine wave with the given frequency, amplitude, and duration."
  (let ((samples (round (* duration sample-rate))))
    (loop for n from 0 below samples
          collect (* amplitude (sin (* 2 pi frequency (/ n sample-rate)))))))

(defun generate-square-wave (frequency amplitude duration sample-rate)
  "Generate a square wave with the given frequency, amplitude, and duration."
  (let ((samples (round (* duration sample-rate))))
    (loop for n from 0 below samples
          collect (if (< (mod (/ n sample-rate) (/ 1 frequency)) (/ 1 frequency 2))
                      amplitude
                      (- amplitude)))))

(defun generate-sawtooth-wave (frequency amplitude duration sample-rate)
  "Generate a sawtooth wave with the given frequency, amplitude, and duration."
  (let ((samples (round (* duration sample-rate))))
    (loop for n from 0 below samples
          collect (* amplitude (/ (mod (/ n sample-rate) (/ 1 frequency)) (/ 1 frequency))))))

(defun generate-triangle-wave (frequency amplitude duration sample-rate)
  "Generate a triangle wave with the given frequency, amplitude, and duration."
  (let ((samples (round (* duration sample-rate)))
        (half-period (round (/ sample-rate frequency 2))))
    (loop for n from 0 below samples
          for phase = (mod n (* 2 half-period))
          collect (if (< phase half-period)
                      (* amplitude (/ phase half-period))
                      (* amplitude (- 1 (/ phase half-period)))))))

(defun apply-envelope (signal attack decay sustain release sample-rate)
  "Apply an ADSR envelope to a given signal."
  (let ((total-samples (length signal))
        (attack-samples (round (* attack sample-rate)))
        (decay-samples (round (* decay sample-rate)))
        (release-samples (round (* release sample-rate)))
        (sustain-samples (- total-samples attack-samples decay-samples release-samples)))
    (append
     (loop for n from 0 below attack-samples
           collect (* (/ n attack-samples) (nth n signal)))
     (loop for n from 0 below decay-samples
           collect (* (+ 1 (* (- sustain 1) (/ n decay-samples))) (nth (+ n attack-samples) signal)))
     (loop for n from 0 below sustain-samples
           collect (* sustain (nth (+ n attack-samples decay-samples) signal)))
     (loop for n from 0 below release-samples
           collect (* (- sustain (* (/ n release-samples) sustain)) (nth (+ n attack-samples decay-samples sustain-samples) signal))))))
