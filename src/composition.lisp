;; composition.lisp - Composition Tools for Muse Music Synthesis System (Common Lisp)

(in-package :cl-user)

(defpackage :muse.composition
  (:use :cl :muse.core)
  (:export 
    create-sequence
    play-sequence
    add-note
    set-tempo
  ))

(in-package :muse.composition)

(defun create-sequence ()
  "Create a new empty sequence."
  (make-array 0 :adjustable t :fill-pointer 0))

(defun add-note (sequence note duration)
  "Add a note to the sequence."
  (vector-push-extend (list note duration) sequence))

(defun set-tempo (sequence tempo)
  "Set the tempo for the sequence."
  (setf (getf (array-dimensions sequence) :tempo) tempo))

(defun play-sequence (sequence)
  "Play the sequence."
  (loop for item across sequence
        do (let ((note (first item))
                 (duration (second item)))
             (play-note note duration))))

(defun play-note (note duration)
  "Play a single note for the given duration."
  ;; Placeholder for actual sound synthesis function
  (format t "Playing note ~a for ~a seconds~%" note duration))

;; Example usage
(let ((seq (create-sequence)))
  (add-note seq 'c4 0.5)
  (add-note seq 'e4 0.5)
  (add-note seq 'g4 0.5)
  (set-tempo seq 120)
  (play-sequence seq))
