(defpackage :muse.utilities
  (:nicknames :mu)
  (:export #:db-to-linear
           ;; Other utility functions will be exported here...
           ))

(in-package :muse.utilities)

;; Implementations for various common utilities used in the project

;; dB to linear conversion (for sound processing)
(defun db->linear (db-level &optional (lin-range 1.0))
  (if (< db-level -87.6)
      -inf
      (* lin-range (expt 10 (/ db-level 20)))))
