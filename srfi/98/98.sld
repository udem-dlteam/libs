;; Re-export Gambit's built-in procedures.

(define-library (srfi 98)
  (export get-environment-variable
          get-environment-variables)
  (import (gambit)))
