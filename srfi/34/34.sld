(define-library (srfi 34)
  (export guard

          ;; TODO: Currently the macro system needs this internal
          ;; helper to be exported. Remove the export later.
          guard-aux)
  (import (gambit))
  (include "34.scm"))
