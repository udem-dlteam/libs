(define-library (gambit optionals)
  (export :optional let-optionals*

          ;; TODO: Currently the macro system needs this internal
          ;; helper to be exported. Remove the export later.
          really-let-optionals*)
  (import (scheme base))
  (include "optionals.scm"))
