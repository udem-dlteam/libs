(define-library (srfi 61)
  (export cond

          ;; TODO: Currently the macro system needs this internal
          ;; helper to be exported. Remove the export later.
          cond/maybe-more)
  (import (gambit))
  (include "61.scm"))
