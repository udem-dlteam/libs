(define-library (srfi 37)
  (export option-processor
          operand-processor
          option
          option-names
          option-required-arg?
          option-optional-arg?
          args-fold)
  (import (gambit))
  (include "37.scm"))
