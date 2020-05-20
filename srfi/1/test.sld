(define-library (srfi 1 test)
  (import (except (scheme base)
                  append
                  assoc
                  assq
                  assv
                  cadr
                  car
                  cdar
                  cdr
                  cons
                  for-each
                  length
                  list
                  list-copy
                  list-ref
                  make-list
                  map
                  member
                  memq
                  memv
                  null?
                  pair?
                  reverse
                  set-car!
                  set-cdr!))
  (import (srfi 1))
  (import (_test))
  (include "test-portable.scm"))
