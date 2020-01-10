;;;============================================================================

;;; File: "43.sld"

;;; Copyright (c) 2018-2020 by Antoine Doucet, All Rights Reserved.
;;; Copyright (c) 2018-2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; SRFI 43, Vector library

(define-library (srfi 43)

  (export
    make-vector
    vector
    vector-unfold
    vector-unfold-right
    vector-copy
    vector-reverse-copy
    vector-append
    vector-concatenate

    vector?
    vector-empty?
    vector=

    vector-ref
    vector-length

    vector-fold
    vector-fold-right
    vector-map
    vector-map!
    vector-for-each
    vector-count

    vector-index
    vector-index-right
    vector-skip
    vector-skip-right
    vector-binary-search
    vector-any
    vector-every

    vector-set!
    vector-swap!
    vector-fill!
    vector-reverse!
    vector-copy!
    vector-reverse-copy!

    vector->list
    reverse-vector->list
    list->vector
    reverse-list->vector
    )

    (import (gambit))
    (include "43.scm")
)
