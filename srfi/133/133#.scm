;;;============================================================================

;;; File: "133#.scm"

;;; Copyright (c) 2018-2020 by Antoine Doucet, All Rights Reserved.
;;; Copyright (c) 2018-2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; SRFI 133, Vector library

(##namespace ("srfi/133#"

    make-vector
    vector
    vector-unfold
    vector-unfold-right
    vector-copy
    vector-reverse-copy
    vector-append
    vector-concatenate
    vector-append-subvectors

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
    vector-cumulate

    vector-index
    vector-index-right
    vector-skip
    vector-skip-right
    vector-binary-search
    vector-any
    vector-every
    vector-partition

    vector-set!
    vector-swap!
    vector-fill!
    vector-reverse!
    vector-copy!
    vector-reverse-copy!
    vector-unfold!
    vector-unfold-right!

    vector->list
    reverse-vector->list
    list->vector
    reverse-list->vector
    string->vector
    vector->string

))
