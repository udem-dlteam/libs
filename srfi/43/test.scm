;;;============================================================================

;;; File: "test.scm"

;;; Copyright (c) 2018-2020 by Antoine Doucet, All Rights Reserved.
;;; Copyright (c) 2018-2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; SRFI 43, Vector library.

(import (srfi 43))
(import (_test))

;;;============================================================================
;;; Constructors
;;;============================================================================
;;;
;;;----------------------------------------------------------------------------
;;; make-vector

    ;;; primitive R5Rs ##make-vector

    (check-equal? (make-vector 4)
      #(0 0 0 0))

    (check-equal? (make-vector 4 1)
      #(1 1 1 1))

;;;----------------------------------------------------------------------------
;;; vector

    ;;; primitive R5Rs ##vector

    (check-equal? (vector 0)
      #(0))

    (check-equal? (vector 0 1 2 3)
      #(0 1 2 3))


;;;----------------------------------------------------------------------------
;;; vector-unfold

    (check-equal?
        (vector-unfold 
           (lambda (i x) (values x (+ x 1)))
                       5
                       0)
        #(0 1 2 3 4))

    (let ((vec (vector 0 1 2 3 4)))
         (check-equal?
           (vector-unfold (lambda (i) (vector-ref vec i))
                     (vector-length vec))
           vec))

    (check-exn
      wrong-number-of-arguments-exception?
         (lambda () 
          (vector-unfold (lambda () '()) 1)))

    (check-exn
      wrong-number-of-arguments-exception?
      (lambda ()
        (vector-unfold (lambda (x1) '()) 1 2)))

    (check-exn 
      wrong-number-of-arguments-exception?
      (lambda ()
        (vector-unfold (lambda (x1) '()))))
    (check-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-unfold (lambda (x1 x2 x3) (values 1 2 3))
                                  1 2 )))
    (check-tail-exn
      type-exception?
      (lambda () (vector-unfold 0 1 2)))


;;;----------------------------------------------------------------------------
;;; vector-unfold-right

    (check-equal?
        (vector-unfold-right (lambda (i x) (values x (+ x 1))) 5 0)
        #(4 3 2 1 0))

    (let ((vec #(1 2 3 4 5)))
        (check-equal?
          (vector-unfold-right (lambda (i x) (values (vector-ref vec x) (+ x 1)))
                                             (vector-length vec)
                                             0)
          #(5 4 3 2 1)))

    (check-exn
      wrong-number-of-arguments-exception?
         (lambda () 
          (vector-unfold-right (lambda () '()) 1)))

    (check-exn
      wrong-number-of-arguments-exception?
      (lambda ()
        (vector-unfold-right (lambda (x1) '()) 1 2)))

    (check-exn 
      wrong-number-of-arguments-exception?
      (lambda ()
        (vector-unfold-right (lambda (x1) '()))))
    (check-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-unfold-right (lambda (x1 x2 x3) (values 1 2 3))
                                  1 2 )))

    (check-tail-exn
      type-exception?
        (lambda () (vector-unfold 0 1 2)))

;;;----------------------------------------------------------------------------
;;; vector-copy

    (let ((vec #(0 1 2 3 4)))
      (check-equal?
        (vector-copy vec)
        vec)
      (check-equal?
        vec #(0 1 2 3 4)))

    (let ((vec #(0 1 2 3 4 5)))
        (check-equal?
          (vector-copy vec 3)
          #(3 4 5))
        (check-equal?
          vec #(0 1 2 3 4 5)))

    (let ((vec #(0 1 2 3 4 5)))
      (check-equal?
        (vector-copy vec 3 10 6)
        #(3 4 5 6 6 6 6)))


    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-copy)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-copy 0 1 2 3 4)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-copy 0)))

;;;----------------------------------------------------------------------------
;;; vector-reverse-copy

    (let ((vec #(5 4 3 2 1)))
        (check-equal?
          (vector-reverse-copy vec 2 5)
          #(1 2 3))
        (check-equal?
          vec 
          #(5 4 3 2 1)))

    (let ((vec #(3 2 1)))
      (check-equal?
        (vector-reverse-copy vec )
        #(1 2 3)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-reverse-copy)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-reverse-copy 0 1 2 3)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-reverse-copy 0)))

;;;----------------------------------------------------------------------------
;;; vector-append

    ;;; primitive R5Rs ##vector-append

;;;----------------------------------------------------------------------------
;;; vector-concatenate

    ;;; primitive ##append-vectors

;;;============================================================================
;;; Predicates
;;;============================================================================
;;;
;;;----------------------------------------------------------------------------
;;; vector?

    ;;; primitive R5Rs ##vector?

;;;----------------------------------------------------------------------------
;;; vector-empty?

    (check-true (vector-empty? #()))
    (check-false (vector-empty? #(0)))
    (check-false (vector-empty? #(#())))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-empty?)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-empty? 0 1)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-empty? 0)))

;;;----------------------------------------------------------------------------
;;; vector=

    (check-true (vector= equal? #() #()))

    (let ((vec #()))
        (check-true (vector= eq? vec vec)))
    (check-true (vector= (lambda (a b) #t) #(1 2 3) #(4 5 6)))
    (check-false (vector= eq? #(0 1 2 3) #()))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector= 0 1)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector= 0 1 2 3)))

    (check-tail-exn
      type-exception?
        (lambda () (vector= 0 #() #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector= eq? 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector= eq? #() 0)))


;;;============================================================================
;;; Selectors
;;;============================================================================
;;;
;;;----------------------------------------------------------------------------
;;; vector-ref

    ;;; primitive R5Rs ##vector-ref

;;;----------------------------------------------------------------------------
;;; vector-length

    ;;; primitive R5Rs ##vector-length


;;;============================================================================
;;; Iteration
;;;============================================================================
;;;
;;;----------------------------------------------------------------------------
;;; vector-fold

    (let ((vec #(0 1 2 3)))
      (check-equal?
        (vector-fold (lambda (index tail elt) (cons elt tail))
                     '() vec) 
        '(3 2 1 0))
      (check-equal?
        vec 
        #(0 1 2 3)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-fold 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-fold 0 1 #() #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-fold (lambda (x1 x2) '()) 1 0 #())))

    (check-exn
      type-exception?
        (lambda () (vector-fold (lambda (x1 x2) '()) 1 #() 0)))


;;;----------------------------------------------------------------------------
;;; vector-fold-right


    (check-equal?
      (vector-fold-right (lambda (index tail elt) (cons elt tail))
                                 '() #(0 1 2 3))
      '(0 1 2 3))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-fold-right 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-fold-right 0 1 #() #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-fold-right (lambda (x1 x2) '()) 1 0 #())))

    (check-exn
      type-exception?
        (lambda () (vector-fold-right (lambda (x1 x2) '()) 1 #() 0)))
     
;;;----------------------------------------------------------------------------
;;; vector-map

    (let ((vec #(0 1 2 3 4)))
      (check-equal?
        (vector-map (lambda (i x) (* x x)) vec)
        #(0 1 4 9 16))
      (check-equal?
        vec
        #(0 1 2 3 4)))

    (let ((vec #(1 2 3 4 5)))
      (check-equal?
        (vector-map (lambda (i x) (- x i)) vec )
        #(1 1 1 1 1))
      (check-equal?
        vec
        #(1 2 3 4 5)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
      (lambda () (vector-map 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-map 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-map (lambda (x1 x2) '()) 0)))

    (check-exn
      type-exception?
        (lambda () (vector-map (lambda (x1 x2) '()) #() 0)))


;;;----------------------------------------------------------------------------
;;; vector-map!

    (check-equal?
      (vector-map (lambda (i x) (* x x)) #(0 1 2 3 4))
      #(0 1 4 9 16))

    (check-equal?
      (vector-map (lambda (i x) (- x i)) #(1 2 3 4 5) )
      #(1 1 1 1 1))

    (check-tail-exn
      wrong-number-of-arguments-exception?
      (lambda () (vector-map 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-map 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-map (lambda (i x) '()) 0)))

    (check-exn
      type-exception?
        (lambda () (vector-map (lambda (i x) '()) #() 0)))


;;;----------------------------------------------------------------------------
;;; vector-for-each

    (let ((vec #(0 1 2 3)))
        (vector-for-each (lambda (i x) (vector-set! vec i (* x x)))
                         vec)
        (check-equal? vec
           #(0 1 4 9)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-for-each 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-for-each 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-for-each (lambda () '()) 0)))

    (check-exn
      type-exception?
        (lambda () (vector-for-each (lambda () '()) #() 0)))

;;;----------------------------------------------------------------------------
;;; vector-count

    (check-equal? 
      (vector-count (lambda (i elt) (even? elt)) #(1 2 3 4))
      2)

    (check-equal?
      (vector-count (lambda (i x y) (< x y)) '#(0 1 2 3 4) '#(1 2 3 4 4 4))
      4)

    (check-tail-exn
      wrong-number-of-arguments-exception?
      (lambda () (vector-count 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-count 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-count (lambda () '()) 0)))


    (check-exn
      type-exception?
        (lambda () (vector-count (lambda () '()) #() 0)))


;;;============================================================================
;;; Searching
;;;============================================================================
;;;
;;;----------------------------------------------------------------------------
;;; vector-index

    (check-equal?
      (vector-index even? #(1 2 3 4))
      1)

    (check-equal?
      (vector-index < #(1 2 3 4) #(2 2 2 2))
      0) 

    (check-false
      (vector-index = #(1 1 1) #(2 2 2 2 2)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-index 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-index 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-index (lambda () '()) 0)))

    (check-exn
      type-exception?
        (lambda () (vector-index (lambda () '()) #() 0)))

;;;----------------------------------------------------------------------------
;;; vector-index-right

    (check-equal?
      (vector-index-right even? #(1 2 3 4))
      3)

    (check-equal?
      (vector-index-right <= #(1 2 3 4) #(2 2 2 2))
      1) 

    (check-false
      (vector-index-right = #(1 1 1) #(2 2 2 2 2)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-index-right 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-index-right 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-index-right (lambda () '()) 0)))

    (check-exn
      type-exception?
        (lambda () (vector-index-right (lambda () '()) #() 0)))


;;;----------------------------------------------------------------------------
;;; vector-skip

    (check-equal?
      (vector-skip number? #(0 1 2 a b 1 2 3))
      3)

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-skip 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-skip 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-skip (lambda () '()) 0)))

    (check-exn
      type-exception?
        (lambda () (vector-skip (lambda () '()) #() 0)))

;;;----------------------------------------------------------------------------
;;; vector-skip-right

    (check-equal?
      (vector-skip-right number? #(0 1 2 a b 0 1 2))
      4)

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-skip-right 0)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-skip-right 0 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-skip-right (lambda () '()) 0)))

    (check-exn
      type-exception?
        (lambda () (vector-skip-right (lambda () '()) #() 0)))


;;;----------------------------------------------------------------------------
;;; vector-binary-search

    (let ((comp? (lambda (x1 x2)
                   (cond ((< x1 x2) -1)
                         ((= x1 x2) 0)
                         (else 1)))))
      (check-equal?
        (vector-binary-search #(1 2 3 4) 2 comp?)
        1))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-binary-search 0 1)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-binary-search 0 1 2 3 4 5)))


    (check-tail-exn
      type-exception?
        (lambda () (vector-binary-search 0 0 (lambda () '()))))

    (check-tail-exn
      type-exception?
        (lambda () (vector-binary-search #() 0 0)))

;;;----------------------------------------------------------------------------
;;; vector-any

    (check-true
      (vector-any = #(0 0 0 0) #(1 0 1) #(2 0 2 0)))

    (check-false
      (vector-any = #(0 0 0) #(1 1 1)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-any 0)))

    (check-tail-exn
      type-exception? 
        (lambda () (vector-any 0 #() #())))

    (check-tail-exn
      type-exception? 
        (lambda () (vector-any (lambda () '()) 0 #())))

    (check-exn
      type-exception? 
        (lambda () (vector-any (lambda () '()) #() 0)))
     

;;;----------------------------------------------------------------------------
;;; vector-every

    (check-false
      (vector-every = #(0 0 0 0) #(1 0 1) #(2 0 2 0)))

    (check-true
      (vector-every = #(1 1 1) #(1 1 1)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-every 0)))

    (check-tail-exn
      type-exception? 
        (lambda () (vector-every 0 #() #())))

    (check-tail-exn
      type-exception? 
        (lambda () (vector-every (lambda () '()) 0 #())))

    (check-exn
      type-exception? 
        (lambda () (vector-every (lambda () '()) #() 0)))

;;;============================================================================
;;; Mutators
;;;============================================================================
;;;
;;;----------------------------------------------------------------------------
;;; vector-set!

    ;;; primitive R5Rs ##vector-set!

;;;----------------------------------------------------------------------------
;;; vector-swap!

    (let ((vec #(0 1 2 3 4)))
        (vector-swap! vec 0 1)
        (check-equal?
          vec
          #(1 0 2 3 4)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-swap! 0 1)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-swap! 0 1 2 3)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-swap! 0 0 1)))

;;;----------------------------------------------------------------------------
;;; vector-fill!

    (let ((vec #(0 0 0 0)))
        (vector-fill! vec 1 0 2)
        (check-equal?
          vec
          #(1 1 0 0)))

    (let ((vec #(0 0 0 0)))
        (vector-fill! vec 1)
        (check-equal?
          vec
          #(1 1 1 1)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-fill! 0)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-fill! 0 1 2 3 4)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-fill! 0 1)))

;;;----------------------------------------------------------------------------
;;; vector-reverse!


    (let ((vec #(0 1 2 3 4)))
          (vector-reverse! vec)
        (check-equal?
          vec
          #(4 3 2 1 0)))

    (let ((vec #(0 1 2 3 4)))
          (vector-reverse! vec 0 3)
        (check-equal?
          vec
          #(2 1 0 3 4)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-reverse!)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-reverse! 0 1 2 3)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-reverse! 0)))

;;;----------------------------------------------------------------------------
;;; vector-copy!

    (let ((vec1 #(0 1 2 3 4))
          (vec2 #(5 6 7 8 9)))
      (vector-copy! vec2 0 vec1)
      (check-equal? vec2 vec1))

    (let ((vec1 #(0 1 2 3 4))
          (vec2 #(5 6 7 8 9)))
      (vector-copy! vec2 2 vec1 0 2 )
      (check-equal?
        vec2
        #(5 6 0 1 9)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-copy! 0 1)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-copy! 0 1 2 3 4 5)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-copy! 0 1 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-copy! #() 1 0)))

;;;----------------------------------------------------------------------------
;;; vector-reverse-copy!

    (let ((vec1 #(0 1 2 3 4))
          (vec2 #(5 6 7 8 9)))
      (vector-reverse-copy! vec2 0 vec1)
      (check-equal? 
        vec2
        #(4 3 2 1 0)))

    (let ((vec1 #(0 1 2 3 4))
          (vec2 #(5 6 7 8 9)))
      (vector-reverse-copy! vec2 2 vec1 0 2 )
      (check-equal?
        vec2
        #(5 6 1 0 9)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-reverse-copy! 0 1)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (vector-reverse-copy! 0 1 2 3 4 5)))

    (check-tail-exn
      type-exception?
        (lambda () (vector-reverse-copy! 0 1 #())))

    (check-tail-exn
      type-exception?
        (lambda () (vector-reverse-copy! #() 1 0)))


;;;============================================================================
;;; Conversion
;;;============================================================================
;;;
;;;----------------------------------------------------------------------------
;;; vector->list

    ;;; primitive R5Rs ##vector->list

;;;----------------------------------------------------------------------------
;;; reverse-vector->list

    (check-equal?
      (reverse-vector->list #(0 1 2 3 4))
      '(4 3 2 1 0))

    (check-equal?
      (reverse-vector->list #(0 1 2 3 4) 1 3)
      '(2 1))

    (check-equal?
      (reverse-vector->list #())
      '())

    (check-equal?
      (reverse-vector->list #(0))
      '(0))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (reverse-vector->list)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (reverse-vector->list 0 1 2 3)))

    (check-tail-exn
      type-exception?
        (lambda () (reverse-vector->list 0)))

    (check-tail-exn
      type-exception?
        (lambda () (reverse-vector->list 0)))

;;;----------------------------------------------------------------------------
;;; list->vector

    ;;; primitive R5Rs ##list->vector

        (check-equal?
            (list->vector '(0 1 2 3))
            #(0 1 2 3))

;;;----------------------------------------------------------------------------
;;; reverse-list->vector

    (check-equal?
      (reverse-list->vector '(0 1 2))
      #(2 1 0))

    (check-equal?
      (reverse-list->vector '())
      #())

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (reverse-list->vector)))

    (check-tail-exn
      wrong-number-of-arguments-exception?
        (lambda () (reverse-list->vector 0 1 2 3)))

;;;============================================================================
