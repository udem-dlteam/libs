;;;============================================================================

;;; File: "133.scm"

;;; Copyright (c) 2018-2020 by Antoine Doucet, All Rights Reserved.
;;; Copyright (c) 2018-2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; SRFI 133, Vector library

(##supply-module srfi/133)

(##include "~~lib/_prim#.scm")   
(##include "~~lib/_gambit#.scm") 
                                 
(##include "133#.scm")

(declare (extended-bindings)) ;; ##fx+ is bound to fixnum addition, etc
(declare (not safe))          ;; claim code has no type errors
(declare (block))             ;; claim no global is assigned


;;;============================================================================

;;; Code ported to Gambit from Taylor Campbell's implementation 
;;; (with corrections from Will Clinger, both in the public domain).

;;;============================================================================

(define-syntax with-vector-check
    (syntax-rules ()
      ((with-vector-check (function-name vec . rest) function-def)
          (macro-force-vars (vec . rest)
            (macro-check-vector
              vec
              0
              (function-name vec . rest)
              function-def)))))

(define-syntax with-proc-check
    (syntax-rules ()
      ((with-proc-check (function-name proc . rest) function-def)
          (macro-force-vars (proc . rest)
            (macro-check-procedure
              proc
              0
              (function-name proc . rest)
              function-def)))))
            
(define-syntax define-vector-check
  (syntax-rules ()
    ((define-vector-check (function-name vec . rest) function-def)
     (define (function-name vec . rest)
         (with-vector-check (function-name vec . rest) function-def)))))

(define-syntax define-proc-check
  (syntax-rules ()
    ((define-proc-check (function-name proc . rest) function-def)
     (define (function-name proc . rest)
         (with-proc-check (function-name proc . rest) function-def)))))

(define-syntax define-proc-vector-check
  (syntax-rules ()
    ((define-proc-vector-check (function-name proc vec . rest) function-def)
     (define-proc-check (function-name proc vec . rest)
       (macro-check-vector
         vec
         1
         (function-name proc vec . rest)
         function-def)))))

;;; Todo: accept optional arguments ++ generalise


;;;============================================================================

;;; Taylor Campbell wrote this code; he places it in the public domain.
;;; Will Clinger [wdc] made some corrections, also in the public domain.
;;; John Cowan modified this code for SRFI 133; his changes are also in
;;; the public domain.

;;; The code was then adapted to Gambit.

;;;============================================================================
;;;============================================================================
;;; Constructors


(define make-vector ##make-vector)


(define vector ##vector)


(define (vector-unfold f len . initial-seeds)
  (define vec (make-vector len))
      (apply vector-unfold! f vec 0 len initial-seeds)
        vec)

(define (vector-unfold-right f len . initial-seeds)
  (define vec (make-vector len))
  (apply vector-unfold-right! f vec 0 len initial-seeds)
  vec)


(define (vector-copy vec #!optional (start 0) 
                                    (end (macro-absent-obj)) 
                                    (fill 0))
  (with-vector-check (vector-copy vec start end fill)
    (let* ((end (if (equal? end (macro-absent-obj))
                    (vector-length vec)
                    end))
          (new-vector (make-vector (- end start) fill)))
      (subvector-move! vec start 
                         (if (> end (vector-length vec))
                             (vector-length vec)
                             end)
                       new-vector 0)
             new-vector)))

(define (vector-reverse-copy vec #!optional (start 0) 
                                            (end (macro-absent-obj)))
  (with-vector-check (vector-reverse-copy vec start end)
    (let ((end (if (equal? end (macro-absent-obj))
                      (vector-length vec)
                      end)))
      (let ((new (make-vector (- end start))))
        (letrec ((loop (lambda (target source sstart i j)
                      (cond ((>= i sstart)
                      (vector-set! target j (vector-ref source i))
                      (loop target source sstart
                            (- i 1)
                            (+ j 1)))))))
          (begin (loop new vec start (- end 1) 0)
                 new))))))


(define vector-append ##vector-append) ;;; R7RS


(define vector-concatenate append-vectors) 

(define (vector-append-subvectors . args)
 ;; GATHER-ARGS returns three values: vectors, starts, ends
   (define (gather-args args)
       (let loop ((args args) (vecs '()) (starts '()) (ends '()))
             (if (null? args)
                 (values (reverse vecs) (reverse starts) (reverse ends))
                 (if (and (pair? args)
                          (pair? (cdr args))
                          (pair? (cddr args)))
                     (macro-force-vars (args)
                       (macro-check-vector
                         (car args)
                         0
                         (vector-append-subvectors . args)    
                         (loop (cdddr args)
                               (cons (car args) vecs)
                               (cons (cadr args) starts)
                               (cons (caddr args) ends))))
                     (##raise-wrong-number-of-arguments-exception 
                         vector-append-subvectors args)))))
     ;; TOTAL-LENGTH computes the length of all subvectors
   (define (total-length starts ends)
       (let loop ((count 0) (starts starts) (ends ends))
             (if (null? starts)
                 count
                 (let ((start (car starts)) (end (car ends)))
                   (loop (+ count (- end start))
                         (cdr starts)
                         (cdr ends))))))
    ;; COPY-EACH! copies each subvector into a result vector
   (define (copy-each! result vecs starts ends)
       (let loop ((at 0) (vecs vecs) (starts starts) (ends ends))
         (if (null? vecs)
             result
             (let ((vec (car vecs)) (start (car starts)) (end (car ends)))
               (%vector-copy! result at vec start end)
               (loop (+ at (- end start))
                     (cdr vecs)
                     (cdr starts)
                     (cdr ends))))))
    (receive (vecs starts ends) (gather-args args)
        (define result (make-vector (total-length starts ends)))
        (copy-each! result vecs starts ends)))

;;;============================================================================

;;; Predicates


(define vector? ##vector?)


(define-vector-check (vector-empty? vec)
    (= (vector-length vec) 0))
    
(define-proc-check (vector= elt=? . vectors)
  (if (null? vectors)
        #t
        (macro-check-vector
          (car vectors)
          1
          (vector= elt=? vectors)
          (if (null? (cdr vectors))
                 #t
              (let loop ((vecs vectors))
                     (let ((vec1 (car vecs))
                       (vec2+ (cdr vecs)))
                       (or (null? vec2+)
                           (and (binary-vector= elt=? vec1 (car vec2+))
                                (loop vec2+)))))))))

(define (binary-vector= elt=? vector-a vector-b)
  (macro-check-vector
    vector-a
    1
    (vector= elt? vector-a vector-b)
    (macro-check-vector 
      vector-b
      2
      (vector= elt? vector-a vector-b)
  (let ((length-a (vector-length vector-a))
        (length-b (vector-length vector-b)))
    (and (= length-a length-b)
         (let loop ((i 0))
                (cond
                  ((= i length-a) #t)
                  ((elt=? (vector-ref vector-a i)
                          (vector-ref vector-b i))
                      (loop (+ i 1)))
                  (else #f))))))))

;;;============================================================================

;;; Selectors


(define vector-ref ##vector-ref)


(define vector-length ##vector-length)


;;;============================================================================


;;; Mutators


(define vector-set! ##vector-set!)


(define-vector-check 
  (vector-swap! vec i j)
      (let ((x (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j x)))

(define
   (vector-fill! vec val #!optional (start 0) (end (macro-absent-obj)))
     (with-vector-check (vector-fill! vec val start end)
     (let ((end (if (equal? end (macro-absent-obj))
                    (vector-length vec)
                    end)))
       (subvector-fill! vec start end val))))


(define (vector-reverse! vec #!optional (start 0) (end (macro-absent-obj)))
  (macro-force-vars (vec start end)
    (macro-check-vector
      vec
      0
      (vector-reverse! vec start end)
  (let ((end (if (equal? end (macro-absent-obj))
                 (vector-length vec)
                 end)))
    (letrec ((loop (lambda (vec i j)
                     (cond ((<= i j)
                            (let ((v (vector-ref vec i)))
                              (vector-set! vec i (vector-ref vec j))
                              (vector-set! vec j v)
                              (loop vec (+ i 1) (- j 1))))))))
       (loop vec start (- end 1)))))))


(define (vector-copy! vec-target tstart vec-source 
           #!optional (sstart 0) (send (macro-absent-obj)))
  (with-vector-check (vector-copy! vec-target tstart vec-source sstart send)
      (macro-check-vector
        vec-source
        2
        (vector-copy! vec-target tstart vec-source sstart send)
        (let ((send (if (equal? send (macro-absent-obj))
                        (vector-length vec-source)
                        send)))
          (subvector-move! vec-source sstart send vec-target tstart)))))


(define (vector-reverse-copy! target tstart source 
                   #!optional (sstart 0) (send (macro-absent-obj)))
  (with-vector-check (vector-reverse-copy! target tstart source sstart send)
      (macro-check-vector
        source
        2
        (vector-reverse-copy! target tstart source sstart send)
        (let ((send (if (equal? send (macro-absent-obj))
                        (vector-length source)
                        send)))
   (letrec ((loop (lambda (target source sstart i j)
                      (cond ((>= i sstart)
                            (vector-set! target j (vector-ref source i))
                            (loop target source sstart
                                  (- i 1)
                                  (+ j 1)))))))
       (loop target source sstart
             (- send 1)
             tstart))))))

(define-proc-vector-check 
    (vector-unfold! f vec start end . initial-seeds)
        (letrec ((tabulate!                   ; Special zero-seed case.
                    (lambda (f vec i len)
                            (cond ((< i len)
                                    (vector-set! vec i (f i))
                                    (tabulate! f vec (+ i 1) len)))))
                 (unfold1!                    ; Fast path for one seed.
                    (lambda (f vec i len seed)
                        (if (< i len)
                            (receive (elt new-seed)
                                     (f i seed)
                                     (vector-set! vec i elt)
                                     (unfold1! f vec (+ i 1) len new-seed)))))
                 (unfold2+!                   ; Slower variant for N seeds.
                    (lambda (f vec i len seeds)
                        (if (< i len)
                            (receive (elt . new-seeds)
                                     (apply f i seeds)
                                     (vector-set! vec i elt)
                            (unfold2+! f vec (+ i 1) len new-seeds))))))
                  (if (< (vector-length vec) start)
                      #!void
                      (cond ((null? initial-seeds)
                              (tabulate! f vec start end))
                            ((null? (cdr initial-seeds))
                              (unfold1! f vec start end (car initial-seeds)))
                            (else
                              (unfold2+! f vec start end initial-seeds))))))

(define-proc-vector-check
  (vector-unfold-right! f vec start end . initial-seeds)
      (letrec ((tabulate!
                   (lambda (f vec i)
                     (cond ((>= i start)
                             (vector-set! vec i (f i))
                             (tabulate! f vec (- i 1))))))
               (unfold1!
                    (lambda (f vec i seed)
                        (if (>= i start)
                            (receive (elt new-seed)
                                     (f i seed)
                                     (vector-set! vec i elt)
                                     (unfold1! f vec (- i 1) new-seed)))))
               (unfold2+!
                   (lambda (f vec i seeds)
                       (if (>= i start)
                           (receive (elt . new-seeds)
                                    (apply f i seeds)
                                    (vector-set! vec i elt)
                                    (unfold2+! f vec (- i 1) new-seeds))))))
          (let ((i (- end 1)))
            (cond ((null? initial-seeds)
                    (tabulate! f vec i))
                  ((null? (cdr initial-seeds))
                    (unfold1!  f vec i (car initial-seeds)))
                  (else
                    (unfold2+! f vec i initial-seeds))))))



;;;============================================================================
;;; Iteration


(define-proc-check (vector-fold kons knil vec . vectors)
      (macro-check-vector
        vec
        2
        (vector-fold kons knil vec vectors)
        (if (null? vectors)
            (%vector-fold1 kons knil (vector-length vec) vec)
            (%vector-fold2+ kons knil
                            (%smallest-length vectors
                                              (vector-length vec)
                                              vector-fold)
                            (cons vec vectors)))))

(define-proc-check (vector-fold-right kons knil vec . vectors)
  (letrec ((loop1 (lambda (kons knil vec i)
                    (if (negative? i)
                        knil
                        (loop1 kons (kons knil (vector-ref vec i))
                                    vec
                                    (- i 1)))))
           (loop2+ (lambda (kons knil vectors i)
                     (if (negative? i)
                         knil
                         (loop2+ kons
                                 (apply kons knil
                                        (vectors-ref vectors i))
                                 vectors
                                 (- i 1))))))
    (macro-check-vector
      vec
      2
      (vector-fold-right kons knil vec)
      (if (null? vectors)
          (loop1  kons knil vec (- (vector-length vec) 1))
          (loop2+ kons knil (cons vec vectors)
                  (- (%smallest-length vectors
                                       (vector-length vec)
                                       vector-fold-right)
                     1))))))

(define-proc-vector-check
   (vector-map f vec . vectors)  ;;; R7Rs #unimplemented in Gambit
      (if (null? vectors)
          (let ((len (vector-length vec)))
             (%vector-map1! f (make-vector len) vec len))
          (let ((len (%smallest-length vectors
                                       (vector-length vec)
                                       vector-map)))
            (%vector-map2+! f (make-vector len) (cons vec vectors)
                            len))))


(define-proc-vector-check (vector-map! f vec . vectors)
    (if (null? vectors)
        (%vector-map1!  f vec vec (vector-length vec))
        (%vector-map2+! f vec (cons vec vectors)
                        (%smallest-length vectors
                                          (vector-length vec)
                                          vector-map!))))

(define-proc-vector-check
   (vector-map! f vec . vectors)
             (if (null? vectors)
                (%vector-map1!  f vec vec (vector-length vec))
                (%vector-map2+! f vec (cons vec vectors)
                                (%smallest-length vectors
                                                  (vector-length vec)
                                                  vector-map!))))


(define-proc-vector-check
   (vector-for-each f vec . vectors )  ;;; R7Rs #unimplemented in Gambit
      (letrec ((for-each1
                 (lambda (f vec i len)
                   (cond ((< i len)
                           (f (vector-ref vec i))
                           (for-each1 f vec (+ i 1) len)))))
               (for-each2+
                 (lambda (f vecs i len)
                   (cond ((< i len)
                           (apply f (vectors-ref vecs i))
                           (for-each2+ f vecs (+ i 1) len))))))
            (if (null? vectors)
                (for-each1 f vec 0 (vector-length vec))
                (for-each2+ f (cons vec vectors) 0
                            (%smallest-length vectors
                                              (vector-length vec)
                                              vector-for-each)))))


(define-proc-vector-check (vector-count pred? vec . vectors)
    (if (null? vectors)
        (%vector-fold1 (lambda (count elt)
                         (if (pred? elt)
                         (+ count 1)
                         count))
                       0
                       (vector-length vec)
                       vec)
        (%vector-fold2+ (lambda (count . elts)
                          (if (apply pred? elts)
                              (+ count 1)
                              count))
                        0
                        (%smallest-length vectors
                                          (vector-length vec)
                                          vector-count)
                        (cons vec vectors))))

(define-proc-check (vector-cumulate f knil vec)
  (macro-check-vector
    vec
    2
    (vector-cumulate f knil vec)
  (let* ((len (vector-length vec))
         (result (make-vector len)))
    (let loop ((i 0) (left knil))
              (if (= i len)
                  result
                  (let* ((right (vector-ref vec i)) (r (f left right)))
                    (vector-set! result i r)
                    (loop (+ i 1) r)))))))


;;;============================================================================

;;; Searching


(define-proc-vector-check
   (vector-index pred? vec . vectors)
    (vector-index/skip pred? vec vectors vector-index))


(define-proc-vector-check
   (vector-skip pred? vec . vectors)
     (vector-index/skip (lambda elts (not (apply pred? elts)))
                        vec vectors
                        vector-skip))


(define vector-index/skip
(letrec ((loop1  (lambda (pred? vec len i)
                   (cond ((= i len) #f)
                         ((pred? (vector-ref vec i)) i)
                         (else (loop1 pred? vec len (+ i 1))))))
         (loop2+ (lambda (pred? vectors len i)
                   (cond ((= i len) #f)
                         ((apply pred? (vectors-ref vectors i)) i)
                         (else (loop2+ pred? vectors len
                                       (+ i 1)))))))
    (lambda (pred? vec vectors callee)
        (if (null? vectors)
            (loop1 pred? vec (vector-length vec) 0)
            (loop2+ pred? (cons vec vectors)
                    (%smallest-length vectors
                                      (vector-length vec)
                                      callee)
                    0)))))


(define-proc-vector-check
  (vector-index-right pred? vec . vectors)
    (vector-index/skip-right pred? vec vectors vector-index-right))


(define-proc-vector-check
   (vector-skip-right pred? vec . vectors)
     (vector-index/skip-right (lambda elts (not (apply pred? elts)))
                              vec vectors
                              vector-index-right))


(define vector-index/skip-right
  (letrec ((loop1  (lambda (pred? vec i)
                     (cond ((negative? i) #f)
                           ((pred? (vector-ref vec i)) i)
                           (else (loop1 pred? vec (- i 1))))))
           (loop2+ (lambda (pred? vectors i)
                     (cond ((negative? i) #f)
                           ((apply pred? (vectors-ref vectors i)) i)
                           (else (loop2+ pred? vectors (- i 1)))))))
    (lambda (pred? vec vectors callee)
        (if (null? vectors)
            (loop1 pred? vec (- (vector-length vec) 1))
            (loop2+ pred? (cons vec vectors)
                    (- (%smallest-length vectors
                                         (vector-length vec)
                                         callee)
                        1))))))


(define (vector-binary-search vec value cmp 
                              #!optional (start 0) 
                                         (end (macro-absent-obj)))
  (with-vector-check (vector-binary-search vec value cmp)
    (macro-check-procedure
      cmp
      2
      (vector-binary-search vec value cmp)
      (let ((end (if (equal? end (macro-absent-obj))
                     (vector-length vec)
                     end)))
        (let loop ((start start) (end end) (j #f))
          (let ((i (quotient (+ start end) 2)))
            (if (or (= start end) (and j (= i j)))
                #f
                (let ((comparison (cmp (vector-ref vec i) value)))
                  (cond ((zero?     comparison) i)
                        ((positive? comparison) (loop start i i))
                        (else                   (loop i end i)))))))))))


(define-proc-vector-check
  (vector-any pred? vec . vectors)
    (letrec ((loop1 (lambda (pred? vec i len len-1)
                    (and (not (= i len))
                         (if (= i len-1)
                             (pred? (vector-ref vec i))
                             (or (pred? (vector-ref vec i))
                                 (loop1 pred? vec (+ i 1)
                                        len len-1))))))
           (loop2+ (lambda (pred? vectors i len len-1)
                     (and (not (= i len))
                          (if (= i len-1)
                              (apply pred? (vectors-ref vectors i))
                              (or (apply pred? (vectors-ref vectors i))
                                  (loop2+ pred? vectors (+ i 1)
                                  len len-1)))))))
        (if (null? vectors)
            (let ((len (vector-length vec)))
              (loop1 pred? vec 0 len (- len 1)))
            (let ((len (%smallest-length vectors
                                         (vector-length vec)
                                         vector-any)))
              (loop2+ pred? (cons vec vectors) 0 len (- len 1))))))


(define-proc-vector-check
  (vector-every pred? vec . vectors)
  (letrec ((loop1 (lambda (pred? vec i len len-1)
                    (or (not (= i len))
                         (if (= i len-1)
                             (pred? (vector-ref vec i))
                             (and (pred? (vector-ref vec i))
                                 (loop1 pred? vec (+ i 1)
                                        len len-1))))))
           (loop2+ (lambda (pred? vectors i len len-1)
                     (or (= i len)
                          (if (= i len-1)
                              (apply pred? (vectors-ref vectors i))
                              (and (apply pred? (vectors-ref vectors i))
                                  (loop2+ pred? vectors (+ i 1)
                                          len len-1)))))))
        (if (null? vectors)
            (let ((len (vector-length vec)))
              (loop1 pred? vec 0 len (- len 1)))
            (let ((len (%smallest-length vectors
                                         (vector-length vec)
                                         vector-every)))
              (loop2+ pred? (cons vec vectors) 0 len (- len 1))))))

(define-proc-vector-check
  (vector-partition pred? vec)
      (let* ((len (vector-length vec))
             (cnt (vector-count pred? vec))
             (result (make-vector len)))
        (let loop ((i 0) (yes 0) (no cnt))
                  (if (= i len)
                      (values result cnt)
                      (let ((elem (vector-ref vec i)))
                        (if (pred? elem)
                            (begin
                              (vector-set! result yes elem)
                              (loop (+ i 1) (+ yes 1) no))
                            (begin
                              (vector-set! result no elem)
                              (loop (+ i 1) yes (+ no 1)))))))))

;;;============================================================================

;;; Conversion


(define vector->list ##vector->list)


(define (reverse-vector->list vec #!optional (start 0) 
                                             (end (macro-absent-obj)))
  (with-vector-check
    (reverse-vector->list vec start end)
    (let ((end (if (equal? end (macro-absent-obj))
                   (vector-length vec)
                   end)))
    (do ((i start (+ i 1))
        (result '() (cons (vector-ref vec i) result)))
        ((= i end) result)))))


(define list->vector ##list->vector)


(define (reverse-list->vector lst #!optional (start 0) 
                                             (end (macro-absent-obj)))
    (let ((end (if (equal? end (macro-absent-obj))
                   (length lst)
                   end))
          (f (lambda (index l) (values (car l) (cdr l)))))
      (vector-unfold-right f (- end start) (list-tail lst start))))

    ;;; todo type check for list


(define (vector->string vec #!optional (start 0) (end (macro-absent-obj)))
  (with-vector-check (vector->string vec start end)
    (let ((end (if (equal? end (macro-absent-obj))
                   (vector-length vec)
                   end)))
        (define result (make-string (- end start)))
        (let loop ((at 0) (i start))
                  (if (= i end)
                      result
                      (let ((val (vector-ref vec i)))
                         (macro-check-char 
                           val
                           i
                           (vector->string vec)
                           (begin
                             (string-set! result at val)
                             (loop (+ at 1) (+ i 1))))))))))

(define (string->vector str #!optional (start 0) (end (macro-absent-obj)))
    (macro-force-vars (str)
      (macro-check-string
        str
        0
        (string->vector str start end)
        (let ((end (if (equal? end (macro-absent-obj))
                   (string-length str)
                   end)))
          (define result (make-vector (- end start)))
              (let loop ((at 0) (i start))
                        (if (= i end)
                            result
                            (begin
                              (vector-set! result at (string-ref str i))
                                           (loop (+ at 1) (+ i 1)))))))))

;;;============================================================================
;;;============================================================================

;;; Internal procedures 

    ; This should be implemented more efficiently.  It shouldn't cons a
    ; closure, and the cons cells used in the loops when using this could
    ; be reused.
(define (vectors-ref vectors i)
  (map (lambda (v) (vector-ref v i)) vectors))



(define %smallest-length
  (letrec ((loop (lambda (vector-list length callee)
                   (if (null? vector-list)
                   length
                   (loop (cdr vector-list)
                         (let ((vec (car vector-list))
                                (vec-rest (cdr vector-list)))
                                (macro-force-vars (vec)
                                (macro-check-vector
                                  vec
                                  3
                                  (%smallest-length vector-list length callee)
                                  (min (vector-length vec)
                                       length))))
                         callee)))))
    loop))


(define %vector-fold1
  (letrec ((loop (lambda (kons knil len vec i)
                     (if (= i len)
                         knil
                         (loop kons
                               (kons knil (vector-ref vec i))
                               len vec (+ i 1))))))
    (lambda (kons knil len vec)
      (loop kons knil len vec 0))))


(define %vector-fold2+
  (letrec ((loop (lambda (kons knil len vectors i)
                   (if (= i len)
                       knil
                       (loop kons
                             (apply kons knil
                                    (vectors-ref vectors i))
                             len vectors (+ i 1))))))
      (lambda (kons knil len vectors)
        (loop kons knil len vectors 0))))

(define %vector-map1!
    (letrec ((loop (lambda (f target vec i)
                     (if (zero? i)
                         target
                         (let ((j (- i 1)))
                           (vector-set! target j
                                        (f (vector-ref vec j)))
                           (loop f target vec j))))))
      (lambda (f target vec len)
        (loop f target vec len))))


(define %vector-map2+!
  (letrec ((loop (lambda (f target vectors i)
                     (if (zero? i)
                         target
                         (let ((j (- i 1)))
                           (vector-set! target j
                            (apply f (vectors-ref vectors j)))
                            (loop f target vectors j))))))
    (lambda (f target vectors len)
      (loop f target vectors len))))

(define %vector-copy!
  (letrec ((loop/l->r (lambda (target source send i j)
                      (cond ((< i send)
                      (vector-set! target j
                      (vector-ref source i))
                      (loop/l->r target source send
                                 (+ i 1) (+ j 1))))))
           (loop/r->l (lambda (target source sstart i j)
                      (cond ((>= i sstart)
                      (vector-set! target j
                      (vector-ref source i))
                      (loop/r->l target source sstart
                                 (- i 1) (- j 1)))))))
    (lambda (target tstart source sstart send)
        (if (> sstart tstart)             ; Make sure we don't copy over
                                          ;   ourselves.
            (loop/l->r target source send sstart tstart)
            (loop/r->l target source sstart (- send 1)
                                 (+ -1 tstart send (- sstart)))))))

;;;============================================================================
