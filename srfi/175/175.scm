;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: MIT

(define (ensure-int x)
  (if (char? x) (char->integer x) x))

(define (base-offset-limit x base offset limit)
  (let ((cc (ensure-int x)))
    (and (fx>= cc base) (fx< cc (fx+ base limit))
         (fx+ offset (fx- cc base)))))

(define (char->int->char map-int char)
  (let ((int (map-int (char->integer char))))
    (and int (integer->char int))))

;;

(define (ascii-codepoint? x)
  (and (fixnum? x) (fx<= 0 x #x7f)))

(define (ascii-char? x)
  (and (char? x) (fx< (char->integer x) #x80)))

(define (ascii-bytevector? x)
  (and (bytevector? x)
       (let check ((i (fx- (bytevector-length x) 1)))
         (or (fx< i 0) (and (fx< (bytevector-u8-ref x i) #x80)
                            (check (fx- i 1)))))))

(define (ascii-string? x)
  (and (string? x)
       (call-with-port
        (open-input-string x)
        (lambda (in)
          (let check ()
            (let ((char (read-char in)))
              (or (eof-object? char)
                  (and (fx< (char->integer char) #x80) (check)))))))))

(define (ascii-control? x)
  (let ((cc (ensure-int x)))
    (or (fx<= 0 cc #x1f) (fx= cc #x7f))))

(define (ascii-non-control? x)
  (let ((cc (ensure-int x)))
    (fx<= #x20 cc #x7e)))

(define (ascii-whitespace? x)
  (let ((cc (ensure-int x)))
    (cond ((fx< cc #x09) #f)
          ((fx< cc #x0e) #t)
          (else (fx= cc #x20)))))

(define (ascii-space-or-tab? x)
  (let ((cc (ensure-int x)))
    (case cc ((#x09 #x20) #t) (else #f))))

(define (ascii-other-graphic? x)
  (let ((cc (ensure-int x)))
    (or (fx<= #x21 cc #x2f)
        (fx<= #x3a cc #x40)
        (fx<= #x5b cc #x60)
        (fx<= #x7b cc #x7e))))

(define (ascii-upper-case? x)
  (let ((cc (ensure-int x)))
    (fx<= #x41 cc #x5a)))

(define (ascii-lower-case? x)
  (let ((cc (ensure-int x)))
    (fx<= #x61 cc #x7a)))

(define (ascii-alphabetic? x)
  (let ((cc (ensure-int x)))
    (or (fx<= #x41 cc #x5a)
        (fx<= #x61 cc #x7a))))

(define (ascii-alphanumeric? x)
  (let ((cc (ensure-int x)))
    (or (fx<= #x30 cc #x39)
        (fx<= #x41 cc #x5a)
        (fx<= #x61 cc #x7a))))

(define (ascii-numeric? x radix)
  (not (not (ascii-digit-value x radix))))

;;

(define (ascii-digit-value x limit)
  (base-offset-limit x #x30 0 (min limit 10)))

(define (ascii-upper-case-value x offset limit)
  (base-offset-limit x #x41 offset (min limit 26)))

(define (ascii-lower-case-value x offset limit)
  (base-offset-limit x #x61 offset (min limit 26)))

(define (ascii-nth-digit n)
  (and (fx<= 0 n 9) (integer->char (fx+ #x30 n))))

(define (ascii-nth-upper-case n)
  (integer->char (fx+ #x41 (fxmodulo n 26))))

(define (ascii-nth-lower-case n)
  (integer->char (fx+ #x61 (fxmodulo n 26))))

(define (ascii-upcase x)
  (if (char? x)
      (integer->char (ascii-upcase (char->integer x)))
      (or (ascii-lower-case-value x #x41 26) x)))

(define (ascii-downcase x)
  (if (char? x)
      (integer->char (ascii-downcase (char->integer x)))
      (or (ascii-upper-case-value x #x61 26) x)))

(define (ascii-control->graphic x)
  (if (char? x)
      (char->int->char ascii-control->graphic x)
      (or (and (fx<= 0 x #x1f) (fx+ x #x40))
          (and (fx= x #x7f) #x3f))))

(define (ascii-graphic->control x)
  (if (char? x)
      (char->int->char ascii-graphic->control x)
      (or (and (fx<= #x40 x #x5f) (fx- x #x40))
          (and (fx= x #x3f) #x7f))))

(define (ascii-mirror-bracket char)
  (if (fixnum? char)
      (let ((char (ascii-mirror-bracket (integer->char char))))
        (and char (char->integer char)))
      (case char
        ((#\() #\))
        ((#\)) #\()
        ((#\[) #\])
        ((#\]) #\[)
        ((#\{) #\})
        ((#\}) #\{)
        ((#\<) #\>)
        ((#\>) #\<)
        (else #f))))

(define (ascii-ci-cmp char1 char2)
  (let ((cc1 (ensure-int char1))
        (cc2 (ensure-int char2)))
    (when (fx<= #x41 cc1 #x5a) (set! cc1 (fx+ cc1 #x20)))
    (when (fx<= #x41 cc2 #x5a) (set! cc2 (fx+ cc2 #x20)))
    (cond ((fx< cc1 cc2) -1)
          ((fx> cc1 cc2) 1)
          (else 0))))

(define (ascii-ci=? char1 char2)
  (fx= (ascii-ci-cmp char1 char2) 0))

(define (ascii-ci<? char1 char2)
  (fx< (ascii-ci-cmp char1 char2) 0))

(define (ascii-ci>? char1 char2)
  (fx> (ascii-ci-cmp char1 char2) 0))

(define (ascii-ci<=? char1 char2)
  (fx<= (ascii-ci-cmp char1 char2) 0))

(define (ascii-ci>=? char1 char2)
  (fx>= (ascii-ci-cmp char1 char2) 0))

(define (ascii-string-ci-cmp string1 string2)
  (call-with-port
   (open-input-string string1)
   (lambda (in1)
     (call-with-port
      (open-input-string string2)
      (lambda (in2)
        (let loop ()
          (let ((char1 (read-char in1))
                (char2 (read-char in2)))
            (cond ((eof-object? char1) (if (eof-object? char2) 0 -1))
                  ((eof-object? char2) 1)
                  (else (let ((cc1 (char->integer char1))
                              (cc2 (char->integer char2)))
                          (when (fx<= #x41 cc1 #x5a) (set! cc1 (fx+ cc1 #x20)))
                          (when (fx<= #x41 cc2 #x5a) (set! cc2 (fx+ cc2 #x20)))
                          (cond ((fx< cc1 cc2) -1)
                                ((fx> cc1 cc2) 1)
                                (else (loop)))))))))))))

(define (ascii-string-ci=? string1 string2)
  (fx= (ascii-string-ci-cmp string1 string2) 0))

(define (ascii-string-ci<? string1 string2)
  (fx< (ascii-string-ci-cmp string1 string2) 0))

(define (ascii-string-ci>? string1 string2)
  (fx> (ascii-string-ci-cmp string1 string2) 0))

(define (ascii-string-ci<=? string1 string2)
  (fx<= (ascii-string-ci-cmp string1 string2) 0))

(define (ascii-string-ci>=? string1 string2)
  (fx>= (ascii-string-ci-cmp string1 string2) 0))
