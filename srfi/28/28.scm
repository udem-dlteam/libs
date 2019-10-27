(define (format fmt . args)
  (let ((n (string-length fmt))
        (p (open-output-string)))

    (let loop ((i 0)
               (escape #f)
               (args args))
      (if (= i n)
          (cond ((pair? args)
                 (error 'format "unused format arguments"))
                (escape
                  (error 'format "incomplete escape sequence"))
                (else
                  (get-output-string p)))

          (let ((ch (string-ref fmt i)))
            (cond (escape
                    (case ch
                      ((#\~)
                       (write-char #\~ p)
                       (loop (+ i 1) #f args))

                      ((#\%)
                       (newline p)
                       (loop (+ i 1) #f args))

                      ((#\a)
                       (if (pair? args)
                           (display (car args) p)
                           (error 'format "No value for ~a"))
                       (loop (+ i 1) #f (cdr args)))

                      ((#\s)
                       (if (pair? args)
                           (write (car args) p)
                           (error 'format "No value for ~s"))
                       (loop (+ i 1) #f (cdr args)))

                      (else
                        (error 'format
                               (string-append "Invalid escape sequence: "
                                              (string #\~ ch))))))

                  ((char=? ch #\~)
                   (loop (+ i 1) #t args))

                  (else
                    (write-char ch p)
                    (loop (+ i 1) #f args))))))))
