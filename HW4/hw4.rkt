
#lang racket

(provide (all-defined-out)) 

(define (sequence low high stride) 
  (if ( > low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s)
         (string-append s suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0) null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))
               
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0) (cons (- 0 x) (lambda() (f (+ x 1))))
                            (cons x (lambda() (f (+ x 1))))))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec ([f (lambda(x) (if x (cons "dan.jpg" (lambda() (f #f)))
                (cons "dog.jpg" (lambda() (f #t)))))])
    (lambda() (f #t))))

(define (stream-add-zero s)
  (lambda() (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda(n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                               (lambda() (f (+ n 1)))))])
    (lambda() (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda(n) (if (>= n (vector-length vec))
                      #f
                      (if (pair? (vector-ref vec n)) 
                      (if (equal? (car (vector-ref vec n)) v)
                          (vector-ref vec n)
                          (f (+ n 1)))
                      (f (+ n 1)))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [index 0]
           [f (lambda (v)
              (let ([ans (vector-assoc v memo)])
                (if ans
                    ans
                    (let ([new-ans (assoc v xs)])
                      (if new-ans
                          (begin (vector-set! memo index new-ans)
                                 (if (< index (- n 1)) 
                                      (set! index (+ index 1))
                                      (set! index 0)) 
                                  new-ans)
                          #f)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([high e1])
       (letrec ([loop (lambda (it)
                        (if (>= it high)
                            #t
                            (loop e2)))])
         (loop e2)))]))