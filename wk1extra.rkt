#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; helper function for streams
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; add an extra stream for tests
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; put your code below

(define (palindromic xs)
  (letrec ([len (length xs)]
           [f (lambda (l end)
                (if (null? l)
                    null
                    (let ([e1 (car l)]
                          [e2 (list-ref xs end)])
                      (cons (+ e1 e2) (f (cdr l) (- end 1))))))])
    (f xs (- len 1))))


(define fibonacci
  (letrec ([f (lambda (f1 f2)
                (cons f2 (lambda () (f (+ f1 f2) f1))))])
    (lambda () (f 1 0))))


(define (stream-until f s)
  (letrec ([helper (lambda (pr)
                     (let ([e (car pr)])
                       (if (f e)
                           (helper ((cdr pr)))
                           e)))])
    (helper (s))))


(define (stream-map f s)
  (letrec ([helper (lambda (x)
                     (cons (f (car x)) (lambda () (helper ((cdr x))))))])
    (lambda () (helper (s)))))


(define (stream-zip s1 s2)
  (letrec ([f (lambda (pr1 pr2)
                (cons (cons (car pr1) (car pr2)) (lambda () (f ((cdr pr1)) ((cdr pr2))))))])
    (lambda () (f (s1) (s2)))))


(define (interleave xs)
  (letrec ([n (length xs)]
           [f (lambda (i x)
                (let* ([pr ((list-ref x i))]
                       [y (list-set x i (cdr pr))])
                  (cons (car pr) (lambda () (f (remainder (+ i 1) n) y)))))])
    (lambda () (f 0 xs))))

                  
(define (pack n s)
  (letrec ([proc s]
           [f (lambda ()
                (let ([l (g n)])
                  (cons l (lambda () (f)))))]
           [g (lambda (i)
                (let ([pr (proc)])
                  (if (= i 1)
                      (begin
                        (set! proc (cdr pr))
                        (cons (car pr) null))
                      (cons (car pr) (g (- i 1))))))])
    (lambda () (f))))                 


(define (sqrt-stream n)
  (letrec ([f (lambda (guess)
                (let ([succ (* 0.5 (+ guess (/ n guess)))])
                  (cons guess (lambda () (f succ)))))])
    (lambda () (f n))))


(define (approx-sqrt n e)
  (letrec ([predicate (lambda (ans)
                        (if (> (abs (- (* ans ans) n)) e)
                            #t
                            #f))]
           [stream (sqrt-stream n)])
    (stream-until predicate stream)))
