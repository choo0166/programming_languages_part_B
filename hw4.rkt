
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;;Q1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;;Q2
(define (string-append-map xs suffix)
  (map (lambda (s)
         (string-append s suffix))
       xs))


;;Q3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let*
                ([i (remainder n (length xs))]
                 [l (list-tail xs i)])
              (car l))]))


;;Q4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))


;;Q5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))


;;Q6
(define dan-then-dog
  (letrec ([f (lambda (flag)
                (if (= flag 1)
                    (cons "dog.jpg" (lambda () (f (* flag (- 0 1)))))
                    (cons "dan.jpg" (lambda () (f (* flag (- 0 1)))))))])
    (lambda () (f (- 0 1)))))


;;Q7
(define (stream-add-zero s)
  (let ([pr (s)])
    (lambda () (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))


;;Q8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))


;;Q9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (pos)
                (if (= pos len)
                    #f
                    (let ([e (vector-ref vec pos)])
                      (if (and (pair? e) (equal? (car e) v))
                          e
                          (f (+ pos 1))))))])
    (f 0)))


;;Q10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [slot 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! cache slot new-ans)
                              (set! slot (remainder (+ slot 1) n))
                              new-ans)
                            #f)))))])
    f))
                        
                      
;;Q11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([v1 e1]
           [v2 e2])
       (letrec ([check-cond (lambda ()
                              (if (< e2 v1)
                                  (check-cond)
                                  #t))])
         (check-cond)))]))
                                  