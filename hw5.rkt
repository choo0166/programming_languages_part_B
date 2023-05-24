;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))


(define (mupllist->racketlist lst)
  (if (aunit? lst)
      null
      (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst))))) 

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
;; env is a list of pairs in form of (var_name value)
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(or (int? e) (closure? e) (aunit? e)) e]
        [(fun? e)
         (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([v (eval-under-env (call-funexp e) env)])
           (if (closure? v)
               (letrec ([arg (eval-under-env (call-actual e) env)]
                        [f (closure-fun v)]
                        [f-name (fun-nameopt f)])
                 (if f-name
                     (let ([f-env (cons (cons f-name v) (closure-env v))])
                       (eval-under-env (fun-body f) (cons (cons (fun-formal f) arg) f-env)))
                     (eval-under-env (fun-body f) (cons (cons (fun-formal f) arg) (closure-env v)))))
               (error "MUPL call applied to non-closure")))]         
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL fst applied to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  ;; (mlet s e1 e2) is a let expression with
  ;; value from e1 bound to s in evaluation of e2
  (if (null? lstlst)
      e2
      (let ([pair (car lstlst)])
        (mlet (car pair) (cdr pair) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2)) ;; _x = e1, _y = e2
         (ifgreater (var "_x") (var "_y")
                    e4 ;; _x > _y
                    (ifgreater (var "_y") (var "_x") ;; _x <= _y
                               e4 ;; _x < _y
                               e3)))) ;; _x = _y

;; Problem 4

(define mupl-map
  ;; f bound to function argument as closure
  ;; l bound to list argument in above closure environment
  (fun #f "f" (fun "mupl-f" "l"
                   (ifgreater (isaunit (var "l")) (int 0)
                                       (aunit)
                                       (apair (call (var "f") (fst (var "l"))) (call (var "mupl-f") (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" (fun #f "l"
                         (call (call (var "map") (fun #f "x" (add (var "x") (var "i")))) (var "l"))))))
