#lang racket

(provide
 list1
 list2
 list3
 *assoc
 mkassoc
 mkassoc*
 apply-binary-op
 apply-unary-op
 evallist
 apply
 apply-value-op
 handle-if
 formals
 body
 funpart
 envpart
 closure?
 apply-closure
 get-first-of-all
 get-second-of-all
 convert-let
 startEval)

;; Make a list of one element
(define (list1 x) (cons x '()))
;; Make a list of two elements
(define (list2 x y) (cons x (cons y '())))
;; Make a list of three elements
(define (list3 x y z) (cons x (cons y (cons z '()))))

;; Modify racket's `assoc` by returning only value of the
;; returned key value pair and false otherwise.
(define (*assoc x y)
  (if (not (assoc x y)) #f
      (cadr (assoc x y))))

;; Make a list of key value pairs.
;; If a list of existing key value pairs is given, add on to it.
;; If a key already exists within the given list, replace it. 
(define (mkassoc x y alist)
  (if (null? alist) (list1 (list2 x y))
      (if (equal? x (caar alist)) (cons (list2 x y) (cdr alist))
          (cons (car alist) (mkassoc x y (cdr alist))))))

;; Recursively add multiple key value pairs to a list using mkassoc
(define (mkassoc* keys values al)
  (if (null? keys) al
      (mkassoc* (cdr keys) (cdr values)
                (mkassoc (car keys) (car values) al))))

;; Apply function f to x and y
(define (apply-binary-op f x y)
  (cond
    ((equal? f 'cons) (cons x y))
    ((equal? f '+) (+ x y))
    ((equal? f '-) (- x y))
    ((equal? f '*) (* x y))
    ((equal? f '/) (/ x y))
    ((equal? f '<) (< x y))
    ((equal? f '>) (> x y))
    ((equal? f '=) (= x y))
    ((equal? f '>=) (>= x y))
    ((equal? f '<=) (<= x y))
    ((equal? f 'equal?) (equal? x y))
    (else (error "apply-binary: operator not supported" f))))

;; Apply function f to x
(define (apply-unary-op f x)
  (cond
    ((equal? f 'car) (car x))
    ((equal? f 'cdr) (cdr x))
    ((equal? f 'pair?) (pair? x))
    (else (error "apply-unary: operator not supported" f))))

;; Recursively evaluate every element of given expression
(define (evallist el env)
  (if (null? el) '()
      (cons (startEval (car el) env)
            (evallist (cdr el) env))))

;; Asset expression is a closure
(define (closure? f)
  (equal? (car f) 'closure))

;; Evaluate expression el
(define (apply el)
  (if (closure? (car el))
      ;; took env out !!
      (apply-closure (car el) (cdr el))
      (apply-value-op (car el) (cdr el))))

;; Apply primop to args
(define (apply-value-op primop args)
  (if (equal? (length args) 1)
      (apply-unary-op (cadr primop) (car args))
      (apply-binary-op (cadr primop) (car args) (cadr args))))

;; Check predicate and return consequent or alternative
(define (handle-if exp env)
  (let ((result (startEval (cadr exp) env)))
    (if (or (null? result) (not result))
      ;; Predicate is False, execute the alternative
      (startEval (cadddr exp) env)
      ;; Predicate is True, execute the consequent
      (startEval (caddr exp) env))))

;; Get the formal parameter names from a lambda
(define (formals exp) (cadr exp))
;; Get the body of a lambda
(define (body exp) (caddr exp))
;; Get the arguments from a closure
(define (funpart clo) (cadr clo))
;; Get the environment from a closure
(define (envpart clo) (caddr clo))

;; Apply a closure to arguments
(define (apply-closure clo args)
  (startEval (body (funpart clo))
             (mkassoc* (formals (funpart clo)) args (envpart clo))))

;; Convert let into a lambda
(define (convert-let lamexp)
  (append (list1 (list3 'lambda
                (get-first-of-all (cadr lamexp))
                (caddr lamexp)))
         (get-second-of-all (cadr lamexp))))

;; Get the first element of every sublist
(define (get-first-of-all ls)
  (cond
    ((null? ls) '())
    ((cons (caar ls) (get-first-of-all (cdr ls))))))

;; Get the second element of every sublist
(define (get-second-of-all ls)
  (cond
    ((null? ls) '())
    ((cons (cadar ls) (get-second-of-all (cdr ls))))))

(define (startEval exp env)
  (cond
    ((number? exp) exp)
    ((symbol? exp)
     (if (*assoc exp env)
         (*assoc exp env)
         (error "Symbol not defined: " exp)))
    ((equal? (car exp) 'quote) (cadr exp))
    ((equal? (car exp) 'let) (startEval (convert-let exp) env))
    ((equal? (car exp) 'lambda) (list3 'closure exp env))
    ((equal? (car exp) 'if)
     (handle-if (exp env)))
    (else (apply (evallist exp env)))))
