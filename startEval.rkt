#lang racket

(provide
 mk-list-of-one
 mk-list-of-two
 mk-list-of-three
 dict-get
 dict-update
 dict-update-rec
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
 *startEval
 startEval)

;; Make a list of one element
;; 
;; param item item1
;;
;; return list
(define (mk-list-of-one item) (cons item '()))

;; Make a list of two elements
;; param item item1
;; param item item2
;;
;; return list
(define (mk-list-of-two item1 item2)
  (cons item1 (cons item2 '())))

;; Make a list of three elements
;;
;; param item item1
;; param item item2
;; param item item3
;;
;; return list
(define (mk-list-of-three item1 item2 item3)
  (cons item1 (cons item2 (cons item3 '()))))

;; Modify racket's `assoc` to work like Python's dict.get()
;;   by returning only the value of a given key instead of the
;;   key value pair. If the key does not exist, return False.
;;
;; param item key
;; param item value
;;
;; return list
(define (dict-get key value)
  (if (not (assoc key value)) #f
      (cadr (assoc key value))))

;; Make a dictionary (map) of key value pairs with a list.
;; 
;; Access this with `dict-get`.
;; If a dict with existing key value pairs is given, add on to it.
;; If a key already exists within the given dict, replace the value
;;
;; param key item
;; param value item
;; param list dict
;;
;; return list
(define (dict-update key value dict)
  (if (null? dict)
      ;; Then start our own 
      (mk-list-of-one (mk-list-of-two key value))
      ;; Else use the given dict
      (if (equal? key (caar dict))
          (cons (mk-list-of-two key value) (cdr dict))
          (cons (car dict) (dict-update key value (cdr dict))))))

;; Recursively add multiple key value pairs to a dictionary using dict-update.
;;   This is equivilent to calling dict-update multiple times.
(define (dict-update-rec keys values al)
  (if (null? keys) al
      (dict-update-rec (cdr keys) (cdr values)
                (dict-update (car keys) (car values) al))))

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
      (cons (*startEval (car el) env)
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
  (let ((result (*startEval (cadr exp) env)))
    (if (or (null? result) (not result))
      ;; Predicate is False, execute the alternative
      (*startEval (cadddr exp) env)
      ;; Predicate is True, execute the consequent
      (*startEval (caddr exp) env))))

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
  (*startEval (body (funpart clo))
             (dict-update-rec (formals (funpart clo)) args (envpart clo))))

;; Convert let into a lambda
(define (convert-let lamexp)
  (append (mk-list-of-one (mk-list-of-three 'lambda
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

(define (*startEval exp env)
  (cond
    ((number? exp) exp)
    ((symbol? exp)
     (if (dict-get exp env)
         (dict-get exp env)
         (error "Symbol not defined: " exp)))
    ((equal? (car exp) 'quote) (cadr exp))
    ((equal? (car exp) 'let) (*startEval (convert-let exp) env))
    ((equal? (car exp) 'lambda) (mk-list-of-three 'closure exp env))
    ((equal? (car exp) 'if)
     (handle-if (exp env)))
    (else (apply (evallist exp env)))))

(define operators
   '((+ (primop +))
     (- (primop -))
     (* (primop *))
     (/ (primop /))
     (= (primop =))
     (equal? (primop equal?))
     (< (primop <))
     (> (primop >))
     (<= (primop <=))
     (>= (primop >=))
     (car (primop car))
     (cdr (primop cdr))
     (cons (primop cons))))

(define (startEval exp)
  (*startEval exp operators))

     
              
