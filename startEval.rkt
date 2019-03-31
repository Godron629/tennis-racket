#lang racket

(provide
 mk-list-of-one
 mk-list-of-two
 mk-list-of-three
 dict-get
 dict-update
 dict-update-rec
 binary-operator
 unary-operator
 evaluate-expression-rec
 operator
 handle-if
 closure
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
;; return item | False
(define (dict-get key value)
  (if (not (assoc key value)) #f
      (cadr (assoc key value))))

;; Make a dictionary (map) of key value pairs with a list.
;; 
;; Access this with `dict-get`.
;; If a dict with existing key value pairs is given, add on to it.
;; If a key already exists within the given dict, replace the value
;;
;; param item key
;; param item value
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

;; Recursively add multiple key value pairs to a dictionary
;;   using dict-update.
;;
;; This is equivilent to calling dict-update multiple times.
;;
;; param list keys
;; param list values
;; param list dict
;;
;; return list
(define (dict-update-rec keys values dict)
  (if (null? keys)
      ;; Then nothing to do
      dict
      ;; Else recursively add key value pairs to dict
      (dict-update-rec
       (cdr keys)
       (cdr values)
       (dict-update (car keys) (car values) dict))))

;; Apply binary function with name f to parameters x and y
;;
;; param symbol f
;; param item x
;; param item y
;;
;; return any
(define (binary-operator f x y)
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

;; Apply unary function with name f to parameter x
;;
;; param symbol f
;; param item x
;;
;; return any
(define (unary-operator f x)
  (cond
    ((equal? f 'car) (car x))
    ((equal? f 'cdr) (cdr x))
    ((equal? f 'pair?) (pair? x))
    (else (error "apply-unary: operator not supported" f))))

;; Recursively evaluate sub-expression of an expression
;;
;; param list exp
;; param list env
;;
;; return any
(define (evaluate-expression-rec exp env)
  (if (null? exp)
      ;; Then nothing to do
      '()
      ;; Else evaluate the current element and recurse
      (cons
       (*startEval (car exp) env)
       (evaluate-expression-rec (cdr exp) env))))

;; Apply operator to args depending on number of parameters
;;
;; param symbol op
;; param list args
;;
;; return any
(define (operator op args)
  (if (> (length args) 1)
      ;; Then we have more than one argument
      (binary-operator (cadr op) (car args) (cadr args))
      ;; Else it's a single argument
      (unary-operator (cadr op) (car args))))

;; Check predicate and return result of consequent or alternative
;;
;; param list exp
;; param list env
;;
;; return any
(define (handle-if exp env)
  (let ((result (*startEval (cadr exp) env)))
    (if (or (null? result) (not result))
      ;; Predicate is False, execute the alternative
      (*startEval (cadddr exp) env)
      ;; Predicate is True, execute the consequent
      (*startEval (caddr exp) env))))

;; Make a closure, all we need to do is make
;; it identifiable as a closure and wrap the
;; lambda and the environment up with it.
;;
;; param list exp
;; param list env
;;
;; return list
(define (mk-closure exp env)
  (mk-list-of-three 'closure exp env))

;; Make a list of identifiable basic racket operators
;; (to tell apart from closures).
;;
;; param list ops
;;
;; return list
(define (mk-default-operators ops)
  (cond
    ((null? ops) '())
    (else (cons
           (mk-list-of-two
            (car ops)
            (mk-list-of-two 'operator (car ops)))
           (mk-default-operators (cdr ops))))))

;; Apply a closure to arguments
;;
;; param list clo
;; param list args
;;
;; return any
(define (closure clo args)
  
  (let* 
    ((closure-environment (caddr clo))
     (closure-function (cadr clo))
     (function-body (caddr closure-function))
     (formal-arguments (cadr closure-function)))
  
    (*startEval function-body  
     ;; 'Execute' the function within the saved environment
     (dict-update-rec
      ;; Get a list of keys
      formal-arguments
      ;; Get a list values
      args
      ;; Combine those key values with the current environment
      closure-environment))))

;; Instead of implementing a `let`, just convert the let
;; into a lambda and start this evaluation over. 
;;
;; i.e. (let ((x 5)) x) => ((lambda (x) x) 5)
;; 
;; param list exp
;;
;; return list lambda exp
(define (convert-let exp)
  ;; Append the (lambda ... ) part to all the arguments
  (append
   (mk-list-of-one
    (mk-list-of-three
     'lambda
     (get-first-of-all (cadr exp))
     (caddr exp)))
   (get-second-of-all (cadr exp))))

;; Get the first element of every sublist
;;
;; param list ls
;;
;; return list
(define (get-first-of-all ls)
  (cond
    ((null? ls) '())
    ((cons (caar ls) (get-first-of-all (cdr ls))))))

;; Get the second element of every sublist
;;
;; param ls list
;;
;; return list
(define (get-second-of-all ls)
  (cond
    ((null? ls) '())
    ((cons (cadar ls) (get-second-of-all (cdr ls))))))

;; Helper function to avaluate subset of racket
;;
;; param list exp
;; param list(list) env
;;
;; return any
(define (*startEval exp env)
  (cond
    ((number? exp)
     ;; return the number
     exp)
    ((symbol? exp)
     ;; if we know about this symbol,
     ;; return it - otherwise, raise.
     (if (dict-get exp env)
         (dict-get exp env)
         (error "Symbol not defined: " exp)))
    ((equal? (car exp) 'quote)
     ;; return what is inside the quote
     (cadr exp))
    ((equal? (car exp) 'let)
     ;; convert the let to a lambda and reevaluate
     (*startEval (convert-let exp) env))
    ((equal? (car exp) 'lambda)
     ;; make a closure and evaluate it later
     (mk-closure exp env))
    ((equal? (car exp) 'if)
     ;; evaluate if statement
     (handle-if exp env))
    (else
     ;; If nothing else, it must be a function.
     ;; This can be a racket function or a lambda.
     (let ((result (evaluate-expression-rec exp env)))
       (if (equal? (caar result) 'closure)
           ;; evaluate closure we built earlier
           (closure (car result) (cdr result))
           ;; it's a racket operator!
           (operator (car result) (cdr result)))))))

;; Evaluate a subset of racket functionality
;; akin to `eval` in racket.
;;
;; param list exp (i.e. '(+ 5 5))
;;
;; return any 
(define (startEval exp)
  (*startEval
   exp
   (mk-default-operators
    '(+ - * / = equal? < > <= >= car cdr cons))))
