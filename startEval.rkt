#lang racket

(provide
 list1
 list2
 list3
 *assoc
 mkassoc
 mkassoc*
 apply-binary-op
 apply-unary-op)

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
    ((equal? f '>=) (>= x y))
    ((equal? f '<=) (<= x y))
    ((equal? f 'equal?) (equal? x y))
    (else (error "apply-binary: operator not supported" f))))

;; Apply function f to x
(define (apply-unary-op f x)
  (cond
    ((equal? f 'quote) x)
    ((equal? f 'car) (car x))
    ((equal? f 'cdr) (cdr x))
    ((equal? f 'number?) (number? x))
    ((equal? f 'list?) (list? x))
    ((equal? f 'symbol?) (symbol? x))
    ((equal? f 'pair?) (pair? x))
    ((equal? f 'null?) (null? x))
    (else (error "apply-unary: operator not supported" f))))