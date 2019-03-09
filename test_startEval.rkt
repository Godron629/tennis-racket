#lang racket/base

(require rackunit "startEval.rkt")

;; list1: create a list of one element
(check-equal? (list1 '()) '(()))
(check-equal? (list1 5) '(5))
(check-equal? (list1 '(1 2 3)) '((1 2 3)))

;; list2: create a list of two elements
(check-equal? (list2 1 2) '(1 2))
(check-equal? (list2 '() 2) '(() 2))
(check-equal? (list2 '(1 2 3) '(4 5 6)) '((1 2 3) (4 5 6)))

;; list3: create a list of three elements
(check-equal? (list3 1 2 3) '(1 2 3))
(check-equal? (list3 '() '() '()) '(() () ()))
(check-equal? (list3 '(1 2 3) '(4 5 6) '(7 8 9)) '((1 2 3) (4 5 6) (7 8 9)))

;; *assoc: modify racket assoc to return only value, not key value pair
(check-equal? (*assoc 1 '((1 2))) 2)
(check-equal? (*assoc 1 '((2 3))) #f)
(check-equal? (*assoc 2 '((1 2) (2 3))) 3)

;; mkassoc: return a list of key value pairs
(check-equal? (mkassoc 1 2 '()) '((1 2)))
(check-equal? (mkassoc 1 2 '((1 3))) '((1 2)))
(check-equal? (mkassoc 1 2 '((3 4))) '((3 4) (1 2)))

;; mkassoc: recursively call mkassoc to add multiple key values at once
(check-equal? (mkassoc* '(1 2 3) '(4 5 6) '()) '((1 4) (2 5) (3 6)))
(check-equal? (mkassoc* '(1 2) '(4 5) '((9 10))) '((9 10) (1 4) (2 5)))
(check-equal? (mkassoc* '(1 2) '(4 5) '((1 10))) '((1 4) (2 5)))

;; apply-binary-op: apply f to x and y
(check-equal? (apply-binary-op '+ 10 5) 15)
(check-equal? (apply-binary-op '- 10 5) 5)
(check-equal? (apply-binary-op '* 10 5) 50)
(check-equal? (apply-binary-op '/ 10 5) 2)
(check-equal? (apply-binary-op '< 10 5) #f)
(check-equal? (apply-binary-op '< 5 10) #t)
(check-equal? (apply-binary-op '< 10 10) #f)
(check-equal? (apply-binary-op '> 10 5) #t)
(check-equal? (apply-binary-op '> 5 10) #f)
(check-equal? (apply-binary-op '> 10 10) #f)
(check-equal? (apply-binary-op '>= 10 5) #t)
(check-equal? (apply-binary-op '>= 5 10) #f)
(check-equal? (apply-binary-op '>= 10 10) #t)
(check-equal? (apply-binary-op '<= 10 5) #f)
(check-equal? (apply-binary-op '<= 5 10) #t)
(check-equal? (apply-binary-op '<= 10 10) #t)
(check-equal? (apply-binary-op 'equal? "hi" "hi") #t)
(check-equal? (apply-binary-op 'equal? "hi" "bye") #f)

;; apply-unary-op: apply f to x
(check-equal? (apply-unary-op 'car '(1 2 3)) 1)
(check-equal? (apply-unary-op 'cdr '(1 2 3)) '(2 3))
(check-equal? (apply-unary-op 'cdr '(1)) '())
(check-equal? (apply-unary-op 'quote '(1 2 3)) '(1 2 3))
(check-equal? (apply-unary-op 'quote 'x) 'x)
(check-equal? (apply-unary-op 'quote 1) 1)
(check-equal? (apply-unary-op 'number? 1) #t)
(check-equal? (apply-unary-op 'number? 'x) #f)
(check-equal? (apply-unary-op 'list? '(1 2 3)) #t)
(check-equal? (apply-unary-op 'list? '1) #f)
(check-equal? (apply-unary-op 'symbol? 1) #f)
(check-equal? (apply-unary-op 'symbol? 'x) #t)
(check-equal? (apply-unary-op 'null? '()) #t)
(check-equal? (apply-unary-op 'null? '(1)) #f)
