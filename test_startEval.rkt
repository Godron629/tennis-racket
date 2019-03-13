#lang racket/base

(require rackunit "startEval.rkt")
(require rackunit/text-ui)

(define-test-suite list1-suite
  "list test suite"
  (test-case "empty list" (check-equal? (list1 '()) '(())))
  (test-case "number" (check-equal? (list1 '5) '(5)))
  (test-case "list" (check-equal? (list1 '(1 2 3)) '((1 2 3))))
  )

(define-test-suite list2-suite "list2 test suite"
  (test-case "numbers" (check-equal? (list2 1 2) '(1 2)))
  (test-case "empty list" (check-equal? (list2 '() 2) '(() 2)))
  (test-case "nested list" (check-equal? (list2 '(1 2 3) '(4 5 6)) '((1 2 3) (4 5 6))))
  )

(define-test-suite list3-suite
  "list3 test suite"
  (test-case "integers" (check-equal? (list3 1 2 3) '(1 2 3)))
  (test-case "empty lists" (check-equal? (list3 '() '() '()) '(() () ())))
  (test-case "nested lists" (check-equal? (list3 '(1 2 3) '(4 5 6) '(7 8 9)) '((1 2 3) (4 5 6) (7 8 9))))
  )

(define-test-suite *assoc-suite
  "*assoc test suite"
  ;; *assoc: modify racket assoc to return only value, not k/v pair
  (test-case "in" (check-equal? (*assoc 1 '((1 2))) 2))
  (test-case "not in" (check-equal? (*assoc 1 '((2 3))) #f))
  (test-case "in second" (check-equal? (*assoc 2 '((1 2) (2 3))) 3))
  )

(define-test-suite mkassoc-suite
  "mkassoc test suite"
  ;; mkassoc: return a list of key value pairs
  (test-case "none in list" (check-equal? (mkassoc 1 2 '()) '((1 2))))
  (test-case "overwrite key" (check-equal? (mkassoc 1 2 '((1 3))) '((1 2))))
  (test-case "some in list" (check-equal? (mkassoc 1 2 '((3 4))) '((3 4) (1 2))))
  )

(define-test-suite mkassoc*-suite
  "mkassoc* test suite"
  ;; mkassoc: recursively call mkassoc to add multiple key values at once
  (test-case "1" (check-equal? (mkassoc* '(1 2 3) '(4 5 6) '()) '((1 4) (2 5) (3 6))))
  (test-case "2" (check-equal? (mkassoc* '(1 2) '(4 5) '((9 10))) '((9 10) (1 4) (2 5))))
  (test-case "3" (check-equal? (mkassoc* '(1 2) '(4 5) '((1 10))) '((1 4) (2 5))))
  )

(define-test-suite apply-binary-suite
  "apply-binary-suite"
  ;; apply-binary-op: apply f to x and y
  (test-case "+" (check-equal? (apply-binary-op '+ 10 5) 15))
  (test-case "-" (check-equal? (apply-binary-op '- 10 5) 5))
  (test-case "*" (check-equal? (apply-binary-op '* 10 5) 50))
  (test-case "/" (check-equal? (apply-binary-op '/ 10 5) 2))
  (test-case "<" (check-equal? (apply-binary-op '< 10 5) #f))
  (test-case "<" (check-equal? (apply-binary-op '< 5 10) #t))
  (test-case "<" (check-equal? (apply-binary-op '< 10 10) #f))
  (test-case ">" (check-equal? (apply-binary-op '> 10 5) #t))
  (test-case ">" (check-equal? (apply-binary-op '> 5 10) #f))
  (test-case ">" (check-equal? (apply-binary-op '> 10 10) #f))
  (test-case ">=" (check-equal? (apply-binary-op '>= 10 5) #t))
  (test-case ">=" (check-equal? (apply-binary-op '>= 5 10) #f))
  (test-case ">=" (check-equal? (apply-binary-op '>= 10 10) #t))
  (test-case "<=" (check-equal? (apply-binary-op '<= 10 5) #f))
  (test-case "<=" (check-equal? (apply-binary-op '<= 5 10) #t))
  (test-case "<=" (check-equal? (apply-binary-op '<= 10 10) #t))
  (test-case "=" (check-equal? (apply-binary-op '= 10 10) #t))
  (test-case "equal?" (check-equal? (apply-binary-op 'equal? "hi" "hi") #t))
  (test-case "equal?" (check-equal? (apply-binary-op 'equal? "hi" "bye") #f))
  )

(define-test-suite apply-unary-suite
  "apply-unary test suite"
  ;; apply-unary-op: apply f to x
  (test-case "car" (check-equal? (apply-unary-op 'car '(1 2 3)) 1))
  (test-case "cdr" (check-equal? (apply-unary-op 'cdr '(1 2 3)) '(2 3)))
  (test-case "cdr" (check-equal? (apply-unary-op 'cdr '(1)) '()))
  (test-case "quote" (check-equal? (apply-unary-op 'quote '(1 2 3)) '(1 2 3)))
  (test-case "quote" (check-equal? (apply-unary-op 'quote 'x) 'x))
  (test-case "quote" (check-equal? (apply-unary-op 'quote 1) 1))
  (test-case "pair?" (check-equal? (apply-unary-op 'pair? '(1 2)) #t))
  (test-case "pair?" (check-equal? (apply-unary-op 'pair? '()) #f))
  (test-case "pair?" (check-equal? (apply-unary-op 'pair? '1) #f))
  )

(define-test-suite apply-value-op-suite
  "apply-value-op test suite"
  ;; apply-value-op: apply operator to arguments
  (test-case "+" (check-equal? (apply-value-op '+ '(1 2)) 3))
  (test-case "-" (check-equal? (apply-value-op '- '(1 2)) -1))
  (test-case "equal?" (check-equal? (apply-value-op 'equal? '(1 2)) #f))
  (test-case "<" (check-equal? (apply-value-op '< '(1 2)) #t))
  )

(define-test-suite apply-suite
  "apply test suite"
  ;; apply: evaluate expression
  (test-case "1" (check-equal? (apply '(+ 1 2)) 3))
  (test-case "2" (check-equal? (apply '(equal? 1 1)) #t))
  (test-case "3" (check-equal? (apply '(car (1 2))) 1))
  (test-case "4" (check-equal? (apply '(cons 1 (2))) '(1 2)))
  )

(define-test-suite evallist-suite
  "evallist test suite"
  ;; evallist: recursively evaluate expressions
  (test-case "1" (check-equal? (evallist '(+ 1 2) '((+ +))) '(+ 1 2)))
  (test-case "2" (check-equal? (evallist '(+ 1 x) '((+ +) (x 5))) '(+ 1 5)))
  (test-case "3" (check-equal? (evallist '(+ 1 (+ 2 3)) '((+ +))) '(+ 1 5)))
  )

;; handle-if: check result of predicate and return appropriate result
(check-equal? (handle-if '(if (equal? 1 1) 55 99) '((equal? equal?))) 55)
(check-equal? (handle-if '(if (equal? 1 2) 55 99) '((equal? equal?))) 99)

;; body: return body of a lambda
(check-equal? (body '(lambda (x) (+ x 1))) '(+ x 1))
(check-equal? (body '(lambda (x y z) (+ x y (- z 1)))) '(+ x y (- z 1)))

;; formals: return formal parameters of a lambda
(check-equal? (formals '(lambda (x) (+ x 1))) '(x))
(check-equal? (formals '(lambda (x y z) (+ x y (- z 1)))) '(x y z))

;; funpart: return the lambda of a closure
(check-equal? (funpart '(closure (lambda (x) (+ x 1)))) '(lambda (x) (+ x 1)))
(check-equal? (formals '(closure (lambda (x y z) (+ x y (- z 1))))) '(lambda (x y z) (+ x y (- z 1))))

;; envpart: return the environment of a closure
(check-equal? (envpart '(closure (lambda (x) (+ x 1)) ((+ +)))) '((+ +)))

;; startEval: evaluate expression
(check-equal? (startEval 1 '()) '1)
; No symbol `x` defined raises error
(check-exn exn:fail? (lambda () (startEval 'x '())))
(check-equal? (startEval 'x '((x 5))) 5)
(check-equal? (startEval '(quote 5) '()) '5)
(check-equal? (startEval '(quote (1 2 3)) '()) '(1 2 3))
;(check-equal? (startEval '(closure 45) '()))

(run-tests list1-suite)
(run-tests list2-suite)
(run-tests list3-suite)
(run-tests *assoc-suite)
(run-tests mkassoc-suite)
(run-tests mkassoc*-suite)
(run-tests apply-binary-suite)
(run-tests apply-unary-suite)
(run-tests apply-value-op-suite)
(run-tests apply-suite)
(run-tests evallist-suite)


;; TODO
;; evallist
;; apply
;; handle-if