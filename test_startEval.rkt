#lang racket/base

(require rackunit "startEval.rkt")
(require rackunit/text-ui)

(define-test-suite mk-list-of-one-suite
  "mk-list-of-one test suite"
  (test-case "empty list" (check-equal? (mk-list-of-one '()) '(())))
  (test-case "number" (check-equal? (mk-list-of-one '5) '(5)))
  (test-case "list" (check-equal? (mk-list-of-one '(1 2 3)) '((1 2 3))))
  )

(define-test-suite mk-list-of-two-suite
  "mk-list-of-two test suite"
  (test-case "numbers" (check-equal? (mk-list-of-two 1 2) '(1 2)))
  (test-case "empty list" (check-equal? (mk-list-of-two '() 2) '(() 2)))
  (test-case "nested list" (check-equal? (mk-list-of-two '(1 2 3) '(4 5 6)) '((1 2 3) (4 5 6))))
  )

(define-test-suite mk-list-of-three-suite
  "mk-list-of-three test suite"
  (test-case "integers" (check-equal? (mk-list-of-three 1 2 3) '(1 2 3)))
  (test-case "empty lists" (check-equal? (mk-list-of-three '() '() '()) '(() () ())))
  (test-case "nested lists" (check-equal? (mk-list-of-three '(1 2 3) '(4 5 6) '(7 8 9)) '((1 2 3) (4 5 6) (7 8 9))))
  )

(define-test-suite dict-get-suite
  "dict-get test suite"
  ;; dict-get: modify racket assoc to return only value, not k/v pair
  (test-case "in" (check-equal? (dict-get 1 '((1 2))) 2))
  (test-case "not in" (check-equal? (dict-get 1 '((2 3))) #f))
  (test-case "in second" (check-equal? (dict-get 2 '((1 2) (2 3))) 3))
  )

(define-test-suite dict-update-suite
  "dict-update test suite"
  ;; dict-update: return a list of key value pairs
  (test-case "none in list" (check-equal? (dict-update 1 2 '()) '((1 2))))
  (test-case "overwrite key" (check-equal? (dict-update 1 2 '((1 3))) '((1 2))))
  (test-case "some in list" (check-equal? (dict-update 1 2 '((3 4))) '((3 4) (1 2))))
  )

(define-test-suite dict-update-rec-suite
  "dict-update-rec test suite"
  ;; dict-update: recursively call dict-update to add multiple key values at once
  (test-case "1" (check-equal? (dict-update-rec '(1 2 3) '(4 5 6) '()) '((1 4) (2 5) (3 6))))
  (test-case "2" (check-equal? (dict-update-rec '(1 2) '(4 5) '((9 10))) '((9 10) (1 4) (2 5))))
  (test-case "3" (check-equal? (dict-update-rec '(1 2) '(4 5) '((1 10))) '((1 4) (2 5))))
  )

(define-test-suite binary-operator-suite
  "binary-operator-suite"
  ;; binary-operator: apply f to x and y
  (test-case "+" (check-equal? (binary-operator '+ 10 5) 15))
  (test-case "-" (check-equal? (binary-operator '- 10 5) 5))
  (test-case "*" (check-equal? (binary-operator '* 10 5) 50))
  (test-case "/" (check-equal? (binary-operator '/ 10 5) 2))
  (test-case "<" (check-equal? (binary-operator '< 10 5) #f))
  (test-case "<" (check-equal? (binary-operator '< 5 10) #t))
  (test-case "<" (check-equal? (binary-operator '< 10 10) #f))
  (test-case ">" (check-equal? (binary-operator '> 10 5) #t))
  (test-case ">" (check-equal? (binary-operator '> 5 10) #f))
  (test-case ">" (check-equal? (binary-operator '> 10 10) #f))
  (test-case ">=" (check-equal? (binary-operator '>= 10 5) #t))
  (test-case ">=" (check-equal? (binary-operator '>= 5 10) #f))
  (test-case ">=" (check-equal? (binary-operator '>= 10 10) #t))
  (test-case "<=" (check-equal? (binary-operator '<= 10 5) #f))
  (test-case "<=" (check-equal? (binary-operator '<= 5 10) #t))
  (test-case "<=" (check-equal? (binary-operator '<= 10 10) #t))
  (test-case "=" (check-equal? (binary-operator '= 10 10) #t))
  (test-case "equal?" (check-equal? (binary-operator 'equal? "hi" "hi") #t))
  (test-case "equal?" (check-equal? (binary-operator 'equal? "hi" "bye") #f))
  )

(define-test-suite unary-operator-suite
  "unary-operator test suite"
  ;; unary-operator: apply f to x
  (test-case "car" (check-equal? (unary-operator 'car '(1 2 3)) 1))
  (test-case "cdr" (check-equal? (unary-operator 'cdr '(1 2 3)) '(2 3)))
  (test-case "cdr" (check-equal? (unary-operator 'cdr '(1)) '()))
  (test-case "pair?" (check-equal? (unary-operator 'pair? '(1 2)) #t))
  (test-case "pair?" (check-equal? (unary-operator 'pair? '()) #f))
  (test-case "pair?" (check-equal? (unary-operator 'pair? '1) #f))
  )

(define-test-suite operator-suite
  "operator test suite"
  ;; operator: apply operator to arguments
  (test-case "+" (check-equal? (operator '(operator +) '(1 2)) 3))
  (test-case "-" (check-equal? (operator '(operator -) '(1 2)) -1))
  (test-case "equal?" (check-equal? (operator '(operator equal?) '(1 2)) #f))
  (test-case "<" (check-equal? (operator '(operator <) '(1 2)) #t))
  )

(define-test-suite evaluate-expression-rec-suite
  "evaluate-expression-rec test suite"
  ;; evaluate-expression-rec: recursively evaluate expressions
  (test-case "1" (check-equal? (evaluate-expression-rec '(+ 1 2) '((+ (operator +)))) '((operator +) 1 2)))
  (test-case "2" (check-equal? (evaluate-expression-rec '(+ 1 x) '((+ (operator +)) (x 5))) '((operator +) 1 5)))
  (test-case "3" (check-equal? (evaluate-expression-rec '(+ 1 (+ 2 3)) '((+ (operator +)))) '((operator +) 1 5)))
  )

(define-test-suite handle-if-suite
  "handle-if test suite"
  ;; handle-if: check result of predicate and return appropriate result
  (test-case "true" (check-equal? (handle-if '(if (equal? 1 1) 55 99) '((equal? (operator equal?)))) 55))
  (test-case "false" (check-equal? (handle-if '(if (equal? 1 2) 55 99) '((equal? (operator equal?)))) 99))
  )

(define-test-suite closure-suite
  "closure test suite"
  ;; closure: apply closure to arguments
  (test-case "1" (check-equal? (closure '(closure (lambda (x) x) ()) '(5)) 5))
  (test-case "2" (check-equal? (closure '(closure (lambda (x) (+ 1 x)) ((+ (operator +)))) '(5)) 6))
  (test-case "3" (check-equal? (closure '(closure (lambda (x) (+ x y)) ((+ (operator +)) (y 10))) '(5)) 15))
  (test-case "4" (check-equal? (closure '(closure (lambda (x y) (+ x y)) ((+ (operator +)))) '(99 99)) 198))
  (test-case "5" (check-equal? (closure '(closure (lambda (x y) (+ q p)) ((+ (operator +)) (q 10) (p 15))) '(99 99)) 25))
  )

(define-test-suite get-first-of-all-suite
  "get-first-of-all test suite"
  ;; get-first-of-all: get the first element of every sublist
  (test-case "empty" (check-equal? (get-first-of-all '()) '()))
  (test-case "one" (check-equal? (get-first-of-all '((1 2))) '(1)))
  (test-case "two" (check-equal? (get-first-of-all '((1 2) (3 4))) '(1 3)))
  )

(define-test-suite get-second-of-all-suite
  "get-second-of-all test suite"
  ;; get-second-of-all: get the second element of every sublist
  (test-case "empty" (check-equal? (get-second-of-all '()) '()))
  (test-case "one" (check-equal? (get-second-of-all '((1 2))) '(2)))
  (test-case "two" (check-equal? (get-second-of-all '((1 2) (3 4))) '(2 4)))
  )

(define-test-suite convert-let-suite
  "convert-let test suite"
  ;; convert let: convert a let expression into a lambda
  (test-case "1" (check-equal? (convert-let '(let ((x 1) (y 2)) 55)) '((lambda (x y) 55) 1 2)))
  (test-case "2" (check-equal? (convert-let '(let ((x 1)) (+ x 10))) '((lambda (x) (+ x 10)) 1)))
  )

(define-test-suite *startEval-suite
  "*startEval test suite"
  ;; *startEval: evaluate expression
  (test-case "1" (check-equal? (startEval 1) '1))
  (test-case "2" (check-exn exn:fail? (lambda () (*startEval 'x))))
  (test-case "3" (check-equal? (*startEval 'x '((x 5))) 5))
  (test-case "4" (check-equal? (startEval '(quote 5)) '5))
  (test-case "5" (check-equal? (startEval '(quote (1 2 3))) '(1 2 3)))
  (test-case "6" (check-equal? (startEval '(quote (quote (quote 5)))) '''5))
  (test-case "7" (check-equal? (startEval '((lambda (x) x) 5)) 5))
  (test-case "8" (check-equal? (*startEval '((lambda (x y) (+ x y)) 5 6) '((+ (operator +)))) 11))
  (test-case "9" (check-equal? (*startEval '((lambda (x y) (+ p q)) 1 1) '((+ (operator +)) (p 5) (q 10))) 15))
  (test-case "10" (check-equal? (startEval '(let ((x 1)) x)) 1))
  (test-case "11" (check-equal? (startEval '(let ((+ (lambda (x) (cdr x))) (- '(1 2 3 4 5))) (+ -))) '(2 3 4 5)))
  (test-case "12" (check-equal? (startEval '(let ((inc (lambda (x) (+ x (quote 1))))) (inc (quote 5)))) '6))
  )

(run-tests mk-list-of-one-suite)
(run-tests mk-list-of-two-suite)
(run-tests mk-list-of-three-suite)
(run-tests dict-get-suite)
(run-tests dict-update-suite)
(run-tests dict-update-rec-suite)
(run-tests binary-operator-suite)
(run-tests unary-operator-suite)
(run-tests operator-suite)
(run-tests evaluate-expression-rec-suite)
(run-tests handle-if-suite)
(run-tests closure-suite)
(run-tests get-first-of-all-suite)
(run-tests get-second-of-all-suite)
(run-tests convert-let-suite)
(run-tests *startEval-suite)
