; List (at least two elements) -> Element
(define (2nd L) (cadr L))
; (= (2nd '(1 2 3)) 2)
; (= (2nd '(1 2)) 2)
; (2nd '(1)) = violation
; (2nd '()) = violation

; one : List -> Boolean
(define (one? L) (and (not (null? L)) (equal? (cdr L) '())))
; (eq? (one? '(9)) #t)
; (eq? (one? '(9 10)) #f)
; (eq? (one? '()) #f)

; insert : Number List -> List
(define (insert x L)
  (cond
    ((null? L) (cons x '()))
    ((< x (car L)) (cons x (cons (car L) (cdr L))))
    (else (cons (car L) (insert x (cdr L))))))
; (equal? (insert 2 '(1 5 7 10)) '(1 2 5 7 10))
; (equal? (insert 2 '()) '(2))

; insertion-sort : List -> List
(define (insertion-sort L)
  (cond
    ((or (one? L) (null? L)) L)
    (else (insert (car L) (insertion-sort (cdr L))))))
; (equal? (insertion-sort '(1 2 3 4 5)) '(1 2 3 4 5))
; (equal? (insertion-sort '(3 1 2 4 5)) '(1 2 3 4 5))
; (equal? (insertion-sort '(5 4 3 2 1)) '(1 2 3 4 5))

; Helper tail recursive function to implement index
; index-iter : NatNum Symbol List[Symbol] -> NatNum
(define (index-iter i sym syms)
  (cond
    ((null? syms) #f)
    ((eq? sym (car syms)) i)
    (else (index-iter (+ i 1) sym (cdr syms)))))
; (= (index-iter 0 'x '(x y z)) 0)
; (= (index-iter 0 'x '(y x z)) 1)
; (eq? (index-iter 0 'x '(a y z)) #f)

; index : List -> NatNum
(define (index sym syms) (index-iter 0 sym syms))
; (= (index 'x '(x)) 0)
; (= (index 'x '(z y x)) 2)
; (= (index 'x '(z x y)) 1)
; (= (index 'y '(x y y z)) 1)
; (eq? (index 'x '(a b c)) #f)
; (eq? (index 'x '()) #f)

; filter-by : Function List -> List
(define (filter-by p L)
  (cond
    ((null? L) L)
    ((p (car L)) (cons (car L) (filter-by p (cdr L))))
    (else (filter-by p (cdr L)))))
; (equal? (filter-by number? '()) '())
; (equal? (filter-by number? '(a b c)) '())
; (equal? (filter-by number? '(() a 5 (1 2 3) (x y z) b 10)) '(5 10))

; find-less : NatNum List -> List
(define (find-less pivot nums) (filter-by (lambda (x) (< x pivot)) nums))
; (equal? (find-less 5 '(1 2 3 4 5 5 5 5 5 6 7 8 9)) '(1 2 3 4))
; (equal? (find-less 5 '(6 7 8 9)) '())

; find-same : NatNum List -> List
(define (find-same pivot nums) (filter-by (lambda (x) (= x pivot)) nums))
; (equal? (find-same 5 '(1 2 3 4 5 5 5 5 5 6 7 8 9)) '(5 5 5 5 5))
; (equal? (find-same 5 '(1 2 3 4 6 7 8 9)) '())

; find-more : NatNum List -> List
(define (find-more pivot nums) (filter-by (lambda (x) (> x pivot)) nums))
; (equal? (find-more 5 '(3 4 5 6 7 8 9 10 100 1000 10000)) '(6 7 8 9 10 100 1000 10000))
; (equal? (find-more 5 '(3 4 5)) '())