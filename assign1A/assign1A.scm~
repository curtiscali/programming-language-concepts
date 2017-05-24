#lang racket

; List (at least two elements) -> Element
(define (2nd L) (cadr L))
; (2nd '(1 2 3)) = 2
; (2nd '(1 2)) = 2
; (2nd '(1)) = violation
; (2nd '()) = violation

; List -> Boolean
(define (one? L) (and (not (null? L)) (equal? (cdr L) '())))
; (one? '(9)) = #t
; (one? '(9 10)) = #f
; (one? '()) = #f

; Number, List -> List
(define (insert x L)
  (cond
    ((null? L) (cons x '()))
    ((< x (car L)) (cons x (cons (car L) (cdr L))))
    (else (cons (car L) (insert x (cdr L))))))
; (insert 2 '(1 5 7 10)) = '(1 2 5 7 10)

; List -> List
(define (insertion-sort L)
  (cond
    ((or (one? L) (null? L)) L)
    (else (insert (car L) (insertion-sort (cdr L))))))
; (insertion-sort '(1 2 3 4 5)) = '(1 2 3 4 5)
; (insertion-sort '(3 1 2 4 5)) = '(1 2 3 4 5)
; (insertion-sort '(5 4 3 2 1)) = '(1 2 3 4 5)

; Helper tail recursive function to implement index
; Int, Symbol, List[Symbol] -> Int
(define (index-iter i sym syms)
  (cond
    ((null? syms) #f)
    ((eq? sym (car syms)) i)
    (else (index-iter (+ i 1) sym (cdr syms)))))

; List -> Number
(define (index sym syms) (index-iter 0 sym syms))
; (index 'x '(x)) = 0
; (index 'x '(z y x)) = 2
; (index 'x '(z x y)) = 1
; (index 'y '(x y y z)) = 1
; (index 'x '(a b c)) = #f
; (index 'x '()) = #f

; Function, List -> List
(define (filter-by p L)
  (cond
    ((null? L) L)
    ((p (car L)) (cons (car L) (filter-by p (cdr L))))
    (else (filter-by p (cdr L)))))
; (filter-by number? '()) = '()
; (filter-by number? '(a b c)) = '()
; (filter-by number? '(() a 5 (1 2 3) (x y z) b 10)) = '(5 10)