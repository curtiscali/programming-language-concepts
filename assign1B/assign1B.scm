; one : List -> Boolean
(define (one? L) (and (not (null? L)) (equal? (cdr L) '())))
; (eq? (one? '(9)) #t)
; (eq? (one? '(9 10)) #f)
; (eq? (one? '()) #f)

; take : Int List -> List
(define (take n L)
  (cond ((null? L) L)
        ((<= n 0) '())
        (else (cons (car L) (take (- n 1) (cdr L))))))
;(equal? (take 1 '(1 2 3)) '(1))
;(equal? (take 2 '(1 2 3)) '(1 2))
;(equal? (take 3 '(1 2 3)) '(1 2 3))
;(equal? (take 4 '(1 2 3)) '(1 2 3))

; drop : Int List -> List
(define (drop n L)
  (cond ((null? L) L)
        ((<= n 0) L)
        (else (drop (- n 1) (cdr L)))))
;(equal? (drop 0 '(1 2 3)) '(1 2 3))
;(equal? (drop 1 '(1 2 3)) '(2 3))
;(equal? (drop 2 '(1 2 3)) '(3))
;(equal? (drop 3 '(1 2 3)) '())
;(equal? (drop 4 '(1 2 3)) '())

; merge : List List -> List
(define (merge ordered1 ordered2)
  (cond ((null? ordered1) ordered2)
        ((null? ordered2) ordered1)
        ((> (car ordered1) (car ordered2))
         (cons (car ordered2) (merge ordered1 (cdr ordered2))))
        (else
         (cons (car ordered1) (merge (cdr ordered1) ordered2)))))
; (equal? (merge '() '()) '())
; (equal? (merge '(1 2 3 4) '()) '(1 2 3 4))
; (equal? (merge '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
; (equal? (merge '(2 4 6 8) '(3)) '(2 3 4 6 8))
; (equal? (merge '(2 4 6 8) '(1 3 5 7)) '(1 2 3 4 5 6 7 8))
; (equal? (merge '(1 3 5 7) '(2 4 6 8)) '(1 2 3 4 5 6 7 8))

; merge-sort List -> List
(define (merge-sort L)
  (if (or (null? L) (one? L)) 
      L
      (let ((amount (/ (length L) 2)))
        (let ((left-sorted (merge-sort (take amount L)))
              (right-sorted (merge-sort (drop amount L))))
          (merge left-sorted right-sorted)))))
; (equal? (merge-sort '(1 2 3 4 5 6)) '(1 2 3 4 5 6))
; (equal? (merge-sort '(1 3 6 4 2 5)) '(1 2 3 4 5 6))
; (equal? (merge-sort '(6 5 4 3 2 1)) '(1 2 3 4 5 6))

; Code for Derivatives

; variable? : Any -> Bool
(define variable? symbol?)
; (eq? (variable? 'x) #t)
; (eq? (variable? 12) #f)

; arg1 : Exp -> Exp
(define arg1 cadr)
; (= (arg1 '(+ 2 3)) 2)
; (= (arg1 '(+ 1 3)) 1)

; arg2 : Exp -> Exp
(define arg2 caddr)
; (= (arg2 '(+ 2 3)) 3)
; (= (arg2 '(+ 1 4)) 4)

; variable=? : Variable Variable -> Bool
(define variable=? eq?)
; (eq? (variable=? 'x 'x) #t)
; (eq? (variable=? 'x 'y) #f)

; make-sum : Exp Exp -> SumExp 
(define (make-sum e1 e2)
  (cond ((and (number? e1) (number? e2)) (+ e1 e2))
         ((and (number? e2) (= e2 0)) e1)
         ((and (number? e1) (= e1 0)) e2)
         (else (list '+ e1 e2))))
; (= (make-sum 2 3) 5)
; (eq? (make-sum 'x 0) 'x)
; (eq? (make-sum 0 'x) 'x)
; (equal? (make-sum 'x 2) '(+ x 2))

; sum? : Any -> Bool
(define (sum? a) (and (pair? a) (eq? (car a) '+)))
; (eq? (sum? (make-sum 2 3)) #t)
; (eq? (sum? '()) #f)

; make-prod : Exp Exp -> ProdExp
(define (make-product e1 e2)
  (cond ((and (number? e1) (number? e2)) (* e1 e2))
         ((and (number? e2) (= e2 0)) 0)
         ((and (number? e1) (= e1 0)) 0)
         ((and (number? e2) (= e2 1)) e1)
         ((and (number? e1) (= e1 1)) e2)
         (else (list '* e1 e2))))
; (equal? (make-product 2 3) 6)
; (eq? (make-product 'x 0) 0)
; (eq? (make-product 0 'x) 0)
; (eq? (make-product 'x 1) 'x)
; (eq? (make-product 1 'x) 'x)
; (equal? (make-product 'x 2) '(* x 2))

; prod? : Any -> Bool
(define (product? a) (and (pair? a) (eq? (car a) '*)))
; (eq? (prod? (make-prod 2 3)) #t)
; (eq? (sum? '()) #f)

; make-expt : Exp Int -> ExptExp
(define (make-expt e n)
  (cond ((and (number? e) (number? n)) (expt e n))
         ((and (number? n) (= n 0)) 1)
         ((and (number? n) (= n 1)) e)
         (else (list '^ e n))))
; (= (make-expt 2 3) 8)
; (= (make-expt 'x 0) 1)
; (eq? (make-expt 'x 1) 'x)
; (equal? (make-expt 'x 4) '(^ x 4))

; expt? : Any -> Bool
(define (expt? a) (and (pair? a) (eq? (car a) '^)))
; (eq? (expt? (make-expt 'x 4)) #t)
; (eq? (expt? '()) #f)

; deriv : ArithExp VarExp -> ArithExp
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (variable=? exp var) 1 0))
        ((sum? exp) 
         (make-sum (deriv (arg1 exp) var)
                   (deriv (arg2 exp) var)))
        ((product? exp)
         (make-sum (make-product (arg1 exp) (deriv (arg2 exp) var))
                   (make-product (arg2 exp) (deriv (arg1 exp) var))))
        ((expt? exp) (make-product
                      (make-product (arg2 exp) (make-expt (arg1 exp) (- (arg2 exp) 1)))
                      (deriv (arg1 exp) var)))
        (else (error 'deriv "Unexpected Input, not an ArithExp"))))
; (= (deriv 1 'x) 0)
; (= (deriv 'y 'x) 0)
; (= (deriv 'x 'x) 1)
; (= (deriv (make-sum 'x 4) 'x) 1)
; (= (deriv (make-product 'x 4) 'x) 4)
; (equal? (deriv (make-expt 'x 4) 'x) (make-product 4 (make-expt 'x 3)))
; (= (deriv (make-expt 'x 4) 'y) 0)

; Code for Integrals

; zero-poly? : Poly -> Bool
(define zero-poly? null?)
;(eq? (zero-poly? '()) #t)
;(eq? (zero-poly? '(1 2 3)) #f)

; const-poly? : Poly -> Bool
(define const-poly? one?)
;(eq? (const-poly? '()) #f)
;(eq? (const-poly? '(5)) #t)
;(eq? (const-poly? '(0 1)) #f)

; poly<-const : Number -> Poly
(define (poly<-const const)
  (if (not (number? const))
      (error "Must pass a number")
      (if (= const 0)
          '()
          (list const))))
; (equal? (poly<-const 4) '(4))
; (eq? (zero-poly? (poly<-const 0)) #t)

; poly<-var -> Poly
(define poly<-var '(0 1))

; shift-left Poly -> Poly
(define (shift-left poly) (cons 0 poly))
; (equal? (shift-left poly<-var) '(0 0 1))

; shift-right : Poly -> Poly
(define (shift-right poly) (cdr poly))
; (equal? (shift-right poly<-var) '(1))

; const-coeff : Number Poly -> Poly
(define (const-coeff C poly)
  (if (zero-poly? poly)
      poly
      (cons (* C (car poly)) (const-coeff C (shift-right poly)))))
;(equal? (const-coeff 3 '(1 2 3)) '(3 6 9))
;(equal? (const-coeff 0 '(1 2 3)) '(0 0 0))

; add-const-poly : Number Poly -> Poly
(define (add-const-poly c poly) (cons (+ (car poly) c) (shift-right poly)))
;(equal? (add-const-poly 3 '(1 2 3)) '(4 2 3))
;(equal? (add-const-poly 0 '(1 2 3)) '(1 2 3))

; add-poly : Poly Poly -> Poly
(define (add-poly p1 p2)
  (cond ((null? p1) p2)
        ((null? p2) p1)
        (else (cons (+ (car p1) (car p2)) (add-poly (shift-right p1) (shift-right p2))))))
;(equal? (add-poly poly<-var (poly<-const 5)) '(5 1))
;(equal? (add-poly poly<-var (poly<-const 0)) poly<-var)
;(equal? (add-poly '(1 2 3) '(5 6)) '(6 8 3))

; mult-poly : Poly Poly -> Poly
(define (mult-poly p1 p2)
  (cond ((zero-poly? p1) '())
        (else
         (let ((p-p2 (const-coeff (car p1) p2))
              (x-p1-p2 (shift-left (mult-poly (shift-right p1) p2))))
          (add-poly p-p2 x-p1-p2)))))
; (equal? (mult-poly '(2 1) '(2 3 1)) '(4 8 5 1))

; expt-poly Poly NatNum -> Poly
(define (expt-poly poly n)
  (if (= n 0)
      (poly<-const 1)
      (mult-poly poly (expt-poly poly (- n 1)))))
; (equal? (expt-poly poly<-var 0) (poly<-const 1))
; (equal? (expt-poly poly<-var 1) poly<-var)
; (equal? (expt-poly poly<-var 2) '(0 0 1))

; exp<-poly-iter : Poly Symbol NatNum -> Exp
(define (exp<-poly-iter poly var degree)
  (cond ((zero-poly? poly) 0)
        (else
         (make-sum (make-product (car poly) (make-expt var degree)) (exp<-poly-iter (shift-right poly) var (+ degree 1))))))

; exp<-poly : Poly Symbol -> Exp
(define (exp<-poly poly var) (exp<-poly-iter poly var 0))
;(equal? (exp<-poly '(3 2 1) 'x) '(+ 3 (+ (* 2 x) (^ x 2))))
;(equal? (exp<-poly '(3 0 1) 'x) '(+ 3 (^ x 2)))
;(= (exp<-poly '() 'x) 0)

; poly<-exp : Exp -> Poly
(define (poly<-exp exp var)
  (cond
    ((and (number? exp) (= exp 0)) '())
    ((number? exp) (poly<-const exp))
    ((and (variable? exp) (not (variable=? exp var))) (error "Single variable expressions only"))
    ((variable? exp) poly<-var)
    ((sum? exp)
     (if (number? (arg1 exp))
         (add-const-poly (arg1 exp) (poly<-exp (arg2 exp)))
         (add-poly (poly<-exp (arg1 exp)) (poly<-exp (arg2 exp)))))
    ((product? exp)
     (if (number? (arg1 exp))
         (const-coeff (arg1 exp) (poly<-exp (arg2 exp)))
         (mult-poly (poly<-exp (arg1 exp) (poly<-exp (arg2 exp))))))
    ((expt? exp) (expt-poly (poly<-exp (arg1 exp)) (arg2 exp)))))
; (equal? (poly<-exp '(+ x 1)) '(1 1))
; (equal? (poly<-exp '(+ 3 (+ (* 2 x) (^ x 2)))) '(3 2 1))
; (equal? (poly<-exp '(+ 3 (^ x 2))) '(3 0 1))
; (equal? (poly<-exp 0) '())

; integrate-poly-iter : Poly NatNum -> Poly
(define (integrate-poly-iter poly i)
  (if (zero-poly? poly)
      '()
      (cons (/ (car poly) (+ i 1)) (integrate-poly-iter (shift-right poly) (+ i 1)))))

; integrate-poly : Poly -> Poly
(define (integrate-poly poly var) (shift-left (integrate-poly-iter poly 0)))
;(equal? (integrate-poly '() 'x) '(0))
;(equal? (integrate-poly poly<-var 'x) '(0 0 1/2))
;(equal? (integrate-poly '(1 4 3) 'x) '(0 1 2 1))

; integral : Exp Symbol -> Exp
(define (integral exp var)
  (exp<-poly (integrate-poly (poly<-exp exp var) var)))