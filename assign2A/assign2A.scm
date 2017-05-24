; Terminals are quoted.
; A rule A -> X1 ... Xn is written (A (X1 ... Xn))
; A grammar is a list of rules.

(define *grammar*
  '((S (E 'eof))
    (E (T E2))
    (E2 ('+ T E2))
    (E2 ('- T E2))
    (E2 ())
    (T (F T2))
    (T2 ('* F T2))
    (T2 ('/ F T2))
    (T2 ())
    (F ('n))
    (F ('id))
    (F ('- F))
    (F ('OP E 'CP))))

; rule-lhs : Rule -> Variable
(define rule-lhs car)

; (eq? (rule-lhs '(E (T E2))) 'E)

; rule-rhs : Rule -> List(Variables or Terminals)
(define rule-rhs cadr)

; (equal? (rule-rhs '(E (T E2))) '(T E2))

; variable? : Any -> Boolean
(define variable? symbol?)

; (variable? 'E)
; (not (variable? ''+))

; terminal? : Any -> Boolean
(define (terminal? a) (and (pair? a) (eq? (car a) 'quote)))
; (terminal? ''+)
; (not (terminal? 'E))

(define (beta rhs alpha)
  (cond ((null? rhs) '())
        ((eq? (car rhs) alpha) (cdr rhs))
        (else (beta (cdr rhs) alpha))))
; (equal? (beta '('OP E 'CP) 'E) '('CP))

; union : Set Set -> Set
(define (union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (member (car set1) set2)) (cons (car set1) (union (cdr set1) set2)))
        (else (union (cdr set1) set2))))
; (equal? (union '(1 2 3 4) '(5 2 6 3)) '(1 4 5 2 6 3))

; first3 Grammar RHS List[Rule] -> List[Terminal]
(define (first3 grammar alpha seen)
  (cond ((null? alpha) '())
        ((terminal? (car alpha)) (list (car alpha)))
        (else
         (let ((pi-hat (filter (lambda (rule) (eq? (car alpha) (rule-lhs rule))) grammar)))
           (let
               ((expanded (first-var3 grammar pi-hat seen)))
             (if (member '() expanded)
                 (first3 grammar (cdr alpha) seen)
                 (filter (lambda (sigma) (not (equal? sigma '()))) (first-var3 grammar pi-hat seen))))))
        ))
; (equal? (first3 *grammar* '() '()) '())
; (equal? (first3 *grammar* '('* F T2) '()) '('*))

; first-alpha Grammar List[Rule List[Rule -> List[Terminal]
(define (first-var3 grammar rules seen)
  (cond ((null? rules) '())
        ((member (car rules) seen) (first-var3 grammar (cdr rules) seen))
        (else
         (union
          (first3 grammar (rule-rhs (car rules)) seen)
          (first-var3 grammar (cdr rules) (cons (car rules) seen))))))

; first-alpha Grammar RHS -> List[Terminal]
(define (first-alpha grammar alpha)
  (let ([combined (union
                   (first3 grammar alpha '())
                   (first-var3 grammar (filter (lambda (r) (equal? alpha (rule-rhs r))) grammar) '()))])
    (if (null? combined) (list combined) (map cadr combined))))
;(equal? (first-alpha *grammar* '(T E2)) '(n id - OP))
;(equal? (first-alpha *grammar* '('+ T E2)) '(+))
;(equal? (first-alpha *grammar* '()) '(()))

;((and (member var (rule-rhs (car rules))) (member '() (first-alpha grammar (beta (rule-rhs (car rules)) var))))
;         (follow-rules4 grammar (rule-lhs (car rules)) grammar (cons (car rules) seen)))

;(union

; follow-rules4 Grammar LHS List[Rule] List[Rule] -> List[Terminal]
(define (follow-rules4 grammar var rules seen)
  (cond ((null? rules) '())
        ((member (car rules) seen) (follow-rules4 grammar var (cdr rules) seen))
        ((and (member var (rule-rhs (car rules))) (member '() (first-alpha grammar (beta (rule-rhs (car rules)) var))))
         (follow-rules4 grammar (rule-lhs (car rules)) grammar (cons (car rules) seen)))
        ((member var (rule-rhs (car rules)))
         (union
          (filter (lambda (sigma) (not (equal? sigma '()))) (first-alpha grammar (beta (rule-rhs (car rules)) var)))
          (follow-rules4 grammar var (cdr rules) seen)))
        (else (follow-rules4 grammar var (cdr rules) seen))))

(define (follow-var grammar var) (follow-rules4 grammar var grammar '()))
; (equal? (follow-var *grammar* 'E) '(eof CP))
; (equal? (follow-var *grammar* 'E2) '(eof CP))
(equal? (follow-var *grammar* 'T) '('+ '- 'eof 'CP))
(equal? (follow-var *grammar* 'F) '('* '/ '+ '- 'eof 'CP))