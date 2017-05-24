;;; Author: Curtis Cali using code from Arthur Nunes-Harwitt

(require (lib "yacc.ss" "parser-tools")
         (lib "lex.ss" "parser-tools")
         (prefix : (lib "lex-sre.ss" "parser-tools")))

(require (lib "pretty.ss"))

(define-tokens value-tokens (NUM ID))

(define-empty-tokens op-tokens
  (BEGIN
   END
   SEMI
   EQ1
   EQ2
   IF
   THEN
   ELSE
   OP
   CP
   COMMA
   LET
   IN
   BSLASH
   ARROW
   PROCS
   +
   -
   *
   /
   EOF))

(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))
 (upper-letter (:/ "A" "Z"))
 (letter (:or lower-letter upper-letter))
 (digit (:/ "0" "9"))
 (ident (:+ letter))
 (number (:+ digit)))

;get-token: inputPort -> token
(define get-token
  (lexer
   ((eof) 'EOF)
   ("if" 'IF)
   ("then" 'THEN)
   ("else" 'ELSE)
   ("let" 'LET)
   ("in" 'IN)
   ("\\" 'BSLASH) ;just one, it looks like two because \ is an escape character
   ("->" 'ARROW)
   ("procedures" 'PROCS)
   ("{" 'BEGIN)
   ("}" 'END)
   ("(" 'OP)
   (")" 'CP)
   (";" 'SEMI)
   ("," 'COMMA)
   ("=" 'EQ1)
   ("==" 'EQ2)
   ("+" '+)
   ("-" '-)
   ("*" '*)
   ("/" '/)
   (number (token-NUM (string->number lexeme)))
   (ident (token-ID (string->symbol lexeme)))
   (whitespace (get-token input-port))))

; one? Any -> Bool
(define (one? L)
  (and
   (pair? L)
   (and
    (not (equal? (car L) '()))
    (equal? (cdr L) '()))))

;; An expression (Exp) is one of the following.

;; a Number n
;; an Identifier x

;; a sum with parts e1 and e2,

;;   where e1 and e2 are expressions
;; a let with parts x, e1, and e2,

;;   where x is an identifier
;;   and e1 and e2 are expressions
;; an if with parts e1, e2, and e3,
;;   where e1, e2, and e3 are expressions
;; a proc with parts x and e
;;   where x is an identifier and e is an expression
;; a funcall with parts e1 and e2
;;   where e1 and e2 are expressions
;; an assignment with parts x and e
;;   where x is an identifier and e is an expression

;; functions for associated with each part: predicate, constructor, selectors.

;; Number is a Scheme number


;; Identifier is a Scheme symbol


; ident?: Any -> Bool
(define ident? symbol?)
; (ident? 'x)


; sum?: Any -> Bool
(define (sum? a) (and (pair? a) (eq? (car a) '+)))
; (eq? (sum? '(+ 2 3)) #t)
; (eq? (sum? '3) #f)


; make-sum: Exp * Exp -> SumExp
(define (make-sum exp1 exp2)
  (list '+ exp1 exp2))
; (equal? (make-sum 2 3) '(+ 2 3))


; arg1: SumExp -> Exp
(define (arg1 e) (car (cdr e)))
; (= (arg1 (make-sum 2 3)) 2)

; arg2: SumExp -> Exp
(define (arg2 e) (car (cdr (cdr e))))
; (= (arg2 (make-sum 2 3)) 3)


; make-diff Exp * Exp -> DiffExp
(define (make-diff e1 e2) (list '- e1 e2))
;(equal? (make-diff 2 3) '(- 2 3))

; diff? Any -> Bool
(define (diff? e) (and (pair? e) (eq? (car e) '-)))
;(diff? (make-diff 3 2))
;(eq? (diff? 99) #f)


; make-prod Exp * Exp -> ProdExp
(define (make-prod e1 e2) (list '* e1 e2))
;(equal? (make-prod 2 3) '(* 2 3))

; prod? Any -> Bool
(define (prod? e) (and (pair? e) (eq? (car e) '*)))
;(prod? (make-prod 3 2))
;(eq? (prod? 99) #f)


; make-quo Exp * Exp -> QuoExp
(define (make-quo e1 e2) (list '/ e1 e2))
;(equal? (make-quo 2 3) '(/ 2 3))

; quo? Any -> Bool
(define (quo? e) (and (pair? e) (eq? (car e) '/)))
;(quo? (make-quo 3 2))
;(eq? (quo? 99) #f)


; make-neg Exp * Exp -> NegExp
(define (make-neg e) (list 'neg e))
;(equal? (make-neg 2 3) '(* 2 3))

; neg? Any -> Bool
(define (neg? e) (and (pair? e) (eq? (car e) 'neg)))
;(neg? (make-neg 3 2))
;(eq? (neg? 99) #f)

; neg-exp NegExp -> Exp
(define neg-exp arg1)
; (= (neg-exp '(neg 3)) 3)


; make-equality Exp * Exp -> EqualityExp
(define (make-equality e1 e2) (list '= e1 e2))
;(equal? (make-equality 2 3) '(* 2 3))

; equality? Any -> Bool
(define (equality? e) (and (pair? e) (eq? (car e) '=)))
;(equality? (make-equality 3 2))
;(eq? (equality? 99) #f)


; let?: Any -> Bool
(define (let? a) (and (pair? a) (eq? (car a) 'let)))
; (eq? (let? '(let x 1 3)) #t)
; (eq? (let? '3) #f)


; make-let: Identifier*Exp * Exp -> LetExp
; Identifier*Exp is represented as a two element list
(define (make-let var exp1 exp2)
  (list 'let var exp1 exp2))
; (equal? (make-let 'x 1 3) '(let x 1 3))


; let-var: LetExp -> Identifier
(define let-var cadr)
; (eq? (let-var (make-let 'x 1 3)) 'x)

  ; let-exp: LetExp -> Exp
(define let-exp caddr)
; (= (let-exp (make-let 'x 1 3)) 1)

; let-body: LetExp -> Exp
(define let-body cadddr)
; (= (let-body (make-let 'x 1 3)) 3)


; make-letstar List[Var, Exp] * Exp -> LetStarExp
(define (make-letstar assigns exp)
  (if (null? assigns)
      exp
      (let
          ((s1 (caar assigns))
           (e1 (cadar assigns)))
        (make-let s1 e1 (make-letstar (cdr assigns) exp)))))


; make-seq List[Exp] -> SeqExp
(define (make-seq exps)
  (if (one? exps)
      (car exps)
      (make-let '*temp* (car exps) (make-seq (cdr exps)))))


; if?: Any -> Bool
(define (if? a) (and (pair? a) (eq? (car a) 'if)))
; (if? (make-if 1 2 3))
; (not (if? 5))

; make-if: Exp * Exp * Exp -> IfExp
(define (make-if exp1 exp2 exp3)
  (list 'if exp1 exp2 exp3))
; (equal? (make-if 1 2 3) '(if 1 2 3))

; if-exp1: IfExp -> Exp
(define if-exp1 cadr)
; (= (if-exp1 (make-if 1 2 3)) 1)

; if-exp2: IfExp -> Exp
(define if-exp2 caddr)
; (= (if-exp2 (make-if 1 2 3)) 2)

; if-exp3: IfExp -> Exp
(define if-exp3 cadddr)
; (= (if-exp3 (make-if 1 2 3)) 3)

; proc?: Any -> Bool
(define (proc? a) (and (pair? a) (eq? (car a) 'proc)))
; (proc? '(proc x 1))
; (not (proc? 3))

; make-proc: Identifier * Exp -> ProcExp
(define (make-proc var exp)
  (list 'proc var exp))
; (equal? (make-proc 'x 1) '(proc x 1))

; proc-var: ProcExp -> Identifier
(define proc-var cadr)
; (eq? (proc-var (make-proc 'x 1)) 'x)

; proc-exp: ProcExp -> Exp
(define proc-exp caddr)
; (= (proc-exp (make-proc 'x 1)) 1)


; make-curried-proc List[Ident] * Exp -> CurriedProc
(define (make-curried-proc ids exp)
  (cond ((null? ids) (make-proc '*temp* exp))
        ((one? ids) (make-proc (car ids) exp))
        else (make-proc (car ids) (make-curried-proc (cdr ids) exp))))


; funcall? Any -> Bool
(define (funcall? a) (and (pair? a) (eq? (car a) 'funcall)))
; (funcall? '(funcall f 1))
; (not (funcall? 3))

; make-funcall: Exp * Exp -> FuncallExp
(define (make-funcall exp1 exp2)
  (list 'funcall exp1 exp2))
; (equal? (make-funcall 'f 1) '(funcall f 1))

; funcall-rator: FuncallExp -> Exp
(define funcall-rator cadr)
; (eq? (funcall-rator (make-funcall 'f 1)) 'f)

; funcall-rand: FuncallExp -> Exp
(define funcall-rand caddr)
; (= (funcall-rand (make-funcall 'f 1)) 1)


; make-curried-funcall Exp * List[Exp] -> CurriedFuncall
(define (make-curried-funcall f exps)
  (cond ((null? exps) (make-funcall f 0))
        ((one? exps) (make-funcall f (car exps)))
        (else (make-curried-funcall
               (make-funcall f (car exps))
               (cdr exps)))))


; assign?: Any -> Bool
(define (assign? a) (and (pair? a) (eq? (car a) 'assign!)))
; (eq? (assign? '(assign! x 3)) #t)
; (eq? (assign? '3) #f)


; make-assign: Identifier * LangExp -> AssignExp
(define (make-assign var exp)
  (list 'assign! var exp))
; (equal? (make-assign 'x 3) (assign! x 3))

; assign-var: AssignExp -> Identifier
(define assign-var cadr)
; (eq? (assign-var (make-assign 'x 3)) 'x)

; assign-exp: AssignExp -> LangExp
(define assign-exp caddr)
; (= (assign-exp (make-assign 'x 3)) 3)


; parse-lang: (() -> token) -> SmallLangExp
(define parse-lang
  (parser
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (error 'parse-lang "error occurred, ~v ~v ~v" a b c)))
   (grammar
    (exp ((BEGIN exps END) (make-seq $2))
         ((LET let-defs IN exp) (make-letstar $2 $4))
         ((PROCS rec-defs IN exp) (make-procs $2 $4))
         ((IF exp THEN exp ELSE exp) (make-if $2 $4 $6))
         ((BSLASH formals BSLASH ARROW  exp ) (make-curried-proc $2 $5))
         ((ID EQ1 exp) (make-assign $1 $3))
         ((comp-exp) $1))
    (exps ((exp) (list $1))
          ((exp SEMI exps) (cons $1 $3)))
    (let-def ((ID EQ1 exp) (list $1 $3)))
    (let-defs ((let-def) (list $1))
              ((let-def COMMA let-defs) (cons $1 $3)))
    (rec-def ((ID OP formals CP EQ1 exp) (list $1 (make-curried-proc $3 $6))))
    (rec-defs ((rec-def) (list $1))
              ((rec-def COMMA rec-defs) (cons $1 $3)))
    (comp-exp ((math-exp EQ2 math-exp) (make-equality $1 $3))
              ((math-exp) $1))
    (math-exp ((math-exp + term) (make-sum $1 $3))
              ((math-exp - term) (make-diff $1 $3))
              ((term) $1))
    (term ((term * factor) (make-prod $1 $3))
          ((term / factor) (make-quo $1 $3))
          ((factor) $1))
    (factor ((simple) $1)
            ((NUM) $1)
            ((- factor) (make-neg $2))
            ((simple OP actuals CP) (make-curried-funcall $1 $3)))
    (simple ((ID) $1)
            ((OP exp CP) $2))
    (actuals (() null)
             ((actualsNE) (reverse $1)))
    (actualsNE ((exp) (list $1))
               ((actualsNE COMMA exp) (cons $3 $1)))
    (formals (() null)
             ((formalsNE) (reverse $1)))
    (formalsNE ((ID) (list $1))
               ((formalsNE COMMA ID) (cons $3 $1))))))


; lexer/parser test
(let* ((example "2+3+4")
       (i (open-input-string example)))
  (equal? (parse-lang (lambda () (get-token i)))
          '(+ (+ 2 3) 4)))

(let* ((example "-1")
       (i (open-input-string example)))
  (equal? (parse-lang (lambda () (get-token i)))
          '(neg 1)))


;; An environment (Env) is a Scheme function from Identifier to Loc.
;; where a Loc is a NatNum

(define empty-env
  (lambda (var) (error 'empty-env "variable undefined")))

; apply-env: Env * Identifier -> Loc
(define (apply-env env var) (env var))

; extend-env: Env * Identifier * Loc -> Env
(define (extend-env env var val)
  (lambda (var2)
    (if (eq? var var2)
        val
        (env var2))))

;; A store (Store) is a list of Loc * Val
;; where a Loc is a NatNum

(define empty-store '())

; extend-store: Store * NatNum * Val -> Store
(define (extend-store store loc v) (cons (cons loc v) store))

; apply-store: Store * NatNum -> Val
(define (apply-store store loc)
  (if (null? store)
      (error 'apply-store "location undefined")
      (if (= (car (car store)) loc)
          (cdr (car store))
          (apply-store (cdr store) loc))))

;; continuations

(define init-k (lambda (v store) v))

;; denotation

(define (boolify num) num)


; meaning: Exp * Env * (Val * Store -> A) * Store -> A
(define (meaning exp env k store)
  (cond ((number? exp) (k exp store))
        ((ident? exp)(k (apply-store store (apply-env env exp)) store))
        ((sum? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1 store2)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2 store3)
                               (k (+ v1 v2) store3))
                             store2))
                  store))
        ((diff? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1 store2)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2 store3)
                               (k (- v1 v2) store3))
                             store2))
                  store))
        ((prod? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1 store2)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2 store3)
                               (k (* v1 v2) store3))
                             store2))
                  store))
        ((quo? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1 store2)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2 store3)
                               (k (/ v1 v2) store3))
                             store2))
                  store))
        ((neg? exp)
         (meaning (neg-exp exp)
                  env
                  (lambda (v1 store2)
                    (k (* v1 -1) store2))
                  store))
        ((let? exp)
         (meaning (let-exp exp)
                  env
                  (lambda (v store2)
                    (let ((new-loc (length store2)))
                      (meaning (let-body exp)
                               (extend-env env (let-var exp) new-loc)
                               k
                               (extend-store store2 new-loc v))))
                  store))
        ((equality? exp)
         (meaning (arg1 exp)
                  env
                  (lambda (v1 store2)
                    (meaning (arg2 exp)
                             env
                             (lambda (v2 store3)
                               (k (= v1 v2) store3))
                             store2))
                  store))
        ((if? exp)
         (meaning (if-exp1 exp)
                  env
                  (lambda (v store2)
                    (if (boolify v)
                        (meaning (if-exp2 exp) env k store2)
                        (meaning (if-exp3 exp) env k store2)))
                  store))
        ((proc? exp)
         (k (lambda (v k2 store2)
              (let ((new-loc (length store2)))
                (meaning (proc-exp exp)
                         (extend-env env (proc-var exp) new-loc)
                         k2
                         (extend-store store2 new-loc v))))
            store))
        ((funcall? exp)
         (meaning (funcall-rator exp)
                  env
                  (lambda (f store2)
                    (meaning (funcall-rand exp)
                             env
                             (lambda (v store3) (f v k store3))
                             store2))
                  store))
        ((assign? exp)
         (meaning (assign-exp exp)
                  env
                  (lambda (v store2)
                    (k v (extend-store store2 (apply-env env (assign-var exp)) v)))
                  store))
      (else (error 'meaning "Unknown expression"))))

; lexer/parser/meaning test

(let* ((example "-1")
       (i (open-input-string example)))
    (= (meaning (parse-lang (lambda () (get-token i))) empty-env init-k empty-store) -1))