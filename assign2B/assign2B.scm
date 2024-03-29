;; Author: Arthur Nunes-Harwitt

;; Import the parser and lexer generators.

(require (lib "yacc.ss" "parser-tools")
         (lib "lex.ss" "parser-tools")
         (prefix : (lib "lex-sre.ss" "parser-tools")))

(require (lib "pretty.ss"))

(define-tokens value-tokens (NUM ID))

(define-empty-tokens op-tokens
  (OP 
   CP
   COMMA
   EQ1
   LET 
   IN 
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
 (idfirst (:or letter (:or "_" "$")))
 (idrest (:or idfirst digit))
 (ident (:: idfirst (:* idrest)))
 (digits (:+ digit))
 (number
  (::
   (:: digits (:? (:: "." digits)))
   (:?
    (::
     (:or "E" "e")
     (:: (:? (:or "+" "-")) digits))))))

;get-token: inputPort -> token
(define get-token
  (lexer
   ((eof) 'EOF)
   ("let" 'LET)
   ("in" 'IN)
   ("(" 'OP)
   (")" 'CP)
   ("," 'COMMA)
   ("=" 'EQ1)
   ("+" '+)
   ("-" '-)
   ("*" '*)
   ("/" '/)
   (number (token-NUM (string->number lexeme)))
   (ident (token-ID (string->symbol lexeme)))
   (whitespace (get-token input-port))))


;;; data definitions

;; A small language expression (SmallLangExp) is one of the following.
;; a number n
;; an identifier x
;; a sum with parts e1 and e2, 
;;   where e1 and e2 are small language expressions
;; a difference with parts e1 and e2, 
;;   where e1 and e2 are small language expressions
;; a product with parts e1 and e2,
;;   where e1 and e2 are small language expressions
;; a quotient with parts e1 and e2, 
;;   where e1 and e2 are small language expressions
;; a negation with part e,
;;   where e is an small language expression
;; a bindings with parts defs and e, 
;;   where defs is a list of identifiers * SmallLangExp
;;   and e is an small language expression

;; functions for associated with each part: predicate, constructor, selectors.

;; Number is a Scheme number

;; Identifier is a Scheme symbol

; arg1 SmallLangExp -> SmallLangExp
(define arg1 cadr)
;(= (arg1 '('sum 2 3)) 2)
;(= (arg1 '('sum 4 3)) 4)
;(not (= (arg1 '('sum 4 3)) 2))

; arg2 SmallLangExp -> SmallLangExp
(define arg2 caddr)
;(= (arg2 '('sum 2 3)) 3)
;(= (arg2 '('sum 4 5)) 5)
;(not (= (arg2 '('sum 4 3)) 4))

; make-sum: SmallLangExp * SmallLangExp -> SumExp
(define (make-sum exp1 exp2)
  (list 'sum exp1 exp2))
; (equal? (make-sum 2 3) '(sum 2 3))

; sum? SmallLangExp -> Boolean
(define (sum? exp) (and (pair? exp) (eq? (car exp) 'sum)))
;(sum? (make-sum 2 3))
;(eq? (sum? '(diff 4 3)) #f)

; make-diff: SmallLangExp * SmallLangExp -> DiffExp
(define (make-diff exp1 exp2)
  (list 'diff exp1 exp2))
; (equal? (make-diff 2 3) '(diff 2 3))

; difference? SmallLangExp -> Boolean
(define (difference? exp) (and (pair? exp) (eq? (car exp) 'diff)))
;(difference? (make-diff 3 2))
;(eq? (difference? (make-sum 3 2)) #f)

; make-prod: SmallLangExp * SmallLangExp -> ProdExp
(define (make-prod exp1 exp2)
  (list 'prod exp1 exp2))
; (equal? (make-prod 2 3) '(prod 2 3))

; product? SmallLangExp -> Boolean
(define (product? exp) (and (pair? exp) (eq? (car exp) 'prod)))
;(product? (make-prod 2 3))
;(eq? (product? (make-sum 2 3)) #f)

; make-quo: SmallLangExp * SmallLangExp -> QuoExp
(define (make-quo exp1 exp2)
  (list 'quo exp1 exp2))
; (equal? (make-quo 2 3) '(quo 2 3))

; quotient? SmallLangExp -> Boolean
(define (quotient? exp) (and (pair? exp) (eq? (car exp) 'quo)))
;(quotient? (make-quo 2 3))
;(eq? (quotient? (make-sum 2 3)) #f)

; make-neg: SmallLangExp -> NegExp
(define (make-neg exp)
  (list 'neg exp))
; (equal? (make-neg 2) '(neg 2))

; neg-exp NegExp -> SmallLangExp
(define neg-exp arg1)
; (eq? (neg-exp (make-neg 2)) 2)

; negate? SmallLangExp -> Boolean
(define (negate? exp) (and (pair? exp) (eq? (car exp) 'neg)))
;(negate? (make-neg 2))
;(eq? (negate? (make-prod 4 2)) #f)

; make-let: Listof(Identifier*SmallLangExp) * SmallLangExp -> BindingExp
; Identifier*SmallLangExp is represented as a two element list
(define (make-let defs exp)
  (list 'with-bindings defs exp))
; (equal? (make-let (list (list 'x 1) (list 'y 2)) 3) '(with-bindings ((x 1) (y 2)) 3))

; let-defs SmallLangExp -> Listof(Identifier*SmallLangExp)
(define let-defs arg1)

; let-exp SmallLangExp -> SmallLangExp
(define let-exp arg2)

; let? SmallLangExp -> Boolean
(define (let? exp) (and (pair? exp) (eq? (car exp) 'with-bindings)))
;(let? (make-let (list (list 'x 1) (list 'y 2)) 3))
;(eq? (let? (make-prod 100 100)) #f)

; parse-small-lang: (() -> token) -> SmallLangExp
(define parse-small-lang
  (parser
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (error 'parse-small-lang "error occurred, ~v ~v ~v" a b c)))
   (grammar
    (exp ((LET let-defs IN exp) (make-let $2 $4))
         ((math-exp) $1))
    (let-def ((ID EQ1 exp) (list $1 $3)))
    (let-defs ((let-def) (list $1))
              ((let-def COMMA let-defs) (cons $1 $3)))
    (math-exp ((math-exp + term) (make-sum $1 $3))
              ((math-exp - term) (make-diff $1 $3))
              ((term) $1))
    (term ((term * factor) (make-prod $1 $3))
          ((term / factor) (make-quo $1 $3))
          ((factor) $1))
    (factor ((ID) $1)
            ((NUM) $1)
            ((- factor) (make-neg $2))
            ((OP exp CP) $2)))))


; lexer/parser test
(let* ((example "let x = -2 + 3 * 4, y = 0 in -2+5*x+y")
       (i (open-input-string example))) ; convert string to inputPort
  (equal? (parse-small-lang (lambda () (get-token i)))
          '(with-bindings ((x (sum (neg 2) (prod 3 4)))
                           (y 0))
             (sum (sum (neg 2) (prod 5 x)) y))))

;; Large Language
;;; Data definitions (Large Language)

;; A large language expression (LargeLangExp) is one of the following.
;; a number n
;; an identifier x
;; A program declaration with parts classes and exprs,
;;   where classes is a series of class declarations and exprs is one or more expressions
;; a class declaration with parts name, parent, fielddecls, and methoddecls,
;;   where name is the name of the class
;;   parent is the name of the class the class inherits from
;;   fielddecls is a series of field declarations
;;   methoddecls is a series of method decalarations
;; a field declaration with parts ident,
;;   where ident is the name of the field
;; a method declaration with parts ident, formals and expr, 
;;   where ident is the name, formals are parameter names, and expr is an Expression
;; an expression, which can be one of the following:
;;   A Small Language let def
;;   A series of procedure definitions
;;   Multiple expressions enclosed in {}
;;   An if statement with parts test, a, and b,
;;      Where test is the test, a is the result if true, b is the result if false
;;   A lambda expression
;;   A new expression
;;   A super call
;;   An assignment
;;   a comparison expression
;; A letdef, which is exactly the same as in the small language
;; A procdef which consists of parts a b and c
;;   where a is the name of the proc, b is the parameters of the proc, and c is an expression
;; A comparison expression, which can be either:
;;   an equal expression with parts a and b,
;;     where a and b are both math expressions
;;   a single math expression
;; A math expression which can be one of
;;   a sum with parts a and b where
;;     a is a math expression and b is a term
;;   a different with parts a and b where
;;     a is a math expression and b is a term
;;   a single term
;; A term which can be one of
;;   a product of a and b where
;;     a is a term and b is a factor
;;   a quotient of a and b where
;;     a is a term and b is a factor
;;   a factor
;; A factor which can be one of
;;   a simple
;;   a number
;;   a negation of a factor
;; A simple which can be one of
;;   an ident
;;   an access consisting of parts a and b where
;;     a is a simple, and b is an ident
;;   a proc call with parts and a and b where
;;     a is a simple, and b is a list of actuals
;;   an expression wrapped in parentheses
;; An actual which is just an expression sometimes followed by other actuals
;; A formal which is just a symbol


;; General Purpose acessors
; arg3 LargeLangExp -> LargeLangExp
(define arg3 cadddr)

; arg4 LargeLangExp -> LargeLangExp
(define (arg4 exp) (car (cdr (cdr (cdr (cdr exp))))))


;; Program Code
; make-program ClassDecls Exp -> Program
(define (make-program cd expr) (list 'program cd expr))
; (equal? (make-program '() '()) '(program () ()))

; program? Exp -> Boolean
(define (program? exp) (and (pair? exp) (eq? (car exp) 'program)))
;(eq? (program? (make-program '() '())) #t)
;(eq? (program? '()) #f)

; program-decls Program -> ClassDecls
(define program-decls arg1)
;(equal? (program-decls (make-program '() '())) '())

; program-exp Program -> Exprs
(define program-exp arg2)
;(equal? (program-exp (make-program '() '())) '())


;; Class Def code
; make-class ID ID FieldDecls MethodDecls -> Class
(define (make-class name parent fields methods) (list 'class name parent fields methods))
;(let
;    ([name (get-token (open-input-string "class"))] [parent (get-token (open-input-string "parent"))])
;  (equal? (make-class name parent '() '()) (list 'class name parent '() '())))

; class-decl? Exp -> Boolean
(define (class-decl? exp) (and (pair? exp) (eq? (car exp) 'class)))
;(let
;    ([name (get-token (open-input-string "class"))] [parent (get-token (open-input-string "parent"))])
;  (eq? (class-decl? (make-class name parent '() '())) #t))
;(eq? (class-decl? '()) #f)

; class-name ClassDecl -> ID
(define class-name arg1)
;(let
;    ([name (get-token (open-input-string "class"))] [parent (get-token (open-input-string "parent"))])
;  (eq? (class-name (make-class name parent '() '())) name))

; class-name ClassDecl -> ID
(define class-parent arg2)
;(let
;    ([name (get-token (open-input-string "class"))] [parent (get-token (open-input-string "parent"))])
;  (eq? (class-parent (make-class name parent '() '())) parent))

; class-fields ClassDecl -> Fields
(define class-fields arg3)
;(let
;    ([name (get-token (open-input-string "class"))] [parent (get-token (open-input-string "parent"))])
;  (equal? (class-fields (make-class name parent '() '())) '()))

; class-fields ClassDecl -> Methods
(define class-methods arg4)
;(let
;    ([name (get-token (open-input-string "class"))] [parent (get-token (open-input-string "parent"))])
;  (equal? (class-methods (make-class name parent '() '())) '()))


;; New code
; make-new ID Actuals -> New
(define (make-new name actuals) (list 'new name actuals))
;(let ([name (get-token (open-input-string "class"))])
;  (equal? (make-new name '()) (list 'new name '())))

; new? Exp -> Boolean
(define (new? exp) (and (pair? exp) (eq? (car exp) 'new)))
;(let ([name (get-token (open-input-string "class"))])
;  (eq? (new? (make-new name '())) #t))

; new-name New -> Id
(define new-name arg1)
;(let ([name (get-token (open-input-string "class"))])
;  (eq? (new-name (make-new name '())) name))

; new-rands New -> Actuals
(define new-rands arg2)
;(let ([name (get-token (open-input-string "class"))])
;  (equal? (new-rands (make-new name '())) '()))


;; Method Def code
; make-method Id Formals Expr -> Method
(define (make-method name formals expr) (list 'method name formals expr))
;(let ([name (get-token (open-input-string "method"))])
;  (equal? (make-method name '() '()) (list 'method name '() '())))

; method? Exp -> Boolean
(define (method? exp) (and (pair? exp) (eq? (car exp) 'method)))
;(let ([name (get-token (open-input-string "method"))]) (method? (make-method name '() '())))

; method-name MethodDecl -> Id
(define method-name arg1)
;(let ([name (get-token (open-input-string "method"))])
;  (eq? (method-name (make-method name '() '())) name))

;method-formals MethodDecl -> List[Id]
(define method-formals arg2)
;(let ([name (get-token (open-input-string "method"))])
;  (eq? (method-formals (make-method name '() '())) '()))

; method-exp MethodDecl -> Exprs
(define method-exp arg3)
;(let ([name (get-token (open-input-string "method"))])
;  (eq? (method-exp (make-method name '() '())) '()))


;; Super Code
; make-super Id Actuals -> Super
(define (make-super name actuals) (list 'super name actuals))
;(let ([name (get-token (open-input-string "method"))])
;  (equal? (make-super name '()) (list 'super name '())))

; supercall? Exp -> Boolean
(define (supercall? exp) (and (pair? exp) (eq? (car exp) 'super)))
;(let ([name (get-token (open-input-string "method"))])
;  (supercall? (make-super name '())))

; supercall-name Supercall -> Id
(define supercall-name arg1)
;(let ([name (get-token (open-input-string "method"))])
;  (equal? (supercall-name (make-super name '())) name))

; supercall-rands Supercall -> Actuals
(define supercall-rands arg2)
;(let ([name (get-token (open-input-string "method"))])
;  (equal? (supercall-rands (make-super name '())) '()))


;; Sequence Code
; make-seq Exprs -> Sequence
(define (make-seq exprs) (cons 'sequence exprs))
;(equal? (make-seq '()) '(sequence))
;(equal? (make-seq '(1 2 3)) '(sequence 1 2 3))

; seq? Exp -> Boolean
(define (seq? exp) (and (pair? exp) (eq? (car exp) 'sequence)))
;(seq? (make-seq '()))

; seq-exps Sequence -> Exprs
(define seq-exps arg1)
;(equal? (seq-exps (make-seq '())) '())


;; Procs Code
; make-procs Procs -> Procs
(define (make-procs procs expr) (list 'procedures procs expr))
;(equal? (make-procs '() '()) '(procedures () ()))

; procs? Exp -> Boolean
(define (procs? exp) (and (pair? exp) (eq? (car exp) 'procedures)))
;(procs? (make-procs '() '()))

; procs-defs Procs -> ProcDefs
(define procs-defs arg1)
;(equal? (procs-defs (make-procs '() '())) '())

; procs-defs Procs -> Exprs
(define procs-exp arg2)
;(equal? (procs-exp (make-procs '() '())) '())


;; Proc Code
; make-proc name formals body -> Proc
(define (make-proc name formals body) (list name (list 'proc formals body)))
;(let ([name (get-token (open-input-string "proc"))])
;  (equal? (make-proc name '() '()) (list name (list 'proc '() '()))))

; proc? Exp -> Boolean
(define (proc? exp) (and (pair? exp) (eq? (caadr exp) 'proc)))
; (let ([name (get-token (open-input-string "proc"))]) (proc? (make-proc name '() '())))


;; If Code
; make-if Cond Expr Expr -> If
(define (make-if condition true-exec false-exec) (list 'if condition true-exec false-exec))
;(equal? (make-if '() '() '()) (list 'if '() '() '()))

; if? Exp -> Boolean
(define (if? exp) (and (pair? exp) (eq? (car exp) 'if)))
;(if? (make-if '() '() '()))

; if-exp1 If -> Expr
(define if-exp1 arg1)
;(equal? (if-exp1 (make-if '() '() '())) '())

; if-exp2 If -> Expr
(define if-exp2 arg2)
;(equal? (if-exp2 (make-if '() '() '())) '())

; if-exp3 If -> Expr
(define if-exp3 arg3)
;(equal? (if-exp2 (make-if '() '() '())) '())


;; Assign Code
; make-assign ID value
(define (make-assign id value) (list 'assign! id value))
;(let
;    ([name (get-token (open-input-string "name"))]
;     [value (get-token (open-input-string "42"))])
;  (equal? (make-assign name value) (list 'assign! name value)))

; assign? Exp -> Boolean
(define (assign? exp) (and (pair? exp) (eq? (car exp) 'assign!)))
;(let
;    ([name (get-token (open-input-string "name"))]
;     [value (get-token (open-input-string "42"))])
;  (assign? (make-assign name value)))

; assign-var Assign -> Id
(define assign-var arg1)
;(let
;    ([name (get-token (open-input-string "name"))]
;     [value (get-token (open-input-string "42"))])
;  (eq? (arg1 (make-assign name value)) name))

; assign-exp Assign -> Id
(define assign-exp arg2)
;(let
;    ([name (get-token (open-input-string "name"))]
;     [value (get-token (open-input-string "42"))])
;  (eq? (arg2 (make-assign name value)) value))


;; Equality Code
; make-equality MathExp MathExp -> Equality
(define (make-equality exp1 exp2) (list 'equality? exp1 exp2))
;(equal? (make-equality '() '()) (list 'equality? '() '()))
  
; equality? Exp -> Boolean
(define (equality? exp) (and (pair? exp) (eq? (car exp) 'equality?)))
;(equality? (make-equality '() '()))


;; Access Code
; make-access Simple Id -> Access
(define (make-access simple id) (list 'send simple id))
;(let
;    ([class (get-token (open-input-string "class"))]
;     [property (get-token (open-input-string "method"))])
;  (equal? (make-access class property) (list 'send class property)))

; access? Exp -> Boolean
(define (access? exp) (and (pair? exp) (eq? (car exp) 'send)))
;(let
;    ([class (get-token (open-input-string "class"))]
;     [property (get-token (open-input-string "method"))])
;  (access? (make-access class property)))

; access-exp Access -> Simple
(define access-exp arg1)
;(let
;    ([class (get-token (open-input-string "class"))]
;     [property (get-token (open-input-string "method"))])
;  (eq? (access-exp (make-access class property)) class))

; access-message Access -> Id
(define access-message arg2)
;(let
;    ([class (get-token (open-input-string "class"))]
;     [property (get-token (open-input-string "method"))])
;  (eq? (access-message (make-access class property)) property))


;; Funcall Code
; make-funcall Id Actuals -> Funcall
(define (make-funcall id actuals) (list 'funcall id actuals))
;(let
;    ([name (get-token (open-input-string "function"))])
;  (equal? (make-funcall name '()) (list 'funcall name '())))

; funcall? Exp -> Boolean
(define (funcall? exp) (and (pair? exp) (eq? (car exp) 'funcall)))
;(let
;    ([name (get-token (open-input-string "function"))])
;  (funcall? (make-funcall name '())))

; funcall-rator Funcall -> Id
(define funcall-rator arg1)
;(let
;    ([name (get-token (open-input-string "function"))])
;  (eq? (funcall-rator (make-funcall name '())) name))

; funcall-rands Funcall -> Actuals
(define funcall-rands arg2)
;(let
;    ([name (get-token (open-input-string "function"))])
;  (equal? (funcall-rands (make-funcall name '())) '()))