;; Author: Arthur Nunes-Harwitt

;; Import the parser and lexer generators.


(require (lib "yacc.ss" "parser-tools")
         (lib "lex.ss" "parser-tools")
         (prefix : (lib "lex-sre.ss" "parser-tools")))

(require (lib "pretty.ss"))

;;; lexer

(define-tokens value-tokens (NUM ID VAR COMP))

(define-empty-tokens op-tokens
  (IF 
   OP 
   CP
   COMMA 
   DOT
   OB
   CB
   BAR
   !
   + 
   - 
   * 
   /
   mod
   EOF
   UNPARSEABLE))

(define-lex-abbrevs
  (CR #\015)
  (LF #\012)
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (letter (:or lower-letter upper-letter))
  (var-start (:or upper-letter "_"))
  (digit (:/ "0" "9"))
  (digits (:+ digit))
  (rest (:or letter "_" digit)))


(define get-token
  (lexer
   ((eof) 'EOF)
   (":-" 'IF)
   ("(" 'OP)
   (")" 'CP)
   ("," 'COMMA)
   ("." 'DOT)
   ("[" 'OB)
   ("]" 'CB)
   ("|" 'BAR)
   ("!" '!)
   ("is" (token-COMP 'is))
   ("<" (token-COMP '<))
   (">" (token-COMP '>))
   ("=<" (token-COMP '=<))
   (">=" (token-COMP '>=))
   ("=" (token-COMP '=))
   ("\\=" (token-COMP '!=))
   ("+" '+)
   ("-" '-)
   ("*" '*)
   ("/" '/)
   ("mod" 'mod)
   ((:: (:? "-") digits) (token-NUM (string->number lexeme)))
   ((:: lower-letter (:* rest)) 
    (token-ID (string->symbol lexeme)))
   ((:: var-start (:* rest)) 
    (token-VAR (string->symbol (string-append "?" lexeme))))
   ((:: "%" (:* (:~ CR LF))) (get-token input-port))
   ((:: "/*" (:* (:~ "*")) (:* (:: (:+ "*") (:~ "*" "/") (:* (:~ "*")))) (:+ "*") "/")
    (get-token input-port))
   (whitespace (get-token input-port))
   (any-char (token-UNPARSEABLE)))) ; use unparseable token to detect lexical errors

;;; constructors

(define (make-variable name) name)

(define (make-clause head body)
  (cons 'if (cons head body)))

(define (make-clause0 head)
  (make-clause head '()))

(define (make-cut) (list '!))

(define (make-in op var exp)
  (list op var exp))

(define (make-p-term id args)
  (cons id args))

(define (make-p-term0 id)
  (make-p-term id '()))

(define *prolog-null* '())

(define (make-prolog-list L)
  (if (null? L) 
      *prolog-null*
      (make-prolog-simple-cons (car L) (make-prolog-list (cdr L)))))
        
(define (one? L)
  (and (pair? L) (null? (cdr L))))

(define (make-prolog-cons e1 e2)
  (let ((prolog-e2 (if (one? e2) (car e2) (make-prolog-list e2)))
        (rev-e1 (reverse e1)))
    (let loop ((result prolog-e2)
               (L rev-e1))
      (if (null? L)
          result
          (loop (make-prolog-simple-cons (car L) result)
                (cdr L))))))

(define (make-prolog-simple-cons e1 e2)
  (list '*cons* e1 e2))

(define (make-sum exp1 exp2)
  (list '+ exp1 exp2))

(define (make-diff exp1 exp2)
  (list '- exp1 exp2))

(define (make-prod exp1 exp2)
  (list '* exp1 exp2))

(define (make-quo exp1 exp2)
  (list '/ exp1 exp2))

(define (make-rem exp1 exp2)
  (list 'mod exp1 exp2))

(define (make-neg exp)
  (list 'neg exp))



;;; parsers
;; crude control over parser errors implemented


(define parse-prolog
  (parser
   (start program)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (prolog-error "Error: Parser could not parse input.")))
   (grammar
    (program ((clause DOT) (list $1))
             ((clause DOT program) (cons $1 $3)))
    (clause ((p-exp) (make-clause0 $1))
            ((p-exp IF p-exps) (make-clause $1 $3)))
    (p-exps ((p-exp) (list $1)) 
             ((p-exp COMMA p-exps) (cons $1 $3)))
    (list ((OB CB) *prolog-null*) 
          ((OB p-exps BAR p-exps CB) (make-prolog-cons $2 $4))
          ((OB p-exps CB) (make-prolog-list $2)))
    (p-exp ((exp) $1)
           ((exp COMP exp) (make-p-term $2 (list $1 $3))))
    (exp ((exp + term) (make-sum $1 $3))
         ((exp - term) (make-diff $1 $3))
         ((term) $1))
    (term ((term * factor) (make-prod $1 $3))
          ((term / factor) (make-quo $1 $3))
          ((term mod factor) (make-rem $1 $3))
          ((factor) $1))
    (factor ((VAR) $1)
            ((NUM) $1)
            ((ID OP p-exps CP) (make-p-term $1 $3))
            ((ID) (make-p-term0 $1))
            ((!) (make-cut))
            ((list) $1)
            ((- factor) (make-neg $2))
            ((OP exp CP) $2)))))

(define parse-query ; there must be a better way than to have to repeat
  (parser
   (start query)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (prolog-error "Error: Parser could not parse input.")))
   (grammar
    (query ((p-exps DOT) $1))
    (p-exps ((p-exp) (list $1)) 
             ((p-exp COMMA p-exps) (cons $1 $3)))
    (list ((OB CB) *prolog-null*) 
          ((OB p-exps BAR p-exps CB) (make-prolog-cons $2 $4))
          ((OB p-exps CB) (make-prolog-list $2)))
    (p-exp ((exp) $1)
           ((exp COMP exp) (make-p-term $2 (list $1 $3))))
    (exp ((exp + term) (make-sum $1 $3))
         ((exp - term) (make-diff $1 $3))
         ((term) $1))
    (term ((term * factor) (make-prod $1 $3))
          ((term / factor) (make-quo $1 $3))
          ((term mod factor) (make-rem $1 $3))
          ((factor) $1))
    (factor ((VAR) $1)
            ((NUM) $1)
            ((ID OP p-exps CP) (make-p-term $1 $3))
            ((ID) (make-p-term0 $1))
            ((!) (make-cut))
            ((list) $1)
            ((- factor) (make-neg $2))
            ((OP exp CP) $2)))))

;;; selectors

(define prolog-car cadr)

(define prolog-cdr caddr)

(define functor-head car)

(define functor-body cdr)

(define arg1 cadr)

(define arg2 caddr)

(define predicate car)

(define clause-head cadr)

(define clause-body cddr)

(define (variable-name var) var)

(define is-var cadr)

(define is-exp caddr)


;;; structure predicates

(define (prolog-list? exp)
  (or (eq? exp *prolog-null*)
      (prolog-cons? exp)))

(define (prolog-cons? exp)
  (and (pair? exp) (eq? (car exp) '*cons*)))

(define (prolog-functor? exp)
  (and (pair? exp) (not (eq? (car exp) '*cons*))))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))

(define (diff? exp)
  (and (pair? exp) (eq? (car exp) '-)))

(define (prod? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define (quo? exp)
  (and (pair? exp) (eq? (car exp) '/)))

(define (rem? exp)
  (and (pair? exp) (eq? (car exp) 'mod)))

(define (neg? exp)
  (and (pair? exp) (eq? (car exp) 'neg)))

(define (variable? v)
  (and (symbol? v) 
       (char=? #\? (string-ref (symbol->string v) 0))))

(define failure? not)

(define (is-goal? goal)
  (eq? (car goal) 'is))

(define (cut-goal? goal)
  (eq? (car goal) '!))

(define (arithmetic-op? op)
  (memq op '(+ - * / mod neg)))

(define (logical-infix-op? op)
  (memq op '(= is \= < > =< >=)))

;;; unification

(define (make-unifier var value) (cons (variable-name var) value))
(define (add-unifier unifier env) (cons unifier env))
(define *init-env* '())

(define (unify-eval exp env) 
    (if (variable? exp)
        (let ((binding (assq (variable-name exp) env)))
          (if binding 
	      (unify-eval (cdr binding) env) 
	      (variable-name exp)))
        exp))


(define (unify exp1 exp2 env) ; no occurs check
    (if (failure? env)
        #f
        (let ((var-value1 (unify-eval exp1 env))
              (var-value2 (unify-eval exp2 env)))
           (cond ((eqv? var-value1 var-value2)
                  env)
                 ((variable? var-value1)
                  (add-unifier (make-unifier var-value1 
                                             var-value2) env))
                 ((variable? var-value2)
                  (add-unifier (make-unifier var-value2 
                                             var-value1) env))
                 ((and (pair? var-value1)
                       (pair? var-value2))
                  (unify (cdr var-value1)
                         (cdr var-value2)
                         (unify (car var-value1) 
                                (car var-value2) env)))
                 (else #f)))))

;;; clause related

(define (get-clauses p-name rules)
  (if (null? rules)
      '()
      (let ((clause (car rules)))
	(if (eq? (predicate (clause-head clause)) p-name)
	    (cons clause (get-clauses p-name (cdr rules)))
	    (get-clauses p-name (cdr rules))))))

(define (rename-vars clause)
  (copy-w/rename-goals (cdr clause) 
		       *init-env*
		       (lambda (goals env2)
			 (make-clause (car goals) (cdr goals)))))

(define (copy-w/rename-goals goals env cont)
  (if (null? goals)
      (cont '() env)
      (copy-w/rename-goal (car goals) 
			  env
			  (lambda (goal env2)
			    (copy-w/rename-goals (cdr goals)
						 env2
						 (lambda (goals2 env3)
						   (cont (cons goal goals2)
							 env3)))))))

(define (copy-w/rename-goal goal env cont)
  (cond ((variable? goal) 
	 (let ((var-value (unify-eval goal env)))
	   (if (eq? goal var-value) ; not in env
	       (let* ((new-name (make-variable (gensym (symbol->string (variable-name goal)))))
		      (new-env (add-unifier (make-unifier goal new-name) env)))
		 (cont new-name new-env))
	       (cont var-value env))))
	((pair? goal)
	 (copy-w/rename-goal (car goal)
			     env
			     (lambda (goal2 env2)
			       (copy-w/rename-goal (cdr goal)
						   env2
						   (lambda (goal3 env3)
						     (cont (cons goal2 goal3)
							   env3))))))
	(else (cont goal env))))



;;; unparsing


(define (prolog-display exp)
  (cond ((variable? exp) 
         (let ((str (symbol->string exp)))
           (display (substring str 1 (string-length str)))))
        ((prolog-list? exp)
         (prolog-display-list exp))
        ((prolog-functor? exp)
         (prolog-display-functor exp))
        (else
         (display exp))))

(define (prolog-display-list exp)
  (cond ((eq? exp *prolog-null*) (display "[]"))
        ((prolog-cons? exp)
         (display "[")
         (prolog-display-list-seq exp)
         (display "]"))
        (else (error 'prolog-display-list "input not a list"))))

(define (prolog-display-list-seq exp)
  (let ((head (prolog-car exp))
        (tail (prolog-cdr exp)))
    (cond ((eq? tail *prolog-null*) 
           (prolog-display head))
          ((prolog-cons? tail) 
           (prolog-display head)
           (display ", ")
           (prolog-display-list-seq tail))
          (else 
           (prolog-display head)
           (display " | ")
           (prolog-display tail)))))
           

(define (prolog-display-functor exp)
  (let ((head (functor-head exp))
        (body (functor-body exp)))
    (cond ((null? body)
           (display head))
          ((logical-infix-op? head) 
           (prolog-display (car body))
           (display " ")
           (display head)
           (display " ")
           (prolog-display (cadr body)))
          ((arithmetic-op? head) 
           (prolog-display-arithmetic exp)) 
          (else 
           (display (functor-head exp))
           (display "(")
           (prolog-display-functor-body (functor-body exp))
           (display ")")))))

(define (prolog-display-functor-body exps) ; non-null list
  (let ((first (car exps))
        (rest (cdr exps)))
    (cond ((null? rest)
           (prolog-display first))
          ((pair? rest)
           (prolog-display first)
           (display ", ")
           (prolog-display-functor-body rest))
          (else (error 'prolog-display-functor-body "non-list tail")))))


(define (prolog-display-arithmetic tree) 

  (define (prolog-display-arithmetic1 tree)
    (if (pair? tree)
      (let ((op (functor-head tree)))
        (cond ((memq op '(+ -))
               (write-char #\()
               (prolog-display-arithmetic tree)
               (write-char #\)))
              (else
               (prolog-display-arithmetic tree))))
      (prolog-display-arithmetic tree)))

  (define (prolog-display-arithmetic2 tree)
    (if (pair? tree)
        (begin
          (write-char #\()
          (prolog-display-arithmetic tree)
          (write-char #\)))
        (prolog-display-arithmetic tree)))
  
  (if (pair? tree)
      (let ((op (functor-head tree)))
           (cond ((memq op '(+ - =))
                  (prolog-display-arithmetic (arg1 tree))
                  (display op)
                  (prolog-display-arithmetic (arg2 tree)))
                 ((eq? op '*)
                  (prolog-display-arithmetic1 (arg1 tree))
                  (display op)
                  (prolog-display-arithmetic1 (arg2 tree)))
                 ((or (eq? op '/) (eq? op 'mod))
                  (prolog-display-arithmetic2 (arg1 tree))
                  (display op)
                  (prolog-display-arithmetic2 (arg2 tree)))
                 ((eq? op 'neg)
                  (display '-)
                  (prolog-display-arithmetic1 (arg1 tree)))
                 (else
                  (prolog-display tree))))
      (prolog-display tree)))
        



(define (display-env vars env)
  (cond ((failure? env) (error 'success "found failure"))
	((or (null? vars)
	     (null? env))
	 (display "yes"))
	(else
	 (for-each (lambda (var)
                     (display " ")
		     (prolog-display (variable-name var)) 
		     (display " = ")
		     (prolog-display (expand-prolog-vars var env)))
		   vars))))

(define (expand-prolog-vars exp env)
  (let ((var-value (unify-eval exp env)))
    (cond ((pair? var-value)
	   (cons (expand-prolog-vars (car var-value) env)
		 (expand-prolog-vars (cdr var-value) env)))
	  (else var-value))))
	   



;;; continuations

(define (make-init-ks vars) 
  (lambda (env kf)
    (display-env vars env)
    (let ((answer (read-line))) 
      (if (and (> (string-length answer) 0)
               (char=? (string-ref answer 0) #\;))
	  (kf)))))




(define *init-kf* 
  (lambda ()
    (display "no.")
    (newline)))

;;; core PROLOG interpreter

(define (prove rules goal env ks kf kcut)
  (let ((op0 (arity0-prim-goal? goal)))
    (if op0 
        ((cdr op0) rules env ks kf kcut)
        (let ((op1 (arity1-prim-goal? goal)))
          (if op1 
              ((cdr op1) (arg1 goal) rules env ks kf kcut)
              (let ((op2 (arity2-prim-goal? goal)))
                (if op2
                    ((cdr op2) (arg1 goal) (arg2 goal) rules env ks kf kcut)
                    (prove-goal (get-clauses (predicate goal) rules)
                                rules
                                goal
                                env                                          
                                ks
                                kf
                                (lambda (env2 ks2) (ks2 env2 kf))))))))))

(define (prove-goal clauses rules goal env ks kf kcut)
  (if (null? clauses)
      (kf)
      (let ((clause (rename-vars (car clauses))))
          (prove-all (clause-body clause)
                     rules
                     (unify goal (clause-head clause) env)
                     ks
                     (lambda ()
                       (prove-goal (cdr clauses)
                                   rules
                                   goal
                                   env                                   
                                   ks
                                   kf
                                   kcut))
                     kcut))))

(define (prove-all goals rules env ks kf kcut)
  (cond ((failure? env) (kf))
        ((null? goals) (ks env kf))
        (else
         (prove rules
                (car goals) 
                env 
                (lambda (env2 kf2)
                  (prove-all (cdr goals)
                             rules
                             env2                             
                             ks
                             kf2
                             kcut))
                kf
                kcut))))
                     




(define *eval-table*
  (list (cons '*number* (lambda (exp k-eval env kf) (k-eval exp)))
        
        (cons '*variable* (lambda (exp k-eval env kf)
                            (let ((value (unify-eval exp env)))
                              (if (number? value)
                                  (k-eval value)
                                  (kf)))))
        
        (cons '+ (lambda (exp k-eval env kf)
                   (prolog-eval (arg1 exp) 
                                (lambda (v) 
                                  (prolog-eval (arg2 exp) 
                                               (lambda (w) 
                                                 (k-eval (+ v w))) 
                                               env 
                                               kf)) 
                                env 
                                kf)))
        
        (cons '- (lambda (exp k-eval env kf)
                   (prolog-eval (arg1 exp) 
                                (lambda (v) 
                                  (prolog-eval (arg2 exp) 
                                               (lambda (w) 
                                                 (k-eval (- v w))) 
                                               env 
                                               kf)) 
                                env 
                                kf)))
        
        (cons '* (lambda (exp k-eval env kf)
                   (prolog-eval (arg1 exp) 
                                (lambda (v) 
                                  (prolog-eval (arg2 exp) 
                                               (lambda (w) 
                                                 (k-eval (* v w))) 
                                               env kf)) 
                                env 
                                kf)))
        
        (cons '/ (lambda (exp k-eval env kf)
                   (prolog-eval (arg1 exp) 
                                (lambda (v) 
                                  (prolog-eval (arg2 exp) 
                                               (lambda (w) 
                                                 (if (= w 0)
                                                     (kf)
                                                     (k-eval (quotient v w))))
                                               env 
                                               kf))
                                env 
                                kf)))
        
        (cons 'mod (lambda (exp k-eval env kf)
                   (prolog-eval (arg1 exp) 
                                (lambda (v) 
                                  (prolog-eval (arg2 exp) 
                                               (lambda (w) 
                                                 (if (= w 0)
                                                     (kf)
                                                     (k-eval (remainder v w))))
                                               env 
                                               kf))
                                env 
                                kf)))
        
        (cons 'neg (lambda (exp k-eval env kf)
                     (prolog-eval (arg1 exp) 
                                  (lambda (w) (k-eval (- w))) 
                                  env 
                                  kf)))  ))

(define (get-eval-type exp)
  (cond ((number? exp) '*number*)
        ((variable? exp) '*variable*)
        ((prolog-functor? exp) (predicate exp))
        (else #f)))

(define (prolog-eval exp k-eval env kf)
  (let* ((tag (get-eval-type exp))
         (op (assq tag *eval-table*)))
    (if op
        ((cdr op) exp k-eval env kf)
        (kf))))

(define *arity0-prims* 
  (list (cons '! (lambda (rules env ks kf kcut) (kcut env ks)))
        
        (cons 'fail (lambda (rules env ks kf kcut) (kf)))
        
        (cons 'true (lambda (rules env ks kf kcut) (ks env kf)))
        
        (cons 'halt (lambda (rules env ks kf kcut) 
                      (*prolog-halt-cont* 'DONE))) 

        (cons 'nl (lambda (rules env ks kf kcut) 
                    (newline)
                    (ks env kf))) ))

(define (arity0-prim-goal? goal)
  (and (null? (cdr goal))
       (assq (predicate goal) *arity0-prims*)))

(define *arity1-prims* 
  (list (cons 'number (lambda (exp rules env ks kf kcut)
                        (let ((v (unify-eval exp env)))
                          (if (number? v)
                              (ks env kf)
                              (kf)))))
        
        (cons 'var (lambda (exp rules env ks kf kcut)
                     (let ((v (unify-eval exp env)))
                       (if (variable? v)
                           (ks env kf)
                           (kf)))))
        
        (cons 'atom (lambda (exp rules env ks kf kcut)
                      (let ((v (unify-eval exp env)))
                        (if (and (prolog-functor? v)
                                 (null? (functor-body v)))
                            (ks env kf)
                            (kf)))))
        
        (cons 'atomic (lambda (exp rules env ks kf kcut)
                        (let ((v (unify-eval exp env)))
                          (if (or (number? v)
                                  (and (prolog-functor? v)
                                       (null? (functor-body v))))
                              (ks env kf)
                              (kf))))) 
        
        (cons 'not (lambda (exp rules env ks kf kcut)
                     (prove rules 
                            exp 
                            env 
                            (lambda (env2 kf2) (kf)) 
                            (lambda () (ks env kf)) 
                            kcut))) 
        (cons 'write (lambda (exp rules env ks kf kcut)
                       (prolog-display (expand-prolog-vars exp env))
                       (ks env kf))) ))

(define (arity1-prim-goal? goal)
  (and (not (null? (cdr goal)))
       (null? (cdr (cdr goal)))
       (assq (predicate goal) *arity1-prims*)))

(define *arity2-prims* 
  (list (cons '= (lambda (exp1 exp2 rules env ks kf kcut)
                   (let ((v1 (unify-eval exp1 env))
                         (v2 (unify-eval exp2 env)))
                     (let ((env2 (unify v1 v2 env)))
                       (if (failure? env2)
                           (kf)
                           (ks env2 kf))))))
        
        (cons '!= (lambda (exp1 exp2 rules env ks kf kcut)
                    (let ((v1 (unify-eval exp1 env))
                          (v2 (unify-eval exp2 env)))
                      (let ((env2 (unify v1 v2 env)))
                        (if (failure? env2)
                            (ks env kf)
                            (kf))))))
        
        (cons 'is (lambda (var exp rules env ks kf kcut)
                    (prolog-eval exp 
                                 (lambda (v)
                                   (let ((env2 (unify var v env)))
                                     (if (failure? env2)
                                         (kf)
                                         (ks env2 kf))))
                                 env 
                                 kf)))
        
        (cons '< (lambda (exp1 exp2 rules env ks kf kcut)
                   (let ((v1 (unify-eval exp1 env))
                         (v2 (unify-eval exp2 env)))
                     (if (and (number? v1)
                              (number? v2)
                              (< v1 v2))
                         (ks env kf)
                         (kf)))))
        
        (cons '> (lambda (exp1 exp2 rules env ks kf kcut)
                   (let ((v1 (unify-eval exp1 env))
                         (v2 (unify-eval exp2 env)))
                     (if (and (number? v1)
                              (number? v2)
                              (> v1 v2))
                         (ks env kf)
                         (kf)))))
        
        (cons '=< (lambda (exp1 exp2 rules env ks kf kcut)
                    (let ((v1 (unify-eval exp1 env))
                          (v2 (unify-eval exp2 env)))
                      (if (and (number? v1)
                               (number? v2)
                               (<= v1 v2))
                          (ks env kf)
                          (kf)))))
        
        (cons '>= (lambda (exp1 exp2 rules env ks kf kcut)
                    (let ((v1 (unify-eval exp1 env))
                          (v2 (unify-eval exp2 env)))
                      (if (and (number? v1)
                               (number? v2)
                               (>= v1 v2))
                          (ks env kf)
                          (kf))))) ))

(define (arity2-prim-goal? goal)
  (and (not (null? (cdr goal)))
       (not (null? (cdr (cdr goal))))
       (null? (cdr (cdr (cdr goal))))
       (assq (predicate goal) *arity2-prims*)))



;;; top level query

(define (find-vars-acc goals a) ;treat goals as a tree-structured list
  (cond ((variable? goals)
         (if (memq goals a)
             a
             (cons goals a)))
        ((pair? goals)
         (find-vars-acc (cdr goals) (find-vars-acc (car goals) a)))
        (else a)))
         

(define (find-vars goals) ;treat goals as a tree-structured list
  (reverse (find-vars-acc goals '())))

(define (prove-query goals rules)
  (let* ((vars (find-vars goals))
         (ks (make-init-ks vars))
         (kcut (lambda (env ks) (ks env *init-kf*))))
    (prove-all goals rules *init-env* ks *init-kf* kcut)))

(define *prolog-reset-cont* #f)
(define *prolog-halt-cont* #f)

(call-with-current-continuation
 (lambda (k)
   (set! *prolog-reset-cont* k)
   (set! *prolog-halt-cont* k)))
   

(define (prolog-reset) (*prolog-reset-cont* 'Done))

(define (prolog-error message)
  (display message)
  (newline)
  (prolog-reset))

(define (run-prolog)
  
  (define (rep rules)
    (display "?- ")
    (let* ((i (open-input-string (read-line)))
           (query (parse-query (lambda () (get-token i)))))
      (prove-query query rules)
      (rep rules)))
  
  (display "Starting PROLOG")
  (newline)
  
      
  (let file-loop ((file (get-file "Consult which PROLOG file?" 
                                  #f 
                                  #f 
                                  #f 
                                  #f 
                                  '(packages enter-packages) 
                                  '(("Any" "*.*") ("PROLOG" "*.pl")))))
    (if file
        (let* ((i (open-input-file file))
               (rules (parse-prolog (lambda () (get-token i)))))
          (close-input-port i)
          (call-with-current-continuation
           (lambda (k)
             (set! *prolog-reset-cont* k)))
          (rep rules))
        (begin
          (display "Warning: No file consulted.")
          (newline)
          (call-with-current-continuation
           (lambda (k)
             (set! *prolog-reset-cont* k)))
          (rep '())))))



(run-prolog)