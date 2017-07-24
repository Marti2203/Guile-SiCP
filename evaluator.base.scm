(define (square x) (* x x))
(define cache '())
(define cache-size 0)
(define true #t)
(define false #f)
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (self-evaluating? exp)
  (cond
    ((boolean? exp) true)
    ((number? exp) true)
    ((string? exp) true)
    (else false)))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))
(define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))
(define (make-begin seq) (cons 'begin seq))
(define (make-define name parameters body) (cons 'define (cons (cons name parameters) body)))
(define (make-let parameters body) (cons 'let (cons parameters body)))
(define (make-procedure-no-define parameters body env) (list 'procedure parameters (scan-out-defines body) env))
(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (make-set variable value) (list 'set! variable value))
(define (make-delay exp) (cons 'delay exp))

(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (definition? exp) (tagged-list? exp 'define))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (if? exp) (tagged-list? exp 'if))
(define (begin? exp) (tagged-list? exp 'begin))
(define (application? exp) (pair? exp))
(define (cond? exp) (tagged-list? exp 'cond))
(define (or? exp) (tagged-list? exp 'or))
(define (and? exp) (tagged-list? exp 'and))
(define (let*? exp) (tagged-list? exp 'let*))
(define (do? exp) (tagged-list exp 'do))
(define (compound-procedure? exp) (tagged-list? exp 'procedure))
(define (unbind? exp) (tagged-list? exp 'unbind))
(define (delay? exp) (tagged-list? exp 'delay))
(define (force? exp) (tagged-list? exp 'force))
(define (cached? exp) (tagged-list? exp 'cache))
(define (values? exp) (tagged-list? exp 'values))
(define (env? exp) (tagged-list? exp 'env))
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (require? exp) (tagged-list? exp 'require))
(define (exit? exp) (tagged-list? exp 'exit))


(define (prime? n) (= n (smallest-divisor n)))
(define (smallest-divisor n) (if(divides? 2 n) 2 (find-divisor n 3)))
(define (find-divisor n test-divisor) (cond
					((> (square test-divisor) n) n) 
					((divides? test-divisor n) test-divisor)
					(else (find-divisor n (+ test-divisor 2)))))
(define (divides? a b) (= (remainder b a) 0))

(define (unless? exp) (tagged-list? exp 'unless))
(define (lambda-lazy? exp) (tagged-list? exp 'lambda-lazy))
(define (list-lazy? exp) (tagged-list? exp 'list))
(define (amb? exp) (tagged-list? exp 'amb))
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (prompt-for-input string) (newline) (newline) (display string) (newline))

(define (announce-output string) (newline) (display string) (newline))

(define (setup-environment)
  (let ((initial-env
	  (extend-environment (primitive-procedure-names)
			      (primitive-procedure-objects)
			      the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (text-of-quotation exp) (cadr exp))

(define (text-of-quotation-special exp env)
  (define (list-create input)
    (if (null? input) ''() (list 'cons (list 'quote (car input)) (list-create (cdr input)))))
  (let ((input (cadr exp)))
    (cond 
      ((null? input) '())
      ((or (number? input) (symbol? input)) input)
      ((list? input) (eval-m-lazy (list-create input) env))
      ((pair? input) (eval-m-lazy (list 'cons (list 'quote (car input)) (list 'quote (cdr input))) env))
      )))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (definition-parameters exp) ; formal parameters
		 (lambda-body exp)))) ; body

(define (definition-parameters exp) (cdadr exp))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond 
    ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else (make-begin seq))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (application-lou? exp) (tagged-list exp 'call))
(define (operator-lou exp) (cadr exp))
(define (operands-lou exp) (cddr exp))

(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-special-proc clasuse) (caddr clause))
(define (predicate-special? clause) (and (= (length clause) 3) (eq? '=> (caddr clause))))
(define (cond-actions clause) (cdr clause))

(define (let? exp) (tagged-list? exp 'let))

(define (let-special? exp) (= 4 (length exp)))
(define (let-special-name exp) (cadr exp))
(define (let-special-pairs exp) (caddr exp))
(define (let-special-parameters exp) (map let-parameter (let-special-pairs exp)))
(define (let-special-expressions exp) (map let-expression (let-special-pairs exp)))
(define (let-special-body exp) (cdddr exp))

(define (let-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-parameter pair) (car pair))
(define (let-expression pair) (cadr pair))
(define (let-parameters exp) (map let-parameter (let-pairs exp)))
(define (let-expressions exp) (map let-expression (let-pairs exp)))


(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))
  
(define (apply-m procedure arguments)
  (cond 
    ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure) (eval-m-sequence
				       (procedure-body procedure)
				       (extend-environment (procedure-parameters procedure) arguments (procedure-environment procedure))))
(else (error "Unknown procedure type -- APPLY" procedure))))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false ; no else clause
    (let ((first (first-clause clauses))
	  (rest (rest-clauses clauses)))
      (if (cond-else-clause? first)
	(if (null? rest)
	  (sequence->exp (cond-actions first))
	  (error "ELSE clause isn’t last -- COND->IF" clauses))
	  (make-if (cond-predicate first)
		   (if (predicate-special? first)
		     (apply-m (cond-special-proc first) (cond-predicate first))
		     (sequence->exp (cond-actions first)))
		   (expand-clauses rest))))))

(define (and->if exp env)
  (define (work elements)
    (if (null? (rest-operands elements)) 
      (first-operand elements)
      (make-if (first-operand elements)
	       (work (rest-operands elements))
	       false)))
(work (order-exps (operands exp) 'right)))

(define (or->if exp env)
  (define (work elements)
    (if (null? (rest-operands elements)) 
      (first-operand elements)
      (make-if (first-operand elements)
	       true
	       (work (rest-operands elements)))))
(work (order-exps (operands exp) 'right)))

(define error-codes 
  (list
    (list "many-arg-err" "Too many arguments supplied")
    (list "few-arg-err" "Too few arguments supplied")
    (list "unbound-err" "Unbound variable")
    (list "unassigned-err" "Unasssigned Variable")
    (list "unbound-var-set-err" "Unbound variable -- set")
    (list "unbound-var-unbind-err" "Unbound variable -- unbind")
    (list "unknown-exp-err" "Unknown expression")
    (list "unknown-proc-err" "Unknown procedure")
    (list "zero-div-err" "Division by zero")
    (list "zero-mod-err" "Modulus by zero")
    (list "not-pair-car-err" "Car: element is not pair")
    (list "not-pair-car-err" "Cdr: element is not pair")
    (list "not-pair-set-car-err" "Set-Car!: element is not pair")
    (list "not-pair-set-cdr-err" "Set-Cdr!: element is not pair")
    (list)
  ))

(define (error? result) (and (string? result) (string-contains result "-err")))

(define (get-error-text code) (cadr (assoc code error-codes)))

(define (try func . args) 
  (let 
    ((result (apply func args))) 
      (if (error? result) (error (get-error-text result)) result)
      ))

(define (extend-environment vars vals base-env) (try extend-environment-base vars vals base-env))

(define (extend-environment-base vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals)) "many-arg-err" "few-arg-err")))

(define (lookup-variable-value var env) (try lookup-variable-value-base var env))

(define (lookup-variable-value-base var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
	((null? vars) (env-loop (enclosing-environment env)))
	((eq? var (car vars)) (let ((value (car vals))) (if (eq? value '*unassigned*) "unassigned-err" value)))
	(else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      "unbound-err"
      (let ((frame (first-frame env)))
	(scan (frame-variables frame)
	      (frame-values frame)))))
  (env-loop env))

(define (has-variable-value? var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
	((null? vars) (env-loop (enclosing-environment env)))
	((eq? var (car vars)) (let ((value (car vals))) (if (eq? value '*unassigned*) false true)))
	(else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment) false
      (let ((frame (first-frame env)))
	(scan (frame-variables frame)
	      (frame-values frame)))))
  (env-loop env))


(define (set-variable-value! var val env) (try set-variable-value!-base var val env))

(define (set-variable-value!-base var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
	((null? vars) (env-loop (enclosing-environment env)))
	((eq? var (car vars)) (set-car! vals val))
	(else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      "unbound-var-set-err"
      (let ((frame (first-frame env)))
	(scan (frame-variables frame)
	      (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond 
	((null? vars)  (add-binding-to-frame! var val frame))
	((eq? var (car vars)) (set-car! vals val))
	(else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define (traverse-env var val env null-action eq-action error-action)
  (define (scan vars vals frame)
    (cond 
      ((null? vars) (null-action var val frame env))
      ((eq? var (car vars)) (eq-action vals val))
      (else (scan (cdr vars) (cdr vals) frame))))
  (if (eq? env the-empty-environment)
    (error-action var)
    (let ((frame (first-frame env)))
      (scan (frame-variables frame)
	    (frame-values frame)
	    frame))))

(define (unbind-variable exp) (cadr exp))

(define (unbind var env) (try unbind-base var env))

(define (unbind-base var env)
    (define (env-loop env)
      (define (scan vars vals)
	(cond 
	  ((null? vars) (env-loop (enclosing-environment env)))
	  ((eq? var (car vars)) 
	   (set! vals (cdr vals))
	   (set! vars (cdr vars)))
	  (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
      "unbound-var-unbind-err"
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
    (env-loop env))


(define (make-frame-pairs variables values) (map (lambda (x y) (cons x y)) variables values))
(define (frame-pairs-variables frame) (map car frame))
(define (frame-pairs-values frame) (map cdr frame))
(define (add-binding-to-frame-pairs! pair frame) (set-car! frame (cons pair frame)))

(define (first-variable frame) (caar frame))
(define (first-value frame) (cadr frame))
(define (variable pair) (car pair))
(define (value pair) (cdr pair))

(define (extend-environment-pairs vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame-pairs vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value-pairs var env)
  (define (env-loop env)
    (define (scan pairs)
      (cond 
	((null? pairs) (env-loop (enclosing-environment env)))
	((eq? var (first-variable pairs)) (cdar pairs))
	(else (scan (cdr pairs)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- LOOKUP!" var)
	(scan (first-frame env))))
  (env-loop env))


(define (set-variable-value-pairs! pair env)
  (define (env-loop env)
    (define (scan pairs)
      (cond 
	((null? pairs) (env-loop (enclosing-environment env)))
	((eq? (variable pair) (first-variable pairs)) (set-car! (car pairs) (value pair)))
	(else (scan (cdr pairs)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
	(scan (first-frame env))))
  (env-loop env))

(define (define-variable-pairs! pair env)
  (let ((frame (first-frame env)))
    (define (scan pairs)
      (cond 
	((null? pairs)  (add-binding-to-frame! (cons var val) frame))
	((eq? (variable pair) (first-variable pairs)) (set-car! (car pairs) (value pair)))
	(else (scan (cdr pairs)))))
    (scan frame)))

(define (traverse-env-pairs env pair null-action eq-action error-action)
  (define (scan pairs frame)
    (cond 
      ((null? vars) (null-action pair frame env))
      ((eq? (variable pair) (first-variable pairs)) (eq-action pairs pair))
      (else (scan (cdr pairs) frame))))
  (if (eq? env the-empty-environment)
    (error-action (variable pair))
    (scan (first-frame env) (first-frame env))))

(define (unbind-pairs var env)
    (define (env-loop env)
      (define (scan pairs)
	(cond 
	  ((null? pairs) (env-loop (enclosing-environment env)))
	  ((eq? var (first-variable pairs)) (set! pairs (cdr pairs)))
	  (else (scan (cdr pairs)))))
      (if (eq? env the-empty-environment)
	(error "Unbound variable -- UNBIND!" var)
	  (scan (first-frame env))))
    (env-loop env))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
   (map (lambda (proc) (list 'primitive (cadr proc)))
	primitive-procedures))

(define (apply-primitive-procedure proc args) (apply-in-underlying-scheme (primitive-implementation proc) args))

(define (apply-in-underlying-scheme proc args) (apply proc args)) 

(define (user-print object)
  (cond 
    ((compound-procedure? object) (if (and (pair? (procedure-body object)) (number? (car (procedure-body object))) (= 12 (car (procedure-body object))))
				    (display (map cadr (cdar (procedure-environment object))))
					       (display (list 'compound-procedure
						 (procedure-parameters object)
						 (procedure-body object)
						 '<procedure-env>))))
    ((thunk? object) (display (thunk-exp object))
    ((compiled-procedure? object) (display ’<compiled-procedure>)))
    (else (display object))))
