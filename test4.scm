(define-syntax cons-stream
  (syntax-rules ()
		((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-null? a) (null? a))
(define the-empty-stream '())

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
		 (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin 	
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))
(define (display-stream s) (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond 
    ((stream-null? stream) the-empty-stream)
    ((pred (stream-car stream))
     (cons-stream (stream-car stream)
		  (stream-filter pred (stream-cdr stream))))
    (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map-l proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map-l (cons proc (map stream-cdr argstreams))))))

(define (stream-append stream-1 stream-2)
  (if (stream-null? stream-1)
    stream-2
    (cons-stream (stream-car stream-1) (stream-append (stream-cdr stream-1) stream-2))))

(define (square x) (* x x))
(define cache '())
(define cache-size 0)
(define true #t)
(define false #f)
(define (tagged-list? exp-m tag)
  (if (pair? exp-m)
    (eq? (car exp-m) tag)
    false))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (self-evaluating? exp-m)
  (cond
    ((boolean? exp-m) true)
    ((number? exp-m) true)
    ((string? exp-m) true)
    (else false)))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))
(define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))
(define (make-begin seq) (cons 'begin seq))
(define (make-define name parameters body) (cons 'define (cons (cons name parameters) body)))
(define (make-let parameters body) (cons 'let (cons parameters body)))
(define (make-procedure-no-define parameters body env) (list 'procedure parameters (scan-out-defines body) env))
(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (make-set variable value) (list 'set! variable value))
(define (make-delay exp-m) (cons 'delay exp-m))

(define (variable? exp-m) (symbol? exp-m))
(define (quoted? exp-m) (tagged-list? exp-m 'quote))
(define (assignment? exp-m) (tagged-list? exp-m 'set!))
(define (definition? exp-m) (tagged-list? exp-m 'define))
(define (lambda? exp-m) (tagged-list? exp-m 'lambda))
(define (if? exp-m) (tagged-list? exp-m 'if))
(define (begin? exp-m) (tagged-list? exp-m 'begin))
(define (application? exp-m) (pair? exp-m))
(define (cond? exp-m) (tagged-list? exp-m 'cond))
(define (or? exp-m) (tagged-list? exp-m 'or))
(define (and? exp-m) (tagged-list? exp-m 'and))
(define (let*? exp-m) (tagged-list? exp-m 'let*))
(define (do? exp-m) (tagged-list exp-m 'do))
(define (compound-procedure? exp-m) (tagged-list? exp-m 'procedure))
(define (unbind? exp-m) (tagged-list? exp-m 'unbind))
(define (delay? exp-m) (tagged-list? exp-m 'delay))
(define (force? exp-m) (tagged-list? exp-m 'force))
(define (cached? exp-m) (tagged-list? exp-m 'cache))
(define (values? exp-m) (tagged-list? exp-m 'values))
(define (env? exp-m) (tagged-list? exp-m 'env))
(define (letrec? exp-m) (tagged-list? exp-m 'letrec))
(define (require? exp-m) (tagged-list? exp-m 'require))




(define (prime? n) (= n (smallest-divisor n)))
(define (smallest-divisor n) (if(divides? 2 n) 2 (find-divisor n 3)))
(define (find-divisor n test-divisor) (cond
					((> (square test-divisor) n) n) 
					((divides? test-divisor n) test-divisor)
					(else (find-divisor n (+ test-divisor 2)))))
(define (divides? a b) (= (remainder b a) 0))

(define (unless? exp-m) (tagged-list? exp-m 'unless))
(define (lambda-lazy? exp-m) (tagged-list? exp-m 'lambda-lazy))
(define (list-lazy? exp-m) (tagged-list? exp-m 'list))
(define (amb? exp-m) (tagged-list? exp-m 'amb))
(define (ramb? exp-m) (tagged-list? exp-m 'ramb))
(define (permanent-assignment? exp-m) (tagged-list? exp-m 'permanent-set!))
(define (if-fail? exp-m) (tagged-list? exp-m 'if-fail))

(define (eval-m exp-m env)
  (cond 
    ((self-evaluating? exp-m) exp-m)
    ((variable? exp-m) (lookup-variable-value exp-m env))
    ((quoted? exp-m) (text-of-quotation exp-m))
    ((cached? exp-m) (list-ref cache (cache-index exp-m))) 
    ((assignment? exp-m) (eval-m-assignment exp-m env))
    ((definition? exp-m) (eval-m-definition exp-m env))
    ((values? exp-m) (list-of-values (exp-values exp-m) env))
    ((if? exp-m) (eval-m-if exp-m env))
    ((and? exp-m) (eval-m-and exp-m env))
    ((or? exp-m) (eval-m-or exp-m env))
    ((let? exp-m) (eval-m (let->combination exp-m) env))
    ((lambda? exp-m)
     (make-procedure (lambda-parameters exp-m)
		     (lambda-body exp-m)
		     env))
    ((begin? exp-m) (eval-m-sequence (begin-actions exp-m) env))
    ((cond? exp-m) (eval-m (cond->if exp-m) env))
    ((unbind? exp-m) (unbind (unbind-variable exp-m) env))
    ((let*? exp-m) (eval-m (let*->nested exp-m) env))
    ((letrec? exp-m)(eval-m (letrec->let exp-m) env))
    ((delay? exp-m) (make-delay (delay-expression exp-m)))
    ((force? exp-m) (eval-m (delay-expression exp-m) env))
    ((env? exp-m) env)
    ((unless? exp-m) (eval-m (unless->if exp-m) env))
    ((application? exp-m)
     (apply-m (eval-m (operator exp-m) env)
	    (list-of-values (operands exp-m) env)))
    (else (error "Unknown exp-mression type -- EVAL" exp-m))))	

(define (eval-m-lou exp-m env)
  (cond 
    ((self-evaluating? exp-m) exp-m)
    ((variable? exp-m) (lookup-variable-value exp-m env))
    ((quoted? exp-m) (text-of-quotation exp-m))
    ((application-lou? exp-m)
     (apply-m (eval-m (operator-lou exp-m) env)
	    (list-of-values (operands-lou exp-m) env)))
    ((assignment? exp-m) (eval-m-assignment exp-m env))
    ((definition? exp-m) (eval-m-definition exp-m env))
    ((if? exp-m) (eval-m-if exp-m env))
    ((lambda? exp-m)
     (make-procedure (lambda-parameters exp-m)
		     (lambda-body exp-m)
		     env))
    ((begin? exp-m) (eval-m-sequence (begin-actions exp-m) env))
    ((cond? exp-m) (eval-m (cond->if exp-m) env))
    (else (error "Unknown exp-mression type -- EVAL" exp-m))))

(define (apply-m procedure arguments)
  (cond 
    ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure) (eval-m-sequence
				       (procedure-body procedure)
				       (extend-environment (procedure-parameters procedure) arguments (procedure-environment procedure))))
    (else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exp-ms env)
(if (no-operands? exp-ms)
  '()
  (cons (eval-m (first-operand exp-ms) env)
	(list-of-values (rest-operands exp-ms) env))))

(define (eval-m-if exp-m env)
  (if 
    (true? (eval-m (if-predicate exp-m) env))
    (eval-m (if-consequent exp-m) env)
    (eval-m (if-alternative exp-m) env)))

(define (eval-m-sequence exp-ms env)
  (cond 
    ((last-exp-m? exp-ms) (eval-m (first-exp-m exp-ms) env))
    (else (eval-m (first-exp-m exp-ms) env)
	  (eval-m-sequence (rest-exp-ms exp-ms) env))))

(define (eval-m-assignment exp-m env)
  (set-variable-value! (assignment-variable exp-m)
		       (eval-m (assignment-value exp-m) env)
		       env)
  'ok)


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	  (let ((record (assoc key-2 (cdr subtable))))
	    (if record (cdr record) false))
	  false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	  (let ((record (assoc key-2 (cdr subtable))))
	    (if record
	      (set-cdr! record value)
	      (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
	  (set-cdr! local-table
		    (cons (list 
			    key-1
				(cons key-2 value))
			  (cdr local-table))))))
    (define (dispatch m)
      (cond 
	((eq? m 'lookup-proc) lookup)
	((eq? m 'insert-proc!) insert!)
	(else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (eval-m-definition exp-m env)
  (define-variable! (definition-variable exp-m)
		    (eval-m (definition-value exp-m) env)
		    env)
  'ok)

(define (get-type exp-m) (car exp-m)) 
(define (eval-m-table exp-m env) ((get 'eval-m (get-type exp-m)) exp-m env))

(define (eval-m-order)
  (define (finder)
  (define state 2)
  (define (work i)
  (if  (= state 2)
   (cond
     ((= i 0) (set! state 0)) 
     ((= i 1) (set! state 1)) 
     (else 2))
   state))
 work)
  (let ((state (finder))) (if (= (+ (state 0) (state 1)) 0) 'left 'right)))


(define (list-of-values-special exp-ms env order)
  (define (work current)
    (if (no-operands? current)
      '()
      (cons (eval-m (first-operand current) env)
	    (work (rest-operands exp-ms) env))))
  (if (eq? order (eval-m-order))
    (work exp-ms)
    (work (reverse exp-ms))))

(define (order-exp-ms exp-ms order)
    (if (eq? order (eval-m-order))
    exp-ms
    (reverse exp-ms)))

(define (text-of-quotation exp-m) (cadr exp-m))

(define (text-of-quotation-special exp-m env)
  (define (list-create input)
    (if (null? input) ''() (list 'cons (list 'quote (car input)) (list-create (cdr input)))))
  (let ((input (cadr exp-m)))
    (cond 
      ((null? input) '())
      ((or (number? input) (symbol? input)) input)
      ((list? input) (eval-m-lazy (list-create input) env))
      ((pair? input) (eval-m-lazy (list 'cons (list 'quote (car input)) (list 'quote (cdr input))) env))
      )))

(define (assignment-variable exp-m) (cadr exp-m))
(define (assignment-value exp-m) (caddr exp-m))

(define (definition-variable exp-m)
  (if (symbol? (cadr exp-m))
    (cadr exp-m)
    (caadr exp-m)))

(define (definition-value exp-m)
  (if (symbol? (cadr exp-m))
    (caddr exp-m)
    (make-lambda (cdadr exp-m) ; formal parameters
		 (cddr exp-m)))) ; body

(define (definition-parameters exp-m) (cdadr exp-m))

(define (lambda-parameters exp-m) (cadr exp-m))
(define (lambda-body exp-m) (cddr exp-m))

(define (if-predicate exp-m) (cadr exp-m))
(define (if-consequent exp-m) (caddr exp-m))
(define (if-alternative exp-m)
  (if (not (null? (cdddr exp-m)))
    (cadddr exp-m)
    'false))

(define (begin-actions exp-m) (cdr exp-m))
(define (last-exp-m? seq) (null? (cdr seq)))
(define (first-exp-m seq) (car seq))
(define (rest-exp-ms seq) (cdr seq))

(define (sequence->exp-m seq)
  (cond 
    ((null? seq) seq)
    ((last-exp-m? seq) (first-exp-m seq))
    (else (make-begin seq))))

(define (operator exp-m) (car exp-m))
(define (operands exp-m) (cdr exp-m))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (application-lou? exp-m) (tagged-list exp-m 'call))
(define (operator-lou exp-m) (cadr exp-m))
(define (operands-lou exp-m) (cddr exp-m))


(define (cond-clauses exp-m) (cdr exp-m))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-special-proc clasuse) (caddr clause))
(define (predicate-special? clause) (and (= (length clause) 3) (eq? '=> (caddr clause))))
(define (cond-actions clause) (cdr clause))

(define cond-test '(cond ((= x 5) 4) ((= x 6) 5) ) )
(define (cond->if exp-m) (exp-mand-clauses (cond-clauses exp-m)))
(define (exp-mand-clauses clauses)
  (if (null? clauses)
    'false ; no else clause
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (if (cond-else-clause? first)
	(if (null? rest)
	  (sequence->exp-m (cond-actions first))
	  (error "ELSE clause isn’t last -- COND->IF" clauses))
	  (make-if (cond-predicate first)
		   (if (predicate-special? first)
		     (apply-m (cond-special-proc first) (cond-predicate first))
		     (sequence->exp-m (cond-actions first)))
		   (exp-mand-clauses rest))))))



(define (and->if exp-m env)
  (define (work elements)
    (if (null? (rest-operands elements)) 
      (first-operand elements)
      (make-if (first-operand elements)
	       (work (rest-operands elements))
	       false)))
(work (order-exp-ms (operands exp-m) 'right)))

(define (or->if exp-m env)
  (define (work elements)
    (if (null? (rest-operands elements)) 
      (first-operand elements)
      (make-if (first-operand elements)
	       true
	       (work (rest-operands elements)))))
(work (order-exp-ms (operands exp-m) 'right)))

(define (eval-m-and exp-m env)
  (define (work elements)
  (cond
    ((null? elements) true)
    ((false? (eval-m (first-operand elements) env) false)
    (else (work (rest-operands elements))))))
 (work (order-exp-ms (operands exp-m) 'right)))

(define (eval-m-or exp-m env)
  (define (work elements)
  (cond
    ((null? elements) false)
    ((true? (eval-m (first-operand element) env) true)
    (else (work (rest-operands elements))))))
  (work (order-exp-ms (operands exp-m) 'right)))


(define (let? exp-m) (tagged-list? exp-m 'let))

(define (let-special? exp-m) (= 4 (length exp-m)))
(define (let-special-name exp-m) (cadr exp-m))
(define (let-special-pairs exp-m) (caddr exp-m))
(define (let-special-parameters exp-m) (map let-parameter (let-special-pairs exp-m)))
(define (let-special-expressions exp-m) (map let-expression (let-special-pairs exp-m)))
(define (let-special-body exp-m) (cdddr exp-m))

(define (let-pairs exp-m) (cadr exp-m))
(define (let-body exp-m) (cddr exp-m))
(define (let-parameter pair) (car pair))
(define (let-expression pair) (cadr pair))
(define (let-parameters exp-m) (map let-parameter (let-pairs exp-m)))
(define (let-expressions exp-m) (map let-expression (let-pairs exp-m)))
(define (let->combination exp-m) ;(if (let-special? exp-m)
				;	(let ((parameters (let-special-parameters exp-m)) (body (let-special-body exp-m)))	
				;	  (display exp-m)
				;	  (make-begin (list
				;		       (make-define (let-special-name exp-m) parameters body)
				;		       (cons (let-special-name exp-m) (let-special-expressions exp-m)))))
					  (cons
					    (make-lambda (let-parameters exp-m) (let-body exp-m))
					    (let-expressions exp-m)));)

(define let*-test '(let* ((x (* 2 3)) (y (* 3 x))) (* x y)))
(define let-test '(let ((x (* 2 3)) (y (* 3 6))) (* x y)))
(define let-special-test '(define (fib n) 
			    (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) )


(define (let*->nested-lets exp-m)
  (define (work pairs body)
  (if (null? pairs)
     body
    (work (cdr pairs) (make-let (car pairs) body))))
  (work (order-exp-ms (let-pairs exp-m) 'right) (let-body exp-m)))

(define (unless->if exp-m) (make-if (cons ('not (if-predicate exp-m))) (if-alternative exp-m) (if-consequent exp-m)))

(define (do-init-triplets exp-m) (cadr exp-m))
(define (do-init-vars exp-m) (map car (do-init-triplets exp-m)))
(define (do-init-starters exp-m) (map cadr (do-init-triplets exp-m)))
(define (do-init-steps exp-m) (map caddr (do-init-triplets exp-m)))
(define (do-test exp-m) (cadddr exp-m))
(define (do-test-condition exp-m) (car (do-test exp-m)))
(define (do-test-end exp-m) (cdr (do-test exp-m)))
(define (do-tests exp-m) (caddr exp-m))
(define (do-commands exp-m) (cadddr exp-m))

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


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
	((null? vars) (env-loop (enclosing-environment env)))
	((eq? var (car vars)) (let ((value (car vals))) (if (eq? value '*unassigned*) 
							  (error "UNASSIGNED VARIABLE" var)
							  value)))
	(else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
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


(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond 
	((null? vars) (env-loop (enclosing-environment env)))
	((eq? var (car vars)) (set-car! vals val))
	(else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
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

(define (unbind-variable exp-m) (cadr exp-m))
(define (unbind var env)
    (define (env-loop env)
      (define (scan vars vals)
	(cond 
	  ((null? vars) (env-loop (enclosing-environment env)))
	  ((eq? var (car vars)) 
	   (set! vals (cdr vals))
	   (set! vars (cdr vars)))
	  (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
	(error "Unbound variable -- UNBIND!" var)
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

(define (pow x y)
  (define (mul current result)
    (if (= 0 current) 
      result
      (mul (- current 1) (* result x))))
  (mul y 1))

(define (import file-name evaluator)
  (define port (open-input-file file-name))
  (define (work number) 
   (let ((line (read port)))
     (if (eof-object? line)
          'done
          (begin (evaluator line the-global-environment) ;(display number) (display "\t") (display line) (newline)  
		 (work (+ 1 number))))))
  (work 0) 
  (close-input-port port))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval-m input the-global-environment)))
      (announce-output output-prompt)
      (add-to-cache! output display-cache-entrance)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (display-cache-entrance) ;(display cache) 
  (newline) (display "$") (display cache-size) (display " = "))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond 
    ((compound-procedure? object) (if (and (pair? (procedure-body object)) (number? (car (procedure-body object))) (= 12 (car (procedure-body object))))
				    (display (map cadr (cdar (procedure-environment object))))
					       (display (list 'compound-procedure
						 (procedure-parameters object)
						 (procedure-body object)
						 '<procedure-env>))))
    ((thunk? object) (display (thunk-exp object)))
    (else (display object))))


(define primitive-procedures
  (list 
    (list '- -)			(list 'tagged-list? tagged-list?)
    (list '* *)			(list 'string-length string-length)
    (list '+ +)		    	(list 'string->symbol string->symbol)
    (list '< <)			(list '> >)
    (list '/ /) 	    	(list 'open-input-file open-input-file)
    (list '>= >=)		(list '<= <=)
    (list 'cdr cdr)		(list 'symbol->string symbol->string)	
    (list 'car car) 		(list 'close-input-port close-input-port) 
    (list 'cons cons)		
    (list 'null? null?)
    (list 'modulo modulo)
    (list 'reverse reverse) 	(list 'eof-object? eof-object?)
    (list 'pow pow) 		(list 'list list)
    (list 'exit exit) 		(list 'not not)
    (list '= =)			(list 'read read)
    (list 'import import)	(list 'string? string?)
    (list 'symbol? symbol?)	(list 'number? number?)
    (list 'error error)		(list 'display display)
    (list 'newline newline)	(list 'append append)
    (list 'list-ref list-ref)	(list 'cadr cadr)
    (list 'caadr caadr)		(list 'cddr cddr)
    (list 'filter filter)	(list 'caddr caddr)
    (list 'set-car! set-car!)	(list 'set-cdr! set-cdr!)
    (list 'length length)	(list 'eq? eq?)
    (list 'even? even?)		(list 'odd? odd?)
    (list 'prime? prime?)	(list 'member member)
    (list 'abs abs)		(list 'memq memq)
    (list 'assoc assoc)		(list 'pair? pair?)
    (list 'string=? string=?) 	(list 'substring substring)
    (list 'eval eval-m)		(list 'user-inital-environment '())
    (list 'apply apply-m)	(list 'prompt-for-input prompt-for-input)    				
    (list 'equal? equal?)	(list 'string-append string-append)
    ))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
   (map (lambda (proc) (list 'primitive (cadr proc)))
	primitive-procedures))

(define (setup-environment)
  (let ((initial-env
	  (extend-environment (primitive-procedure-names)
			      (primitive-procedure-objects)
			      the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define (apply-in-underlying-scheme proc args) (apply proc args))



(define (halts? p a)
  (if (p a) #t #f))

(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p)
    (run-forever)
    'halted))

(define (scan-out-defines body)
  (define (get-pure-body current-elements)
    (cond 
      ((null? current-elements) error "NO BODY")
      ((definition? (car current-elements)) (get-pure-body (cdr current-elements)))
      (else current-elements)))
  (define pure-body (get-pure-body body))
  (let* ((defines (map cdr (filter definition? body)))
	 (pairs (map (lambda (pair) (list (let-parameter pair) '*unassigned*)) defines))
	 (sets (map (lambda (pair) (make-set (let-parameter pair) (let-expression pair))) defines)))
    (display defines) (newline)
  (make-let pairs (append sets pure-body))))

(define (contains? l e)
  (cond 
    ((null? l) #f)
    ((eq? (car l) e) #t)
    (else (contains? (cdr l) e))))

(define (set? l)
  (define set '())
  (cond 
    ((null? l) true)
    ((contains? set (car l)) false) 
    (else (set! set (cons (car l) set)) (set? (cdr l)))))

(define (has? pred l) (= (length (filter pred l)) 0))
(define (split pred l)
  (define (work current qualified non)
  (cond
    ((null? current) (cons qualified non))
    ((pred (car current)) (work (cdr current) (cons (car current) qualified)) non)
    (else (work (cdr current) qualified (cons (car current) non)))))
  (work l '() '()))

(define (contains-recursive? l e)
  (cond
    ((null? l) #f)
    ((list? (car l)) (or (contains-recursive? (car l) e) (contains-recursive? (cdr l) e)))
    ((eq? (car l)) #t)
    (else (contains-recursive? (cdr l) e))))

(define (has-recursive? l pred?)
  (cond
    ((null? l) #f)
    ((list? (car l)) (or (has-recursive? (car l) pred?) (has-recursive? (cdr l) pred?)))
    ((pred? (car l)) #t)
    (else (has-recursive? (cdr l) e))))

(define (!= a b) (not (eq? a b)))

(define test-body '( (define x 6) (define y (* x 2)) (define z cons) (z x y) ) )
(define (delay-expression exp-m) (cdr exp-m))
(define (cache-index exp-m) (- (cadr exp-m) 1))
(define (exp-values exp-m) (cdr exp-m))
(define (add-to-cache! input success) (if (not (unspecified? input) ) (begin (set! cache (append cache (list input))) (set! cache-size (+ 1 cache-size)) (success))))

(eval-m '(define (map1 p l) (display l) (newline)  (if (null? l) '() (cons (p (car l)) (map1 p (cdr l))))) 
	the-global-environment)
(eval-m '(define (map2 p l1 l2) (if (null? l1) '() (cons (p (car l1) (car l2)) (map2 p (cdr l1) (cdr l2))))) 
	the-global-environment)


(define test-body-eva '((define a 1) (define b (+ a x)) (+ a b)) )

(define (apply-no-extra-frame-m procedure arguments)
  (define (scan-out-defines body)
    (define (get-pure-body current-elements)
      (cond 
	((null? current-elements) error "NO BODY")
	((definition? (car current-elements)) (get-pure-body (cdr current-elements)))
	(else current-elements)))
    (define pure-body (get-pure-body body))
    (let* ((defines (map cdr (filter definition? body)))
	   (names (map (lambda (pair) (let-parameter pair)) defines))
	   (sets (map (lambda (pair) (make-set (let-parameter pair) (let-expression pair))) defines)))
     (cons names (append sets pure-body))))
  (define result (scan-out-defines (procedure-body procedure)))
  (cond 
    ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure) (eval-m-sequence (cdr result)
				     (extend-environment (append (car result) (procedure-parameters procedure)) 
							 (append (map (lambda (x) '*unassigned*) (car result)) arguments) 
							 (procedure-environment procedure))))
    (else (error "Unknown procedure type -- APPLY" procedure))))

(define (letrec->let exp-m)
  (let* ((defines (let-pairs exp-m))
	 (names (map let-parameter defines))
	 (sets (map (lambda (pair) (make-set (let-parameter pair) (let-expression pair))) defines)))
  (make-let names (append sets (let-body exp-m)))))

(define factorial-Y (lambda (n)
		       ((lambda (fact) (fact fact n))
			(lambda (ft k)
			  (if (= k 1)
			    1
			    (* k (ft ft (- k 1))))))))
(define fibonacci-Y (lambda (n)
		      ((lambda (fib) (fib fib n 0 1)) 
		       (lambda (fibo n a b) (if (= n 0) a (fibo fibo (- n 1) b (+ a b)))))))

(define (parity x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n) (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n) (if (= n 0) false (ev? od? ev? (- n 1))))))

(define (eval-analyze exp-m env) ((analyze exp-m) env))

(define (analyze exp-m)
  (cond 
    ((self-evaluating? exp-m) (analyze-self-evaluating exp-m))
    ((quoted? exp-m) (analyze-quoted exp-m))
    ((variable? exp-m) (analyze-variable exp-m))
    ((assignment? exp-m) (analyze-assignment exp-m))
    ((definition? exp-m) (analyze-definition exp-m))
    ((let? exp-m) (analyze-application (let->combination exp-m)))
    ((if? exp-m) (analyze-if exp-m))
    ((lambda? exp-m) (analyze-lambda exp-m))
    ((begin? exp-m) (analyze-sequence (begin-actions exp-m)))
    ((cond? exp-m) (analyze (cond->if exp-m)))
    ((application? exp-m) (analyze-application exp-m))
    (else (error "Unknown expression type -- ANALYZE" exp-m))))

(define (analyze-self-evaluating exp-m) (lambda (env) exp-m))
(define (analyze-quoted exp-m) (let ((qval (text-of-quotation exp-m))) (lambda (env) qval)))
(define (analyze-variable exp-m) (lambda (env) (lookup-variable-value exp-m env)))
(define (analyze-assignment exp-m) 
  (let ((var (assignment-variable exp-m))
	(vproc (analyze (assignment-value exp-m))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp-m)
  (let ((var (definition-variable exp-m))
	(vproc (analyze (definition-value exp-m))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp-m)
  (let ((pproc (analyze (if-predicate exp-m)))
	(cproc (analyze (if-consequent exp-m)))
	(aproc (analyze (if-alternative exp-m))))
    (lambda (env)
      (if (true? (pproc env))
	(cproc env)
	(aproc env)))))

(define (analyze-lambda exp-m)
  (let ((vars (lambda-parameters exp-m))
	(bproc (analyze-sequence (lambda-body exp-m))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
	    (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-sequence-Lysa exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
	  (else ((car procs) env)
		(execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))


(define (analyze-application exp-m)
  (let ((fproc (analyze (operator exp-m)))
	(aprocs (map analyze (operands exp-m))))
    (lambda (env) 
      (execute-application (fproc env)
			   (map (lambda (aproc) (aproc env))
				aprocs)))))
(define (execute-application proc args)
  (cond 
    ((primitive-procedure? proc) (apply-primitive-procedure proc args))
    ((compound-procedure? proc) ((procedure-body proc) (extend-environment (procedure-parameters proc) args (procedure-environment proc))))
    (else (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (driver-loop-analyze)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval-analyze input the-global-environment)))
      (announce-output output-prompt)
      (add-to-cache! output display-cache-entrance)
      (user-print output)))
  (driver-loop-analyze ))

;(define (unless condition usual-value exceptional-value) (if condition exceptional-value usual-value))
(define-syntax unless
  (syntax-rules ()
		((_ condition exceptional-value usual-value) (if (not condition) exceptional-value usual-value))))
(define (factorial n) (unless (= n 1) (* n (factorial (- n 1))) 1))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (delay-it-pure exp env)
  (list 'thunk exp env))
(define (thunk-pure? obj)
  (tagged-list? obj 'thunk))

(define (delay-it-memo exp env)
  (list 'thunk exp env))
(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (memo-proc proc)
  (let ((already-run? false) 
	(result false))
    (lambda ()
      (if (not already-run?)
	(begin (set! result (proc))
	       (set! already-run? true)
	       result)
	result))))

(define (list->cons exp-m)
  (if (null? exp-m) ''() (list 'cons (first-exp-m  exp-m) (list->cons (rest-exp-ms exp-m)))))

(define (eval-m-lazy exp-m env)
  (cond 
    ((self-evaluating? exp-m) exp-m)
    ((variable? exp-m) (lookup-variable-value exp-m env))
    ((quoted? exp-m) (text-of-quotation-special exp-m env))
    ((cached? exp-m) (list-ref cache (cache-index exp-m))) 
    ((assignment? exp-m) (eval-m-assignment exp-m env))
    ((definition? exp-m) (eval-m-definition-lazy exp-m env))
    ((values? exp-m) (list-of-arg-values (exp-values exp-m) env))
    ((if? exp-m) (eval-if-lazy exp-m env))
    ((and? exp-m) (eval-m-and exp-m env))
    ((or? exp-m) (eval-m-or exp-m env))
    ((let? exp-m) (eval-m-lazy (let->combination exp-m) env))
    ((lambda? exp-m)
     (make-procedure (lambda-parameters exp-m)
		     (lambda-body exp-m)
		     env))
    ((begin? exp-m) (eval-m-sequence-lazy (begin-actions exp-m) env))
    ;((begin? exp-m) (eval-sequence-cy (begin-actions exp-m) env))
    ((cond? exp-m) (eval-m-lazy (cond->if exp-m) env))
    ((unbind? exp-m) (unbind (unbind-variable exp-m) env))
    ((list-lazy? exp-m) (eval-m-lazy (list->cons (rest-exp-ms exp-m)) env))
    ((let*? exp-m) (eval-m-lazy (let*->nested exp-m) env))
    ((letrec? exp-m)(eval-m-lazy (letrec->let exp-m) env))
    ((force? exp-m) (eval-m-lazy (delay-expression exp-m) env))
    ((env? exp-m) env)
    ((unless? exp-m) (eval-m-lazy (unless->if exp-m) env))
    ((application? exp-m)
     (apply-m-lazy (actual-value (operator exp-m) env)
		   (operands exp-m)
		   env))
    (else (error "Unknown exp-mression type -- EVAL" exp-m))))

(define (eval-m-sequence-lazy exp-ms env)
  (cond 
    ((last-exp-m? exp-ms) (eval-m-lazy (first-exp-m exp-ms) env))
    (else (eval-m-lazy (first-exp-m exp-ms) env)
	  (eval-m-sequence-lazy (rest-exp-ms exp-ms) env))))

(define (eval-sequence-cy exps env)
  (cond 
    ((last-exp-m? exps) (eval-m-lazy (first-exp-m exps) env))
    (else (actual-value (first-exp-m exps) env)
	  (eval-sequence-cy (rest-exp-ms exps) env))))

(define (actual-value exp-m env) (force-it (eval-m-lazy exp-m env)))

(define (apply-m-lazy procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure (list-of-arg-values arguments env))) ; changed
	((compound-procedure? procedure)
	 (eval-m-sequence-lazy (procedure-body procedure)
			(extend-environment
			  (procedure-parameters procedure)
			  (list-of-delayed-args arguments env) ; changed
			  (procedure-environment procedure))))
	(else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps) env)
	  (list-of-arg-values (rest-operands exps)
			      env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps) env)
	  (list-of-delayed-args (rest-operands exps)
				env))))

(define (eval-m-definition-lazy exp-m env)
  (define-variable! (definition-variable exp-m)
		    (eval-m-lazy (definition-value exp-m) env)
		    env)
  'ok)


(define (eval-if-lazy exp-m env)
  (if 
    (true? (actual-value (if-predicate exp-m) env))
    (eval-m-lazy (if-consequent exp-m) env)
    (eval-m-lazy (if-alternative exp-m) env)))

(define input-prompt-lazy ";;; L-Eval input:")
(define output-prompt-lazy ";;; L-Eval value:")
(define (driver-loop-lazy)
  (prompt-for-input input-prompt-lazy)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt-lazy)
      (user-print output)))
  (driver-loop-lazy))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it-pure obj)
  (if (thunk? obj)
    (actual-value (thunk-exp obj) (thunk-env obj))
  obj))

(define (force-it obj)
  (cond 
    ((thunk? obj) (let ((result (actual-value
				  (thunk-exp obj)
				  (thunk-env obj))))
		    (set-car! obj 'evaluated-thunk)
		    (set-car! (cdr obj) result) ; replace exp with its value
		    (set-cdr! (cdr obj) '()) ; forget unneeded env
		    result))
    ((evaluated-thunk? obj) (thunk-value obj))
    (else obj)))

(define (force-it-special obj)
  (cond
    ((thunk-pure? obj) (actual-value (trunk-exp obj) (trunk-env obj)))
    ((thunk-memo? obj) (let ((result (actual-value
				       (thunk-exp obj)
				       (thunk-env obj))))
			 (set-car! obj 'evaluated-thunk)
			 (set-car! (cdr obj) result) ; replace exp with its value
			 (set-cdr! (cdr obj) '()) ; forget unneeded env
			 result))
    ((evaluated-thunk? obj) (thunk-value obj))
    (else obj)))


(eval-m-lazy '(define (p1 x) (set! x (cons x '(2))) x) the-global-environment)
(eval-m-lazy '(define (p2 x) (define (p e) e x) (p (set! x (cons x '(2))))) the-global-environment)

(define (apply-m-speical procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure (list-of-arg-values arguments env))) ; changed
	((compound-procedure? procedure)
	 (eval-m-sequence (procedure-body procedure)
			(extend-environment
			  (map (lambda (element) (if (symbol? element) element (car element))) (procedure-parameters procedure))
			  (list-of-values-speicialised parameters arguments env) ; changed
			  (procedure-environment procedure))))
	(else (error "Unknown procedure type -- APPLY" procedure))))

(define (lazy? exp-m) (eq? 'lazy (cadr exp-m)))
(define (memoized? exp-m) (eq? 'memoized (cadr exp-m)))
(define (list-of-values-speicialised parameters arguments env)
  (map (lambda (parameter argument)
	 (cond
	   ((symbol? parameter) (eval-m argument env))
	   ((lazy? parameter) (delay-it-pure argument env))
	   ((memoized? parameters) (delay-it-memo arguements env)))) parameters arguments))

(define (amb-choices exp-m) (cdr exp-m))

(define (ambeval exp-m env succeed fail) ((amb-analyze exp-m) env succeed fail))

(define (require-predicate exp) (cadr exp))

(define (amb-analyze exp-m)
  (cond
    ((amb? exp-m) (amb-analyze-amb exp-m))
    ((ramb? exp-m) (amb-analyze-ramb exp-m))
    ((require? exp-m) (amb-analyze-require exp-m))
    ((self-evaluating? exp-m) (amb-analyze-self-evaluating exp-m))
    ((quoted? exp-m) (amb-analyze-quoted exp-m))
    ((variable? exp-m) (amb-analyze-variable exp-m))
    ((assignment? exp-m) (amb-analyze-assignment exp-m))
    ((permanent-assignment? exp-m) (amb-analyze-permanent-assignment exp-m))
    ((definition? exp-m)  (amb-analyze-definition exp-m))
    ((let? exp-m) (amb-analyze-application (let->combination exp-m)))
    ((if? exp-m) (amb-analyze-if exp-m))
    ((if-fail? exp-m) (amb-analyze-if-fail exp-m))
    ((lambda? exp-m) (amb-analyze-lambda exp-m))
    ((and? exp-m) (amb-analyze (and->if exp-m '())))
    ((or? exp-m) (amb-analyze (or->if exp-m '())))
    ((begin? exp-m) (amb-analyze-sequence (begin-actions exp-m)))
    ((cond? exp-m) (amb-analyze (cond->if exp-m)))
    ((application? exp-m) (amb-analyze-application exp-m))
    (else (error "Unknown expression type -- ANALYZE" exp-m))))

(define (amb-analyze-self-evaluating exp-m)
  (lambda (env succeed fail)
    (succeed exp-m fail)))

(define (amb-analyze-quoted exp-m)
  (let ((qval (text-of-quotation exp-m)))
    (lambda (env succeed fail) (succeed qval fail))))

(define (amb-analyze-variable exp-m)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp-m env)
	     fail)))

(define (amb-analyze-lambda exp-m)
  (let ((vars (lambda-parameters exp-m))
	(bproc (amb-analyze-sequence (lambda-body exp-m))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))


(define (amb-analyze-if exp-m)
  (let 
    ((pproc (amb-analyze (if-predicate exp-m)))
     (cproc (amb-analyze (if-consequent exp-m)))
     (aproc (amb-analyze (if-alternative exp-m))))
    (lambda (env succeed fail)
      (pproc env
	     ;; success continuation for evaluating the predicate
	     ;; ;; to obtain pred-value
	     (lambda (pred-value fail2)
	       (if (true? pred-value)
		 (cproc env succeed fail2)
		 (aproc env succeed fail2)))
	     ;; failure continuation for evaluating the predicate
	     fail))))

(define (amb-analyze-if-fail exp-m)
  (let 
    ((first (amb-analyze (cadr exp-m)))
     (second (amb-analyze (caddr exp-m))))
    (lambda (env succeed fail)
      (first env
	     (lambda (value fail2) value)
	     ;; failure continuation for evaluating the predicate
	     (lambda () (second env (lambda (value fail2) value) fail))))))

(define (amb-analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
	 ;; success continuation for calling a
	 (lambda (a-value fail2)
	   (b env succeed fail2))
	 ;; failure continuation for calling a
	 fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
	    (cdr rest-procs))))
  (let ((procs (map amb-analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;(define (for-each proc items) (if (null? items) 'done (begin (proc (car items)) (for-each proc (cdr items))))) 

(define (amb-analyze-definition exp-m)
  (let ((var (definition-variable exp-m))
	(vproc (amb-analyze (definition-value exp-m))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (define-variable! var val env)
	       (succeed 'ok fail2))
	     fail))))


(define (amb-analyze-assignment exp-m)
  (let ((var (assignment-variable exp-m))
	(vproc (amb-analyze (assignment-value exp-m))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2) ; *1*
	       (let ((old-value (lookup-variable-value var env)))
		 (set-variable-value! var val env)
		 (succeed 'ok 
			  (lambda () ; *2*
			    (set-variable-value! var old-value env)
			    (fail2)))))
	     fail))))

(define (amb-analyze-permanent-assignment exp-m)
  (let ((var (assignment-variable exp-m))
	(vproc (amb-analyze (assignment-value exp-m))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2) ; *1*
		 (set-variable-value! var val env)
		 (succeed 'ok  (lambda () (fail2))))
	     fail))))

(define (amb-analyze-application exp-m)
  (let 
    ((fproc (amb-analyze (operator exp-m)))
     (aprocs (map amb-analyze (operands exp-m))))
    (lambda (env succeed fail)
      (fproc env
	     (lambda (proc fail2)
	       (get-args aprocs
			 env
			 (lambda (args fail3)
			   (amb-execute-application
			     proc args succeed fail3))
			 fail2))
	     fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
		  ;; success continuation for this aproc
		  (lambda (arg fail2)
		    (get-args (cdr aprocs)
			      env
			      ;; success continuation for recursive
			      ;; call to get-args
			      (lambda (args fail3)
				(succeed (cons arg args)
					 fail3))
			      fail2))
		  fail)))

(define (amb-execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
	 (succeed (apply-primitive-procedure proc args) fail))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment (procedure-parameters proc)
			      args
			      (procedure-environment proc))
	  succeed
	  fail))
	(else (error "Unknown procedure type -- AMB-EXECUTE-APPLICATION" proc))))

(define (amb-analyze-amb exp-m)
  (let ((cprocs (map amb-analyze (amb-choices exp-m))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices) (fail)
	  ((car choices) env
			 succeed
			 (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (amb-analyze-ramb exp-m)
  (let ((cprocs (map amb-analyze (amb-choices exp-m))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices) (fail)
	  ((car choices) env
			 succeed
			 (lambda () (try-next (cdr choices))))))
      (try-next (randomize cprocs)))))

(define (randomize l)
  (define (list-without l index)
    (define (work list current-index)
      (if (= current-index index) 
	(cdr list)
	(cons (car list) (work (cdr list) (+ 1 current-index)))))
    (work l 0))
  (define (work current)
    (if (null? current) '()
      (let ((index (random (length current))))
      (cons (list-ref current index) (work (list-without current index))))))
  (work l))

(define (amb-analyze-require exp)
  (let ((pproc (amb-analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if (not pred-value) (fail2) (succeed 'ok fail2)))
	     fail))))


(define amb-input-prompt ";;; Amb-Eval input:")
(define amb-output-prompt ";;; Amb-Eval value:")
(define (amb-driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input amb-input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
	(try-again)
	(begin
	  (newline)
	  (display ";;; Starting a new problem ")
	  (ambeval input 
		   the-global-environment
		   ;; ambeval success
		   (lambda (val next-alternative)
		     (announce-output amb-output-prompt)
		     (user-print val)
		     (internal-loop next-alternative))
		   ;; ambeval failure
		   (lambda ()
		     (announce-output ";;; There are no more values of")
		     (user-print input)
		     (amb-driver-loop)))))))
  (internal-loop (lambda () (newline) (display ";;; There is no current problem") (amb-driver-loop))))

(define (amb-import file-name)
  (define port (open-input-file file-name))
  (define (work number) 
   (let ((line (read port)))
     (if (eof-object? line)
          'done
          (begin (ambeval line the-global-environment (lambda (val fail) 'ok) (lambda () 'fail!) ); (display number) (display "\t") (display line) (newline)  
		 (work (+ 1 number))))))
  (work 0) 
	  (close-input-port port))

(define (multiple-dwelling-pure)
  (map (lambda (smith) 
	 (map (lambda (baker) 	  
		(map (lambda (fletcher)
		       	  (map (lambda (cooper) 
				(map (lambda (miller)
				       (list (list 'baker baker) (list 'cooper cooper) (list 'fletcher fletcher) (list 'miller miller) (list 'smith smith)))   
				     (filter (lambda (miller) (and (not (= fletcher miller)) (not (= miller smith)) (not (= miller baker)) (> miller cooper) (not (= (abs (- cooper fletcher)) 1)))) (list 3 4 5)))) 
			      (filter (lambda (cooper) (and (not (= cooper smith)) (not (= cooper fletcher)) (not (= baker cooper)))) (list 2 3 4))))
		       (filter (lambda (fletcher) (and (not (= fletcher baker)) (not (= fletcher smith)) (not (= (abs (- smith fletcher)) 1)))) (list 2 4))))  
		(filter (lambda (baker) (not (= baker smith))) (list 1 2 3 4)))) 
       (list 1 2 3 4 5)))

(amb-import "interpret.scm")
(amb-import "AmbQuery.scm")

(define query-input-prompt ";;; Query input:")
(define query-output-prompt ";;; Query results:")
(define (query-driver-loop)
  (prompt-for-input query-input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond 
      ((assertion-to-be-added? q) (add-rule-or-assertion! (add-assertion-body q))
				  (newline)
				  (display "Assertion added to data base.")
				  (query-driver-loop))
      (else (newline) (display query-output-prompt)
	    (display-stream
	      (stream-map
		(lambda (frame) (instantiate q frame (lambda (v f) (contract-question-mark v))))
		(qeval q (singleton-stream '()))))
	    (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond 
      ((var? exp) (let ((binding (binding-in-frame exp frame)))
		    (if binding
		      (copy (binding-value binding))
		      (unbound-var-handler exp frame))))
      ((pair? exp) (cons (copy (car exp)) (copy (cdr exp))))
      (else exp)))
  (copy exp))

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
      (qproc (contents query) frame-stream)
      (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
	(find-assertions query-pattern frame)
	(delay (apply-rules query-pattern frame))))
    frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream	
    (conjoin (rest-conjuncts conjuncts)
	     (qeval (first-conjunct conjuncts)
		    frame-stream))))

(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts)
		      frame-stream)))))

(put 'or 'qeval disjoin)

(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
	(singleton-stream frame)
	the-empty-stream))
    frame-stream))

(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if 
	(execute (instantiate call frame (lambda (v f) (error "Unknown pat var -- LISP-VALUE" v))))
	(singleton-stream frame)
	the-empty-stream))
    frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
	 (args exp)))

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
		    (check-an-assertion datum pattern frame))
		  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
      the-empty-stream
      (singleton-stream match-result))))


(define (pattern-match pat dat frame)
  (cond 
    ((eq? frame 'failed) 'failed)
    ((equal? pat dat) frame) 
    ((var? pat) (extend-if-consistent pat dat frame))
    ((and (pair? pat) (pair? dat)) (pattern-match (cdr pat)
						  (cdr dat)
						  (pattern-match (car pat)
								 (car dat)
								 frame)))
    (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
      (pattern-match (binding-value binding) dat frame)
      (extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
		    (apply-a-rule rule pattern frame))
		  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
	    (unify-match query-pattern
			 (conclusion clean-rule)
			 query-frame)))
      (if (eq? unify-result 'failed)
	the-empty-stream
	(qeval (rule-body clean-rule)
	       (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond 
	((var? exp) (make-new-variable exp rule-application-id))
	((pair? exp)(cons (tree-walk (car exp))
			  (tree-walk (cdr exp))))
	(else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond 
    ((eq? frame 'failed) 'failed)
    ((equal? p1 p2) frame)
    ((var? p1) (extend-if-possible p1 p2 frame))
    ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
    ((and (pair? p1) (pair? p2))
     (unify-match (cdr p1)
		  (cdr p2)
		  (unify-match (car p1)
			       (car p2)
			       frame)))
    (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond 
      (binding (unify-match (binding-value binding) val frame))
      ((var? val) ; ***
       (let ((binding (binding-in-frame val frame)))
	 (if binding 
	   (unify-match var (binding-value binding) frame) 
	   (extend var val frame))))
      ((depends-on? val var frame) 'failed)
      (else (extend var val frame)))))


(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond 
      ((var? e) (if (equal? var e)
		  true
		  (let ((b (binding-in-frame e frame)))
		    (if b
		      (tree-walk (binding-value b))
		      false))))
      ((pair? e) (or (tree-walk (car e))
		     (tree-walk (cdr e))))
      (else false)))
(tree-walk exp))

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))


(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules)))

(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream)))


(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
      (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
    (let ((key (index-key-of assertion)))
      (let ((current-assertion-stream (get-stream key 'assertion-stream)))
	(put key 'assertion-stream
	     (cons-stream assertion current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
      (let ((key (index-key-of pattern)))
	(let ((current-rule-stream (get-stream key 'rule-stream)))
	  (put key 'rule-stream
	       (cons-stream rule current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat) (constant-symbol? (car pat)))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream
      (stream-car s1)
      (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream
      (stream-car s1)
      (interleave-delayed (force delayed-s2)
			  (delay (stream-cdr s1))))))

(define (stream-flatmap proc s) (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x) (cons-stream x the-empty-stream))

(define (type exp)
  (if (pair? exp)
    (car exp)
    (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
    (cdr exp)
    (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp) (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement) (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true)
  (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond 
    ((pair? exp) (cons (map-over-symbols proc (car exp))
		       (map-over-symbols proc (cdr exp))))
    ((symbol? exp) (proc exp))
    (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
      (list '? (string->symbol(substring chars 1 (string-length chars))))
      symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)
(define (new-rule-application-id) (set! rule-counter (+ 1 rule-counter)) rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
(string->symbol (string-append "?" 
			       (if (number? (cadr variable))
				     (string-append (symbol->string (caddr variable))
						    "-"
						    (number->string (cadr variable)))
				     (symbol->string (cadr variable))))))

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding)
  (car binding))
(define (binding-value binding)
  (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))
(define (extend-with-binding binding frame)
  (cons binding frame))

(define (singleton-stream? stream) (or (not (pair? stream)) (eq? the-empty-stream (stream-cdr stream))))
(define (uniquely-asserted exp frame-stream)
  (let 
    ((result (stream-flatmap (lambda (frame) (qeval (car exp) (singleton-stream frame))) frame-stream)))
    (if (singleton-stream? result) result the-empty-stream)))

(put 'unique 'qeval uniquely-asserted)

(define (simple-stream-flatmap proc s)
(simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
(stream-map (lambda (frame) (if (singleton-stream? frame) (car frame) frame))
	    (stream-filter (lambda (frame) (not (stream-null? x)))  stream)))

(define (query-import file-name)
  (define port (open-input-file file-name))
  (define (work number) 
   (let ((line (read port)))
     (if (eof-object? line)
          'done 
	  (let ((q (query-syntax-process line)))
	    (cond 
	      ((assertion-to-be-added? q) (add-rule-or-assertion! (add-assertion-body q)))
	      (else (stream-map (lambda (frame) (instantiate q frame (lambda (v f) (contract-question-mark v)))) (qeval q (singleton-stream '())))))
	     ; (display number) (display "\t") (display line) (newline)  
	     (work (+ 1 number))))))
  (work 0) 
  (close-input-port port))

(query-import "Microshaft.scm")

(use-modules (ice-9 r5rs))
(define user-initial-environment (scheme-report-environment 5))

(define (combine-streams frame-stream-1 frame-stream-2)
  (if (eq? the-empty-stream frame-stream-2) 
    frame-stream-1
		 (stream-flatmap (lambda (frame-1) (stream-filter (lambda (result) (not (eq? result 'failed))) 
								  (stream-map (lambda (frame-2) (combine-frames frame-1 frame-2)) frame-stream-2))) 
				   frame-stream-1)))

(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))

(define (combine-frames frame-1 frame-2)
  (cond 
   ((null? frame-1) frame-2)
   ((eq? frame-2 'failed) 'failed)
   ((binding-in-frame (binding-variable (first-binding frame-1)) frame-2)
    (let ((variable (binding-variable (first-binding frame-1))))
      (combine-frames (rest-bindings frame-1) (if (same? (binding-in-frame variable frame-1) (binding-in-frame variable frame-2)) 
						frame-2
						'failed))))
   (else (combine-frames (rest-bindings frame-1) (extend-with-binding (first-binding frame-1) frame-2)))))

(define (same? x y) 
  (cond
    ((and (null? x) (null? y)) true)
    ((and (pair? x) (pair? y)) (and (same? (car x) (car y)) (same? (cdr x) (cdr y))))
    ((and (not (pair? x)) (not (pair? y))) (eq? x y))
    (else false)))

(define (conjoin-faster conjuncts frame-stream) (if (null? conjuncts) 
						  the-empty-stream 
						  (combine-streams (qeval (first-conjunct conjuncts) frame-stream) 
								   (conjoin-faster (rest-conjuncts conjuncts) frame-stream))))
(define (get-query-variables query)
  (cond
    ((null? query) '())
    ((var? query) (list query))
    ((symbol? (car query)) (get-query-variables (cdr query)))
    ((var? (car query)) (cons (car query) (get-query-variables (cdr query))))
    ((pair? (car query))(append (get-query-variables (car query)) (get-query-variables (cdr query))))))

(define (not-faster pattern frame-stream)
  (define variables (get-query-variables pattern))
  (stream-flatmap (lambda (frame) (not-check pattern frame) frame-stream)))

(define (not-check pattern frame variables)
    (if (= (length variables) (length (filter (lambda (variable) (binding-in-frame variable frame)) variables)))
	(if (stream-null? (qeval (negated-query pattern) (singleton-stream frame))) (singleton-stream frame) the-empty-stream) 
	 ( frame  ;the-empty-stream What is the issue?
	)))

(define (fast-check pattern frame variables check)
    (if (= (length variables) (length (filter (lambda (variable) (binding-in-frame variable frame)) variables)))
	(if (check) (singleton-stream frame) the-empty-stream) 
	 ( frame  ;the-empty-stream What is the issue?
	)))

(define (lisp-value-faster call frame-stream)
  (define variables (get-query-variables pattern))
  (stream-flatmap
    (lambda (frame)
      (if (= (length variables) (length (filter (lambda (variable) (binding-in-frame variable frame)) variables)))
	(if (execute (instantiate call frame (lambda (v f) (error "Unknown pat var -- LISP-VALUE BUT THIS IS IMPOSSIBLE" v))))
	  (singleton-stream frame)
	  the-empty-stream)
	the-empty-stream ;Here!
	)
    frame-stream)))
(put 'lisp-value-f 'qeval lisp-value-faster)


(define (exit-query ignore stream) (exit))
(put 'and-f 'qeval conjoin-faster)
(put 'not-f 'qeval not-faster)
(put 'exit 'qeval exit-query)
;(and-f (job ?x ?y) (job ?x ?y))
;(and-f (job ?x ?y) (salary ?x ?amount))
;(and (supervisor ?x ?y) (not-f (job ?x (computer programmer))))
;(and (not-f (job ?x (computer programmer))) (supervisor ?x ?y))
