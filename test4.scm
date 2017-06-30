(load "streams.base.scm")
(load "evaluator.base.scm")
(load "table.base.scm")

(define (eval-m exp env)
  (cond 
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((cached? exp) (list-ref cache (cache-index exp))) 
    ((assignment? exp) (eval-m-assignment exp env))
    ((definition? exp) (eval-m-definition exp env))
    ((values? exp) (list-of-values (exp-values exp) env))
    ((if? exp) (eval-m-if exp env))
    ((and? exp) (eval-m-and exp env))
    ((or? exp) (eval-m-or exp env))
    ((let? exp) (eval-m (let->combination exp) env))
    ((lambda? exp)
     (make-procedure (lambda-parameters exp)
		     (lambda-body exp)
		     env))
    ((begin? exp) (eval-m-sequence (begin-actions exp) env))
    ((cond? exp) (eval-m (cond->if exp) env))
    ((unbind? exp) (unbind (unbind-variable exp) env))
    ((let*? exp) (eval-m (let*->nested exp) env))
    ((letrec? exp)(eval-m (letrec->let exp) env))
    ((delay? exp) (make-delay (delay-expression exp)))
    ((force? exp) (eval-m (delay-expression exp) env))
    ((env? exp) env)
    ((unless? exp) (eval-m (unless->if exp) env))
    ((application? exp)
     (apply-m (eval-m (operator exp) env)
	    (list-of-values (operands exp) env)))
    (else (error "Unknown expression type -- EVAL" exp)))	
  'ok)


(define (eval-m-definition exp env)
  (define-variable! (definition-variable exp)
		    (eval-m (definition-value exp) env)
		    env)
  'ok)

(define (get-type exp) (car exp)) 
(define (eval-m-table exp env) ((get 'eval-m (get-type exp)) exp env))

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


(define (list-of-values-special exps env order)
  (define (work current)
    (if (no-operands? current)
      '()
      (cons (eval-m (first-operand current) env)
	    (work (rest-operands exps) env))))
  (if (eq? order (eval-m-order))
    (work exps)
    (work (reverse exps))))

(define (order-exps exps order)
    (if (eq? order (eval-m-order))
    exps
    (reverse exps)))

(define cond-test '(cond ((= x 5) 4) ((= x 6) 5) ) )


(define (eval-m-and exp env)
  (define (work elements)
  (cond
    ((null? elements) true)
    ((false? (eval-m (first-operand elements) env) false)
    (else (work (rest-operands elements))))))
 (work (order-exps (operands exp) 'right)))

(define (eval-m-or exp env)
  (define (work elements)
  (cond
    ((null? elements) false)
    ((true? (eval-m (first-operand element) env) true)
    (else (work (rest-operands elements))))))
  (work (order-exps (operands exp) 'right)))


(define (let->combination exp) ;(if (let-special? exp)
				;	(let ((parameters (let-special-parameters exp)) (body (let-special-body exp)))	
				;	  (display exp)
				;	  (make-begin (list
				;		       (make-define (let-special-name exp) parameters body)
				;		       (cons (let-special-name exp) (let-special-expressions exp)))))
					  (cons
					    (make-lambda (let-parameters exp) (let-body exp))
					    (let-expressions exp)));)

(define let*-test '(let* ((x (* 2 3)) (y (* 3 x))) (* x y)))
(define let-test '(let ((x (* 2 3)) (y (* 3 6))) (* x y)))
(define let-special-test '(define (fib n) 
			    (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) )

(define (let*->nested-lets exp)
  (define (work pairs body)
  (if (null? pairs)
     body
    (work (cdr pairs) (make-let (car pairs) body))))
  (work (order-exps (let-pairs exp) 'right) (let-body exp)))

(define (unless->if exp) (make-if (cons ('not (if-predicate exp))) (if-alternative exp) (if-consequent exp)))

(define (do-init-triplets exp) (cadr exp))
(define (do-init-vars exp) (map car (do-init-triplets exp)))
(define (do-init-starters exp) (map cadr (do-init-triplets exp)))
(define (do-init-steps exp) (map caddr (do-init-triplets exp)))
(define (do-test exp) (cadddr exp))
(define (do-test-condition exp) (car (do-test exp)))
(define (do-test-end exp) (cdr (do-test exp)))
(define (do-tests exp) (caddr exp))
(define (do-commands exp) (cadddr exp))

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

(define (display-cache-entrance) ;(display cache) 
  (newline) (display "$") (display cache-size) (display " = "))


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


(define the-global-environment (setup-environment))

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
(define (delay-expression exp) (cdr exp))
(define (cache-index exp) (- (cadr exp) 1))
(define (exp-values exp) (cdr exp))
(define (add-to-cache! input success) (if (not (unspecified? input) ) (begin (set! cache (append cache (list input))) (set! cache-size (+ 1 cache-size)) (success))))

;(eval-m '(define (map1 p l) (display l) (newline)  (if (null? l) '() (cons (p (car l)) (map1 p (cdr l))))) the-global-environment)
;(eval-m '(define (map2 p l1 l2) (if (null? l1) '() (cons (p (car l1) (car l2)) (map2 p (cdr l1) (cdr l2))))) the-global-environment)
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

(define (letrec->let exp)
  (let* ((defines (let-pairs exp))
	 (names (map let-parameter defines))
	 (sets (map (lambda (pair) (make-set (let-parameter pair) (let-expression pair))) defines)))
  (make-let names (append sets (let-body exp)))))

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

(define (eval-analyze exp env) ((analyze exp) env))

(define (analyze exp)
  (cond 
    ((self-evaluating? exp) (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((definition? exp) (analyze-definition exp))
    ((let? exp) (analyze-application (let->combination exp)))
    ((if? exp) (analyze-if exp))
    ((lambda? exp) (analyze-lambda exp))
    ((begin? exp) (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((application? exp) (analyze-application exp))
    (else (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp) (lambda (env) exp))
(define (analyze-quoted exp) (let ((qval (text-of-quotation exp))) (lambda (env) qval)))
(define (analyze-variable exp) (lambda (env) (lookup-variable-value exp env)))
(define (analyze-assignment exp) 
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
	(cproc env)
	(aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze-sequence (lambda-body exp))))
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


(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
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

(define (list->cons exp)
  (if (null? exp) ''() (list 'cons (first-exp  exp) (list->cons (rest-exps exp)))))

(define (eval-m-lazy exp env)
  (cond 
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation-special exp env))
    ((cached? exp) (list-ref cache (cache-index exp))) 
    ((assignment? exp) (eval-m-assignment exp env))
    ((definition? exp) (eval-m-definition-lazy exp env))
    ((values? exp) (list-of-arg-values (exp-values exp) env))
    ((if? exp) (eval-if-lazy exp env))
    ((and? exp) (eval-m-and exp env))
    ((or? exp) (eval-m-or exp env))
    ((let? exp) (eval-m-lazy (let->combination exp) env))
    ((lambda? exp)
     (make-procedure (lambda-parameters exp)
		     (lambda-body exp)
		     env))
    ((begin? exp) (eval-m-sequence-lazy (begin-actions exp) env))
    ;((begin? exp) (eval-sequence-cy (begin-actions exp) env))
    ((cond? exp) (eval-m-lazy (cond->if exp) env))
    ((unbind? exp) (unbind (unbind-variable exp) env))
    ((list-lazy? exp) (eval-m-lazy (list->cons (rest-exps exp)) env))
    ((let*? exp) (eval-m-lazy (let*->nested exp) env))
    ((letrec? exp)(eval-m-lazy (letrec->let exp) env))
    ((force? exp) (eval-m-lazy (delay-expression exp) env))
    ((env? exp) env)
    ((unless? exp) (eval-m-lazy (unless->if exp) env))
    ((application? exp)
     (apply-m-lazy (actual-value (operator exp) env)
		   (operands exp)
		   env))
    (else (error "Unknown expression type -- EVAL" exp))))

(define (eval-m-sequence-lazy exps env)
  (cond 
    ((last-exp? exps) (eval-m-lazy (first-exp exps) env))
    (else (eval-m-lazy (first-exp exps) env)
	  (eval-m-sequence-lazy (rest-exps exps) env))))

(define (eval-sequence-cy exps env)
  (cond 
    ((last-exp? exps) (eval-m-lazy (first-exp exps) env))
    (else (actual-value (first-exp exps) env)
	  (eval-sequence-cy (rest-exps exps) env))))

(define (actual-value exp env) (force-it (eval-m-lazy exp env)))

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

(define (eval-m-definition-lazy exp env)
  (define-variable! (definition-variable exp)
		    (eval-m-lazy (definition-value exp) env)
		    env)
  'ok)


(define (eval-if-lazy exp env)
  (if 
    (true? (actual-value (if-predicate exp) env))
    (eval-m-lazy (if-consequent exp) env)
    (eval-m-lazy (if-alternative exp) env)))

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

(define (lazy? exp) (eq? 'lazy (cadr exp)))
(define (memoized? exp) (eq? 'memoized (cadr exp)))
(define (list-of-values-speicialised parameters arguments env)
  (map (lambda (parameter argument)
	 (cond
	   ((symbol? parameter) (eval-m argument env))
	   ((lazy? parameter) (delay-it-pure argument env))
	   ((memoized? parameters) (delay-it-memo arguements env)))) parameters arguments))

(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail) ((amb-analyze exp) env succeed fail))

(define (require-predicate exp) (cadr exp))

(define (amb-analyze exp)
  (cond
    ((amb? exp) (amb-analyze-amb exp))
    ((ramb? exp) (amb-analyze-ramb exp))
    ((require? exp) (amb-analyze-require exp))
    ((self-evaluating? exp) (amb-analyze-self-evaluating exp))
    ((quoted? exp) (amb-analyze-quoted exp))
    ((variable? exp) (amb-analyze-variable exp))
    ((assignment? exp) (amb-analyze-assignment exp))
    ((permanent-assignment? exp) (amb-analyze-permanent-assignment exp))
    ((definition? exp)  (amb-analyze-definition exp))
    ((let? exp) (amb-analyze-application (let->combination exp)))
    ((if? exp) (amb-analyze-if exp))
    ((if-fail? exp) (amb-analyze-if-fail exp))
    ((lambda? exp) (amb-analyze-lambda exp))
    ((and? exp) (amb-analyze (and->if exp '())))
    ((or? exp) (amb-analyze (or->if exp '())))
    ((begin? exp) (amb-analyze-sequence (begin-actions exp)))
    ((cond? exp) (amb-analyze (cond->if exp)))
    ((application? exp) (amb-analyze-application exp))
    (else (error "Unknown expression type -- ANALYZE" exp))))

(define (amb-analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (amb-analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) (succeed qval fail))))

(define (amb-analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
	     fail)))

(define (amb-analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (amb-analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))


(define (amb-analyze-if exp)
  (let 
    ((pproc (amb-analyze (if-predicate exp)))
     (cproc (amb-analyze (if-consequent exp)))
     (aproc (amb-analyze (if-alternative exp))))
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

(define (amb-analyze-if-fail exp)
  (let 
    ((first (amb-analyze (cadr exp)))
     (second (amb-analyze (caddr exp))))
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

(define (for-each proc items) (if (null? items) 'done (begin (proc (car items)) (for-each proc (cdr items))))) 

(define (amb-analyze-definition exp)
  (let ((var (definition-variable exp))
	(vproc (amb-analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (define-variable! var val env)
	       (succeed 'ok fail2))
	     fail))))


(define (amb-analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (amb-analyze (assignment-value exp))))
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

(define (amb-analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (amb-analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2) ; *1*
		 (set-variable-value! var val env)
		 (succeed 'ok  (lambda () (fail2))))
	     fail))))

(define (amb-analyze-application exp)
  (let 
    ((fproc (amb-analyze (operator exp)))
     (aprocs (map amb-analyze (operands exp))))
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

(define (amb-analyze-amb exp)
  (let ((cprocs (map amb-analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(if (null? choices) (fail)
	  ((car choices) env
			 succeed
			 (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (amb-analyze-ramb exp)
  (let ((cprocs (map amb-analyze (amb-choices exp))))
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

(define (simple-stream-flatmap proc s) (simple-flatten (stream-map proc s)))
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
;(amb-import "interpret.scm")
;(amb-import "AmbQuery.scm")

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
