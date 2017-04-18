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


(define query-input-prompt ";;; Query input:")
(define query-output-prompt ";;; Query results:")
(define (query-driver-loop)
  (prompt-for-input query-input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond
      ((eq? q 'try-again) (amb))
      ((assertion-to-be-added? q) (add-rule-or-assertion! (add-assertion-body q))
				  (newline)
				  (display "Assertion added to data base.")
				  (query-driver-loop))
      (else (newline) (display query-output-prompt)
	    (let ((frame (qeval q '()))) 
	      (require (not (null? frame)))
	      (display (instantiate q (qeval q '()) (lambda (v f) (contract-question-mark v))))
	      (query-driver-loop)))))

(define (query-driver-start input)
    (let ((q input))
    (cond
      ((eq? q 'try-again) (amb))
      ((assertion-to-be-added? q) (add-rule-or-assertion! (add-assertion-body q))
				  (newline)
				  (display "Assertion added to data base.")
				  (query-driver-loop))
      (else (newline) (display query-output-prompt)
	    (let ((frame (qeval q '()))) 
	      (require (not (null? frame)))
	      (display (instantiate q (qeval q '()) (lambda (v f) (contract-question-mark v))))
	      (query-driver-loop))))))

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

(define (qeval query frame)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
      (qproc (contents query) frame)
      (simple-query query frame))))

(define (simple-query query-pattern frame)
  (ramb	(find-assertions query-pattern frame)
	(apply-rules query-pattern frame)))

(define (conjoin conjuncts frame)
  (if (empty-conjunction? conjuncts)
    '()	
    (conjoin (rest-conjuncts conjuncts)
	     (qeval (first-conjunct conjuncts)
		    frame))))

(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame)
  (if (empty-disjunction? disjuncts)
    '()
    (ramb (qeval (first-disjunct disjuncts) frame)
	  (disjoin (rest-disjuncts disjuncts) frame))))

(put 'or 'qeval disjoin)

(define (negate operands frame)
  (require (stream-null? (qeval (negated-query operands) frame)))
  frame)

(put 'not 'qeval negate)

(define (lisp-value call frame)
  (require (execute (instantiate call frame (lambda (v f) (error "Unknown pat var -- LISP-VALUE" v)))))
  frame)

(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
	 (args exp)))

(define (always-true ignore frame) frame) 
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame) (check-an-assertion (a-random-element-of (fetch-assertions pattern frame)) pattern frame))		 

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (require (not (eq? match-result 'failed)))
      match-result))


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
  (apply-a-rule (a-random-element-of (fetch-rules pattern frame)) pattern frame))
		 

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
	    (unify-match query-pattern
			 (conclusion clean-rule)
			 query-frame)))
      (require (not (eq? unify-result 'failed)))
	(qeval (rule-body clean-rule)
	       unify-result))))

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


(define THE-ASSERTIONS '())
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s '())))

(define THE-RULES '())
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules)))

(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (append
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
      (cons assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
    (let ((key (index-key-of assertion)))
      (let ((current-assertion-stream (get-stream key 'assertion-stream)))
	(put key 'assertion-stream
	     (cons assertion current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
      (let ((key (index-key-of pattern)))
	(let ((current-rule-stream (get-stream key 'rule-stream)))
	  (put key 'rule-stream
	       (cons rule current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat) (constant-symbol? (car pat)))

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

(define (uniquely-asserted exp frame) (qeval (car exp) frame))

(put 'unique 'qeval uniquely-asserted)

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

(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))

(define (combine-frames frame-1 frame-2)
  (require (not (eq? frame-2 'failed)))
  (cond 
   ((null? frame-1) frame-2)
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

(define (conjoin-faster conjuncts frame) (if (null? conjuncts) '()
						  (combine-frames (qeval (first-conjunct conjuncts) frame) 
								   (conjoin-faster (rest-conjuncts conjuncts) frame))))


(define (exit-query ignore stream) (exit))
(put 'and-f 'qeval conjoin-faster)
(put 'exit 'qeval exit-query)
