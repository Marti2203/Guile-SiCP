(define true #t)
(define false #f)
(define (tagged-list? exp-m tag)
  (if (pair? exp-m)
    (eq? (car exp-m) tag)
    false))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
	      ((machine 'install-operations) ops)
	      ((machine 'install-instruction-sequence)
	       (assemble controller-text machine))
	      machine))

(define (make-register name)
  (let ((contents '*unassigned*)
	(traced false)
	)
    (define (display-info value)
    (newline)
    (display name)
    (display " ")
    (display contents)
    (display "=>")
    (display value)
    (newline))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'trace-on!) (set! traced true) 'trace-on)
	    ((eq? message 'traced) traced)
	    ((eq? message 'trace-off!) (set! traced false) 'trace-off)
	    ((eq? message 'set) (lambda (value) (if traced (display-info value)) (set! contents value)))
	    (else (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

(define (make-stack)
  (let ((s '())
	(number-pushes 0)
	(max-depth 0)
	(current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
	(error "Empty stack -- POP")
	(let ((top (car s)))
	  (set! s (cdr s))
	  (set! current-depth (- current-depth 1))
	  top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-stack-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
		     'maximum-depth '= max-depth)) (newline) )
    (define (dispatch message)
      (cond 
	((eq? message 'push) push)
	((eq? message 'pop) (pop))
	((eq? message 'initialize) (initialize))
	((eq? message 'print-stack-statistics) (print-stack-statistics))
	(else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
(stack 'pop))
(define (push stack value)
((stack 'push) value))

(define (make-stack-table-strict)
  (let ((s '()))
    (define (push var value)
      (set! s (cons (cons var value) s)))
    (define (pop var)
      (if (null? s)
	(error "Empty stack -- POP")
	(let ((top (car s)))
	  (if (eq? var (car top))
	    (begin (set! s (cdr s)) (cdr top))
	    (error "VARIABLE NOT SAME -- POP Table-Strict" var (car top))
	    ))))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond 
	((eq? message 'push) push)
	((eq? message 'pop)  pop)
	((eq? message 'initialize) (initialize))
	(else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop-table-strict stack variable)
((stack 'pop) variable))
(define (push-table-strict stack variable value)
((stack 'push) variable value))

(define (make-stack-table)
  (let ((s '()))
    (define (push var value)
      (let ((pair (assoc var s)))
      (if pair
	(begin (((cdr pair) 'push) value) s)
	(begin (set! s (cons (cons var (make-stack)) s)) (((cdar s) 'push) value) s))))
    (define (pop var)
      (if (null? s)
	(error "Empty stack -- POP")
	(let ((pair (assoc var s)))
	  (if pair ((cdr pair) 'pop)
	    (error "VARIABLE HAS NO STACK POP Table" var)
	    ))))
    (define (initialize)
      (for-each (lambda (pair) (initalize (cdr pair))) s)
      'done)
    (define (dispatch message)
      (cond 
	((eq? message 'push) push)
	((eq? message 'pop)  pop)
	((eq? message 'initialize) (initialize))
	(else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop-table stack variable)
((stack 'pop) variable))
(define (push-table stack variable value)
((stack 'push) variable value))
(define (initialize stack) (stack 'initialize))

(define (make-new-machine)
  (let (
		(pc (make-register 'pc))
		(flag (make-register 'flag))
		(stack (make-stack))
		(the-instruction-sequence '())
		(used-instructions '())
		(entry-points '())
		(stacked '())
		(assignments '())
		(instruction-counter 0)
		(instruction-count 0)
		(trace false)
		(labels '())
		(breakpoints '())
	)
    (let ((the-ops (list (list 'initialize-stack (lambda () (stack 'initialize)))))
	  (register-table (list (list 'pc pc) (list 'flag flag))))
      (define (add-used! inst) 
	(set! used-instructions 
	  (add-sorted-set inst 
			  (lambda (element) (position-of element instruction-set)) 
			  used-instructions)) 
	dispatch)
      (define (add-entry! name) (set! entry-points (add-set name entry-points)) dispatch)
      (define (add-stacked! name) (set! stacked (add-set name stacked)) dispatch)
      (define (add-assignment! name inst) (let ((pair (assoc name assignments)))
					    (if pair (set-cdr! pair (add-set inst (cdr pair)))
					      (set! assignments (cons (cons name (list inst)) assignments))) dispatch))
      (define (add-label-trace! name counter) (set! labels (cons (cons counter name) labels)) dispatch)
      (define (allocate-register name)
	(if (assoc name register-table)
	  (error "Multiply defined register: " name)
	  (set! register-table
	    (cons (list name (make-register name))
		  register-table)))
	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val (cadr val)
	    (error "Unknown register:" name))))
      (define (reset-counter!) (set! instruction-counter 0))

      (define (trace-on!) (set! trace true) 'trace-on)
      (define (trace-off!) (set! trace false) 'trace-off)
      
      (define (set-breakpoint! label n)
	(let ((pair (assoc-cdr label labels)))
	  (if pair
	    (begin (set! breakpoints (add-sorted-set (cons (+ (car pair) n) (cons (cdr pair) n)) (lambda (element) (car element)) breakpoints)) "breakpoint set")
	    (error "Unknown label -- Set-Breakpoint" label))))


      (define (cancel-breakpoint! label n)
	(define (work current)
	  (cond 
	    ((null? current) '())
	    ((and (eq? label (cadar current)) (eq? n (cddar current))) (cdr current))
	    (else  (cons (car current) (work (cdr current))))))
	(set! breakpoints (work breakpoints)) dispatch)

      (define (cancel-all-breakpoints) (set! breakpoints '()) dispatch)
      
      (define (display-line insts)
	(let ((label (assoc (- (instruction-number (car insts)) 1) labels)))
	  (if label (begin (display (cdr label)) (newline))))
	(display (list instruction-counter ":" (instruction-text (car insts))))	 (newline))

      (define (execute-debug proceed)
	(set! instruction-counter (+ 1 instruction-counter))
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	    'done
	    (begin
	      (if trace (display-line insts))
	      (if proceed (begin ((instruction-execution-proc (car insts))) (execute-debug false)))
	      (let ((breakpoint (assoc (modulo instruction-counter instruction-count) breakpoints)))
		(if breakpoint (begin (newline) (display (cadr breakpoint)) (display " : ") (display (cddr breakpoint)) (newline))
		 (begin ((instruction-execution-proc (car insts))) (execute-debug false))))))))

      (define (execute-debugless)
	(set! instruction-counter (+ 1 instruction-counter))
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	    'done
	    (begin
	      (if trace (display-line insts))
		 ((instruction-execution-proc (car insts)))
		 (execute-debugless)))))
      
      (define (execute)
	(if (null? breakpoints)
	  (execute-debugless)
	  (execute-debug false)))

      (define (get-trace-setting name type) ((lookup-register name) type))
      (define (dispatch message)
	(cond 
	  ((eq? message 'start)
	   (set-contents! pc the-instruction-sequence)
	   (reset-counter!)
	   (execute))
	  ((eq? message 'proceed) (if (or (null? (get-contents pc)) (eq? (get-contents pc) '*unassigned*)) (error "CANNOT PROCEED WHEN NOT RUN OR TERMINATED") (execute-debug true)))
	  ((eq? message 'install-instruction-sequence) (lambda (seq) (set! the-instruction-sequence seq) (set! instruction-count (length seq))))
	  ((eq? message 'allocate-register) allocate-register)
	  ((eq? message 'get-register) lookup-register)
	  ((eq? message 'install-operations) (lambda (ops) (set! the-ops (append the-ops ops))))
	  ((eq? message 'stack) stack)
	  ((eq? message 'operations) the-ops)
	  ((eq? message 'registers) register-table)
	  ((eq? message 'has-register?) (lambda (name) (if (assoc name register-table) true false)))
	  ((eq? message 'add-used!) add-used!)
	  ((eq? message 'add-entry!) add-entry!)
	  ((eq? message 'add-stacked!) add-stacked!)
	  ((eq? message 'add-assignment!) add-assignment!)
	  ((eq? message 'add-label-trace!) add-label-trace!)
	  ((eq? message 'labels) labels)
	  ((eq? message 'trace-on!) (trace-on!))
	  ((eq? message 'trace-off!) (trace-off!))
	  ((eq? message 'register-trace-on!) (lambda (name) (get-trace-setting name 'trace-on!)))
	  ((eq? message 'register-trace-off!) (lambda (name) (get-trace-setting name 'trace-off!)))
	  ((eq? message 'assignments) assignments)
	  ((eq? message 'stacked) stacked)
	  ((eq? message 'instruction-counter) instruction-counter)
	  ((eq? message 'entry-points) entry-points)
	  ((eq? message 'breakpoints) breakpoints)
	  ((eq? message 'set-breakpoint!) set-breakpoint!)
	  ((eq? message 'cancel-breakpoint!) cancel-breakpoint!)
	  ((eq? message 'cancel-all-breakpoints) cancel-all-breakpoints)
	  ((eq? message 'used-instructions) used-instructions)
	  ((eq? message 'instruction-sequence) the-instruction-sequence)
		((eq? message 'print-stack-statistics) (stack 'print-stack-statistics))
	  (else (error "Unknown request -- MACHINE" message))))
dispatch)))

(define (assoc-cdr element table)
  (cond 
    ((null? table) false)
    ((eq? element (cdar table)) (car table))
    (else (assoc-cdr element (cdr table)))))

(define (start machine) (machine 'start))
(define (proceed machine) (machine 'proceed))
(define (print-stack-statistics machine) (machine 'print-stack-statistics))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts labels machine)
		    insts)
		  machine 1
		  ))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each
      (lambda (inst)
	(set-instruction-execution-proc!
	  inst
	  (make-execution-procedure (instruction-text inst) labels machine pc flag stack ops)))
      insts)))


(define (extract-labels text receive machine counter)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
		    (lambda (insts labels)
		      (let ((next-inst (car text)))
			(if (symbol? next-inst)
			  (begin (add-label-trace! machine next-inst counter) (receive insts (update-labels-if-possible (make-label-entry next-inst insts) labels)))
			  (receive (cons (make-instruction next-inst counter)
					 insts)
				   labels)))) machine (+ 1 counter))))

(define instruction-set '(assign branch goto perform restore save test))

(define (update-labels-if-possible label-entry labels)
  (if (assoc (car label-entry) labels)
    (error "ALREADY DEFINED LABEL -- ASSEMBLE" (car label-entry))
    (cons label-entry labels)))

(define (make-instruction text counter)
  (list text counter))
(define (instruction-text inst)
  (car inst))
(define (instruction-number inst)
	(cadr inst))
(define (instruction-execution-proc inst)
  (cddr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! (cdr inst) proc))

(define (make-label-entry label-name insts) (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (if (memq (car inst) instruction-set)
    (add-used-instruction! machine (car inst)))
  (cond 
    ((eq? (car inst) 'assign)
     (make-assign inst machine labels ops pc))
    ((eq? (car inst) 'test)
     (make-test inst machine labels ops flag pc))
    ((eq? (car inst) 'branch)
     (make-branch inst machine labels flag pc))
    ((eq? (car inst) 'goto)
     (make-goto inst machine labels pc))
    ((eq? (car inst) 'save)
     (make-save inst machine stack pc))
    ((eq? (car inst) 'restore)
     (make-restore inst machine stack pc))
    ((eq? (car inst) 'perform)
     (make-perform inst machine labels ops pc))
    (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (add-used-instruction! machine instruction)
  ((machine 'add-used!) instruction))
(define (add-entry-point! machine name)
  ((machine 'add-entry!) name))
(define (add-stacked! machine name)
  ((machine 'add-stacked!) name))
(define (add-assignment! machine name exp)
  ((machine 'add-assignment!) name exp))
(define (add-register! machine name)
  (if (not ((machine 'has-register?) name)) ((machine 'allocate-register) name)))
(define (add-label-trace! machine label counter)
  ((machine 'add-label-trace!) label counter))


(define (add-set element set) (if (memq element set) set (cons element set)))
(define (add-sorted-set element pred set) 
  (cond 
    ((null? set) (list element))
    ((< (pred element) (pred (car set))) (cons element set))
    ((= (pred element) (pred (car set))) (if (equal? element (car set)) set (cons element set)))
    ((> (pred element) (pred (car set))) (cons (car set) (add-sorted-set element pred (cdr set))))))

(define (make-assign inst machine labels operations pc)
  (let ((name (assign-reg-name inst)))
        (add-register! machine name)
  (let 
    ((target (get-register machine name))
     (value-exp (assign-value-exp inst)))
    (add-assignment! machine name value-exp)
    (let ((value-proc
	    (if (operation-exp? value-exp)
	      (make-operation-exp value-exp machine labels operations)
	      (make-primitive-exp (car value-exp) machine labels))))
      (lambda () ; execution procedure for assign
	(set-contents! target (value-proc))
	(advance-pc pc))))))

(define (assign-reg-name assign-instruction) (cadr assign-instruction))
(define (assign-value-exp assign-instruction) (cddr assign-instruction))

(define (advance-pc pc) (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      (let ((condition-proc (make-operation-exp condition machine labels operations)))
	(lambda ()
	  (set-contents! flag (condition-proc))
	  (advance-pc pc)))
      (error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
      (let ((insts (lookup-label labels (label-exp-label dest))))
	(lambda ()
	  (if (get-contents flag)
	    (set-contents! pc insts)
	    (advance-pc pc))))
      (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
(cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond 
      ((label-exp? dest)
       (let ((insts (lookup-label labels (label-exp-label dest))))
	 (lambda () (set-contents! pc insts))))
      ((register-exp? dest)
       (let ((name (register-exp-reg dest)))
	 (add-entry-point! machine name)
       (let ((reg (get-register machine name)))
	 (lambda ()
	   (set-contents! pc (get-contents reg))))))
      (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
(cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((name (stack-inst-reg-name inst)))
    (add-stacked! machine name)
  (let ((reg (get-register machine name)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((name (stack-inst-reg-name inst)))
    (add-stacked! machine name)
  (let ((reg (get-register machine name)))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc)))))
(define (stack-inst-reg-name stack-instruction)
(cadr stack-instruction))

(define (make-save-table inst machine stack pc)
  (let ((name (stack-inst-reg-name inst)))
  (let ((reg (get-register machine name)))
    (lambda ()
      (push-table-strict stack name (get-contents reg))
      (advance-pc pc)))))

(define (make-restore-table inst machine stack pc)
  (let ((name (stack-inst-reg-name inst)))
  (let ((reg (get-register machine name)))
    (lambda ()
      (set-contents! reg (pop-table-strict stack name inst))
      (advance-pc pc)))))
(define (stack-inst-reg-name stack-instruction)
(cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
      (let ((action-proc (make-operation-exp action machine labels operations)))
	(lambda ()
	  (action-proc)
	  (advance-pc pc)))
      (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond 
    ((constant-exp? exp) (let ((c (constant-exp-value exp))) (lambda () c)))
    ((label-exp? exp) (let ((insts (lookup-label labels (label-exp-label exp))))
			(lambda () insts)))
    ((register-exp? exp) (add-register! machine (register-exp-reg exp)) (let ((r (get-register machine (register-exp-reg exp))))
			   (lambda () (get-contents r))))
    (else (error "Unknown expression type -- ASSEMBLE" exp))))

(define (make-primitive-exp-bootstrapped exp machine labels)
  (cond 
    ((constant-exp? exp) (let ((c (constant-exp-value exp))) (lambda () c)))
    ((label-exp? exp) (let ((insts (lookup-label labels (label-exp-label exp))))
			(lambda () insts)))
    ((register-exp? exp) (let ((r (get-register machine (register-exp-reg exp))))
			   (lambda () (get-contents r))))
    (else (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs (if (any? label-exp? (operation-exp-operands exp)) (error "LABEL IN OPERATION -- ASSEMBLE" exp) 
		  (map (lambda (e) (make-primitive-exp e machine labels))
		     (operation-exp-operands exp)))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp) (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp) (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp) (cdr operation-exp))

(define (contains? l e) 
	(cond 
		((null? l) false)
		((eq? (car l) e) true)
		(else (contains? (cdr l) e))))
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "Unknown operation -- ASSEMBLE" symbol))))

(define (any? pred l)
  (if (null? l) false
    (or (pred (car l)) (any? pred (cdr l)))))

(define (all? pred l)
	(if (null? l) true) 
		(and (pred (car l)) (all?? pred (cdr l))))

(define (gcd-machine)
  (make-machine	(list (list 'rem remainder) (list '= =))
		'( (assign a (const 16932))
		   (assign b (const 1002))
		   test-b 
		   (test (op =) (reg b) (const 0))
			 (branch (label gcd-done))
			 (assign t (op rem) (reg a) (reg b))
			 (assign a (reg b))
			 (assign b (reg t))
			 (goto (label test-b))
			 gcd-done)))

(define (fact-machine)
  (make-machine (list (list '= =) (list '* *) (list '- -) (list 'read read) (list 'print (lambda (x) (display x) (newline))))
		'(controller
		   (assign n (op read))
		   (test (op =) (reg n) (const 0))
		   (branch (label end))
		   (perform (op initialize-stack))
		   (assign continue (label end-fact))

		   start-fact
		   (test (op =) (reg n) (const 1))
		   (branch (label base))
		   (save n)
		   (assign n (op -) (reg n) (const 1))
		   (save continue)
		   (assign continue (label after-base))
		   (goto (label start-fact))

		   after-base
		   (restore continue)
		   (restore n)
		   (assign val (op *) (reg n) (reg val))
		   (goto (reg continue))

		   base
		   (assign val (const 1))
		   (goto (reg continue))
		   	
		   end-fact
		   (perform (op print) (reg val))
		   (goto (label controller))
		   end
		   
		   )))

(define (count-leaves-machine)
  (make-machine (list (list 'null? null?) (list 'pair? pair?) (list '+ +) (list 'car car) (list 'cdr cdr)  (list 'print (lambda (x) (display x) (newline))) )
		'(controller
		   (assign val (const 0))
		   (assign continue (label end))
		   
		   cond-test
		   (test (op null?) (reg tree))
		   (branch (label is-null))
		   (test (op pair?) (reg tree))
		   (branch (label is-pair))
		   (assign val (const 1))
		   (goto (reg continue))

		   is-pair
		   (save tree)
		   (assign tree (op car) (reg tree))
		   (save continue)
		   (assign continue (label counted-car))
		   (goto (label cond-test))
		   
		   counted-car
		   (restore continue)
		   (restore tree)
		   (save tree)
		   (assign tree (op cdr) (reg tree))
		   (save val)
		   (save continue)
		   (assign continue (label counted-cdr))
		   (goto (label cond-test))

		   counted-cdr
		   (restore continue)
		   (restore tree)
		   (assign val (op +) (reg tree) (reg val))
		   (restore tree)
		   (goto (reg continue))

		   is-null
		   (assign val (const 0))
		   (goto (reg continue))
		   end	   
		   )))

(define (count-leaves-machine-iter)
  (make-machine (list (list 'null? null?) (list 'pair? pair?) (list '+ +) (list 'car car) (list 'cdr cdr)  (list 'print (lambda (x) (display x) (newline))) )
		'(controller
		   (assign val (const 0))
		   (assign continue (label end))
		   cond-test
		   (test (op null?) (reg tree))
		   (branch (label is-null))

		   (test (op pair?) (reg tree))
		   (branch (label is-pair))
		   (assign val (op +) (reg val) (const 1))
		   (goto (reg continue))

		   is-pair
		   (save continue)
		   (assign continue (label counted-car))
		   (save tree)
		   (assign tree (op car) (reg tree))
		   (goto (label cond-test))
		   
		   counted-car
		   (restore tree)
		   (assign continue (label counted-cdr))
		   (assign tree (op cdr) (reg tree))
		   (goto (label cond-test))

		   counted-cdr
		   (restore continue)
		   (goto (reg continue))

		   is-null
		   (goto (reg continue))
		   end	   
		   )))


(define (append-machine)
  (make-machine (list (list 'null? null?) (list 'pair? pair?) (list '+ +) (list 'car car) (list 'cdr cdr) (list 'cons cons)  (list 'print (lambda (x) (display x) (newline))) )
		'(controller
		   (assign continue (label end))
		   
		   test-cond
		   (test (op null?) (reg a))
		   (branch (label is-null))
		   (assign c (op car) (reg a))
		   (save c)
		   (save continue)
		   (assign continue (label got-cdr))
		   (assign a (op cdr) (reg a))
		   (goto (label test-cond))

		   got-cdr
		   (restore continue)
		   (restore c)
		   (assign value (op cons) (reg c) (reg value))
		   (goto (reg continue))

		   is-null
		   (assign value (reg b))
		   (goto (reg continue))
		   end	   
		   )))

(define (append!-machine)
  (make-machine (list (list 'null? null?) (list 'null-cdr? (lambda (x) (null? (cdr x)))) 
		      (list 'pair? pair?) (list '+ +) (list 'car car) (list 'cdr cdr) (list 'cons cons) (list 'set-cdr! set-cdr!) (list 'print (lambda (x) (display x) (newline))) )
		'(controller
		   (assign continue (label end))
		   
		   test-cond
		   (test (op null-cdr?) (reg a))
		   (branch (label is-null))
		   (save a)
		   (save continue)
		   (assign continue (label got-cdr))
		   (assign a (op cdr) (reg a))
		   (goto (label test-cond))

		   got-cdr
		   (restore continue)
		   (restore a)
		   (goto (reg continue))

		   is-null
		   (perform (op set-cdr!) (reg a) (reg b))
		   (goto (reg continue))
		   end	   
		   )))


(define (position-of element l)
  (define (work current index)
    (cond 
      ((null? current) false)
      ((eq? (car current) element) index)
      (else (work (cdr current) (+ 1 index)))))
  (work l 0))

(define (append-x x y)
  (if (null? x) y (cons x (append (cdr x) y))))


(define mach (append!-machine))
(set-register-contents! mach 'a '(2 4))
(set-register-contents! mach 'b '(3 5))

(define (adjoin-arg arg arglist) (append arglist (list arg)))


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

(define (pow x y) (if (= y 0) 1 (* x (pow x (- y 1)))))

(define (get-global-environment) the-global-environment)
(define (empty-arglist) '())

(define primitive-procedures
  (list 
    (list '- -)								(list 'close-input-port close-input-port) 
    (list '* *)								(list 'string-length string-length)
    (list '+ +)		  					(list 'string->symbol string->symbol)
    (list '< <)								(list 'list list)	
		(list '/ /) 							(list 'pair? pair?)	
		(list 'cdr cdr)						(list 'memq memq)	
		(list 'cons cons)					(list 'open-input-file open-input-file)
    (list '>= >=)						  (list 'symbol->string symbol->string)	
    (list 'car car) 					(list 'eof-object? eof-object?)		
    (list 'null? null?)				(list 'get-global-environment get-global-environment)
    (list 'modulo modulo)			(list 'adjoin-arg adjoin-arg)
    (list 'reverse reverse)		(list 'quit exit)			
    (list 'pow pow) 					(list 'exit exit) 				
    (list '= =)								(list 'import import)			
    (list 'string? string?)		(list 'symbol? symbol?)		
    (list 'error error)				(list 'newline newline)		
    (list 'list-ref list-ref)	(list 'caadr caadr)				
    (list 'filter filter)			(list 'length length)			
    (list 'set-car! set-car!)	(list 'set-cdr! set-cdr!)	
    (list 'even? even?)				(list 'member member)
    (list 'abs abs)						(list '> >)		
    (list 'assoc assoc)				(list 'equal? equal?)	
    (list 'string=? string=?) (list 'substring substring)
    (list 'not not)						(list 'read read)					
  	(list 'number? number?)		(list 'empty-arglist empty-arglist)	
		(list 'display display)		(list 'last-operand? (lambda (ops) (null? (cdr ops))))
    (list 'append append)			(list 'caddr caddr)		
    (list 'cadr cadr)					(list 'cddr cddr)		
    (list 'eq? eq?)						(list 'odd? odd?)		
    (list '<= <=)							(list 'print-stack-statistics (lambda () (print-stack-statistics eceval)))		
    ))

(define (get-function symbol) (primitive-eval symbol))

(define (import-to-primitives file-name)
  (define port (open-input-file file-name))
  (define (work) 
   (let ((line (read port)))
     (if (eof-object? line)
		 			'done
					(begin
						;(display line) (newline)
						(primitive-eval line)
						(if (pair? (cadr line)) 
						(let ((name (caadr line)))
						(set! primitive-procedures (cons (list name (get-function name)) primitive-procedures)))) ;(display number) (display "\t") (display line) (newline)  
									(work)))))
  (display (work))
  (close-input-port port))
	
(import-to-primitives "evaluator.base.scm")

(define the-global-environment (setup-environment))
(define eceval
	  (make-machine primitive-procedures
	      '(
		read-eval-print-loop
		(perform (op initialize-stack))
		(perform (op prompt-for-input) (const ";;; EC-Eval input:"))
		(assign exp (op read))
		(assign env (op get-global-environment))
		(assign continue (label print-result))
		(goto (label eval-dispatch))
		
		
		print-result
		(perform (op print-stack-statistics))
		(perform (op announce-output) (const ";;; EC-Eval value:"))
		(perform (op user-print) (reg val))
		(goto (label read-eval-print-loop))

		eval-dispatch
		(test (op self-evaluating?) (reg exp))
		(branch (label ev-self-eval))
		(test (op variable?) (reg exp))
		(branch (label ev-variable))
		(test (op quoted?) (reg exp))
		(branch (label ev-quoted))
		(test (op assignment?) (reg exp))
		(branch (label ev-assignment))
		(test (op definition?) (reg exp))
		(branch (label ev-definition))
		(test (op if?) (reg exp))
		(branch (label ev-if))
		(test (op lambda?) (reg exp))
		(branch (label ev-lambda))
		(test (op begin?) (reg exp))
		(branch (label ev-begin))
		(test (op and?) (reg exp))
		(branch (label ev-and))
		(test (op or?) (reg exp))
		(branch (label ev-or))
		(test (op cond?) (reg exp))
		(branch (label ev-cond))
		(test (op let?) (reg exp))
		(branch (label ev-let))
		(test (op application?) (reg exp))
		(branch (label ev-application))
		(goto (label unknown-expression-type))

		ev-self-eval
		(assign val (reg exp))
		(goto (reg continue))

		ev-variable
		(assign val (op lookup-variable-value-base) (reg exp) (reg env))
		(goto (reg continue))

		ev-quoted
		(assign val (op text-of-quotation) (reg exp))
		(goto (reg continue))

		ev-lambda
		(assign unev (op lambda-parameters) (reg exp))
		(assign exp (op lambda-body) (reg exp))
		(assign val (op make-procedure) (reg unev) (reg exp) (reg env))
		(goto (reg continue))

		ev-application
		(save continue)
		(save env)
		(assign unev (op operands) (reg exp))
		(save unev)
		(assign exp (op operator) (reg exp))
		(assign continue (label ev-appl-did-operator))
		(goto (label eval-dispatch))

		ev-appl-did-operator
		(restore unev) ; the operands
		(restore env)
		(assign argl (op empty-arglist))
		(assign proc (reg val)) ; the operator
		(test (op no-operands?) (reg unev))
		(branch (label apply-dispatch))
		(save proc)

		ev-appl-operand-loop
		(save argl)
		(assign exp (op first-operand) (reg unev))
		(test (op last-operand?) (reg unev))
		(branch (label ev-appl-last-arg))
		(save env)
		(save unev)
		(assign continue (label ev-appl-accumulate-arg))
		(goto (label eval-dispatch))

		ev-appl-accumulate-arg
		(restore unev)
		(restore env)
		(restore argl)
		(assign argl (op adjoin-arg) (reg val) (reg argl))
		(assign unev (op rest-operands) (reg unev))
		(goto (label ev-appl-operand-loop))

		ev-appl-last-arg
		(assign continue (label ev-appl-accum-last-arg))
		(goto (label eval-dispatch))

		ev-appl-accum-last-arg
		(restore argl)
		(assign argl (op adjoin-arg) (reg val) (reg argl))
		(restore proc)
		(goto (label apply-dispatch))

		ev-appl-did-operator-lazy
		(restore unev)
		(restore env)
		(assign argl (reg unev))
		(assign proc (reg val))
		(goto (label apply-dispatch))

		apply-dispatch
		(test (op primitive-procedure?) (reg proc))
		(branch (label primitive-apply))
		(test (op compound-procedure?) (reg proc))
		(branch (label compound-apply))
		(goto (label unknown-procedure-type))

		primitive-apply
		(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
		(restore continue)
		(goto (reg continue))

		compound-apply
		(assign unev (op procedure-parameters) (reg proc))
		(assign env (op procedure-environment) (reg proc))
		(assign env (op extend-environment-base) (reg unev) (reg argl) (reg env))
		(assign unev (op procedure-body) (reg proc))
		(goto (label ev-sequence))

		ev-begin
		(assign unev (op begin-actions) (reg exp))
		(save continue)
		(goto (label ev-sequence))

		ev-sequence
		(assign exp (op first-exp) (reg unev))
		(test (op last-exp?) (reg unev))
		(branch (label ev-sequence-last-exp))
		(save unev)
		(save env)
		(assign continue (label ev-sequence-continue))
		(goto (label eval-dispatch))

		ev-sequence-continue
		(restore env)
		(restore unev)
		(assign unev (op rest-exps) (reg unev))
		(goto (label ev-sequence))

		ev-sequence-last-exp
		(restore continue)
		(goto (label eval-dispatch))

		ev-if
		(save exp) ; save expression for later
		(save env)
		(save continue)
		(assign continue (label ev-if-decide))
		(assign exp (op if-predicate) (reg exp))
		(goto (label eval-dispatch)) ; evaluate the predicate

		ev-if-decide
		(restore continue)
		(restore env)
		(restore exp)
		(test (op true?) (reg val))
		(branch (label ev-if-consequent))

		ev-if-alternative
		(assign exp (op if-alternative) (reg exp))
		(goto (label eval-dispatch))

		ev-if-consequent
		(assign exp (op if-consequent) (reg exp))
		(goto (label eval-dispatch))

		ev-assignment
		(assign unev (op assignment-variable) (reg exp))
		(save unev) ; save variable for later
		(assign exp (op assignment-value) (reg exp))
		(save env)
		(save continue)
		(assign continue (label ev-assignment-1))
		(goto (label eval-dispatch)) ; evaluate the assignment value

		ev-assignment-1
		(restore continue)
		(restore env)
		(restore unev)
		(perform (op set-variable-value!-base) (reg unev) (reg val) (reg env))
		(assign val (const ok))
		(goto (reg continue))

		ev-definition
		(assign unev (op definition-variable) (reg exp))
		(save unev) ; save variable for later
		(assign exp (op definition-value) (reg exp))
		(save env)
		(save continue)
		(assign continue (label ev-definition-1))
		(goto (label eval-dispatch)) ; evaluate the definition value

		ev-definition-1
		(restore continue)
		(restore env)
		(restore unev)
		(perform (op define-variable!) (reg unev) (reg val) (reg env))
		(assign val (const ok))
		(goto (reg continue))

		ev-and
		(assign unev (op operands) (reg exp))
		(save continue)
		(save unev)
		(assign exp (op first-operand) (reg unev))
		(assign continue (label ev-and-loop))
		(goto (label eval-dispatch))

		ev-and-loop
		(restore unev)
		(test (op false?) (reg val))
		(branch (label short-circuit))
		(test (op null?) (reg unev))
		(branch (label end-loop))
		(assign unev (op rest-operands) (reg unev))
		(save unev)
		(assign exp (op first-operand) (reg unev))
		(goto (label eval-dispatch))

		ev-or
		(assign unev (op operands) (reg exp))
		(save continue)
		(save unev)
		(assign exp (op first-operand) (reg unev))
		(assign continue (label ev-or-loop))
		(goto (label eval-dispatch))

		ev-or-loop
		(restore unev)
		(test (op true?) (reg val))
		(branch (label short-circuit))
		(test (op null?) (reg unev))
		(branch (label end-loop))
		(assign unev (op rest-operands) (reg unev))
		(save unev)
		(assign exp (op first-operand) (reg unev))
		(goto (label eval-dispatch))

		end-loop
		short-circuit
		(restore continue)
		(goto (reg continue))

		ev-cond
		(assign unev (op operands) (reg exp))
		(save continue)
		(save unev)
		(assign exp (op first-operand) (reg unev))
		(assign exp (op cond-predicate) (reg exp))
		(assign continue (label ev-pred))
		(goto (label eval-dispatch))

		ev-pred
		(test (op true?) (reg val))
		(branch (label pred-true))
		(restore unev)
		(assign unev (op rest-operands) (reg unev))
		(save unev)
		(assign exp (op first-operand) (reg unev))
		(assign exp (op cond-predicate) (reg exp))
		(goto (label eval-dispatch))

		pred-true
		(restore unev)
		(assign exp (op first-operand) (reg unev))
		(assign exp (op rest-operands) (reg exp))
		(restore continue)
		(goto (label eval-dispatch))

		ev-let
		(assign proc (op let-pairs) (reg exp))
		(goto (label seperate-lists))

		seperated-lists
		(assign proc (op make-lambda) (reg argl) (reg proc))
		(assign exp (op cons) (reg proc) (reg unev))
		(goto (label eval-dispatch))

		seperate-lists
		(test (op null?) (reg proc))
		(branch (label seperated-lists)) 
		(assign val (op let-parameter) (reg proc))
		(assign argl (op adjoin-arg) (reg val) (reg argl))
		(assign val (op let-expression) (reg proc))
		(assign unev (op adjoin-arg) (reg val) (reg unev))
		(assign proc (op cdr) (reg proc))
		(goto (label seperate-lists))

		unknown-expression-type
		(assign val (const 7))
		(goto (label signal-error))

		unknown-procedure-type
		(restore continue) ; clean up stack (from apply-dispatch)
		(assign val (const 8))
		(goto (label signal-error))

		signal-error
		(assign val (op get-error-text) (reg val))
		(perform (op user-print) (reg val))
		(goto (label read-eval-print-loop))
	      )))
