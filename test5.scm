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

(define mach (append!-machine))
(set-register-contents! mach 'a '(2 4))
(set-register-contents! mach 'b '(3 5))

(define (adjoin-arg arg arglist) (append arglist (list arg)))

(define (make-compiled-procedure entry env)
(list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
(tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (import file-name evaluator)
  (define port (open-input-file file-name))
  (define (work number) 
   (let ((line (read port)))
     (if (not (eof-object? line))
          (begin (evaluator line the-global-environment) ;(display number) (display "\t") (display line) (newline)  
		 (work (+ 1 number))))))
  (work 0) 
  (close-input-port port))

(define (pow x y) (if (= y 0) 1 (* x (pow x (- y 1)))))

(define (get-global-environment) the-global-environment)
(define (empty-arglist) '())

(define (div-checked x y) (if (= y 0) "zero-div-err" (/ x y)))
(define (car-checked element)  (if (not (pair? element)) "not-pair-car-err" (car element)))
(define (cdr-checked element) (if (not (pair? element))  "not-pair-cdr-err" (cdr element)))
(define (modulo-checked x y) (if (= y 0 ) "zero-mod-err" (modulo x y)))
(define (set-car!-checked x y) (if (not (pair? x)) "not-pair-set-car-err" (set-car! x y)))
(define (set-cdr!-checked x y) (if (not (pair? x)) "not-pair-set-cdr-err" (set-cdr! x y)))

(load "evaluator.base.scm")

(define primitive-procedures
  (list 
    (list '- -)								(list 'close-input-port close-input-port) 
    (list '* *)								(list 'string-length string-length)
    (list '+ +)		  					(list 'string->symbol string->symbol)
    (list '< <)								(list 'list list)	
		(list '/ div-checked) 		(list 'pair? pair?)	
		(list 'cdr cdr-checked)		(list 'memq memq)	
		(list 'cons cons)					(list 'open-input-file open-input-file)
    (list '>= >=)						  (list 'symbol->string symbol->string)	
    (list 'car car-checked) 	(list 'eof-object? eof-object?)		
    (list 'null? null?)				(list 'get-global-environment get-global-environment)
		(list 'quit exit)					(list 'adjoin-arg adjoin-arg)
	  (list 'modulo modulo-checked)
    (list 'reverse reverse)		(list '<= <=)		
    (list 'pow pow) 					(list 'odd? odd?)						
    (list '= =)								(list 'import import)			
    (list 'string? string?)		(list 'symbol? symbol?)		
    (list 'error error)				(list 'newline newline)		
    (list 'list-ref list-ref)	(list 'caadr caadr)				
    (list 'filter filter)			(list 'length length)			
    (list 'even? even?)				(list 'set-car! set-car!-checked)	
		(list 'member member)			(list 'set-cdr! set-cdr!-checked)	
    (list 'abs abs)						(list '> >)		
    (list 'assoc assoc)				(list 'equal? equal?)	
    (list 'string=? string=?) (list 'substring substring)
    (list 'not not)						(list 'read read)					
  	(list 'number? number?)		(list 'empty-arglist empty-arglist)	
		(list 'display display)		(list 'last-operand? (lambda (ops) (null? (cdr ops))))
    (list 'append append)			(list 'caddr caddr)		
    (list 'cadr cadr)					(list 'cddr cddr)		
    (list 'eq? eq?)						(list 'print-stack-statistics (lambda () (print-stack-statistics eceval)))
		(list 'make-compiled-procedure make-compiled-procedure)
		(list 'compiled-procedure? compiled-procedure?)
		(list 'compiled-procedure-entry compiled-procedure-entry)
    ))

(define (primitive? func) (if (assoc func primitive-procedures) true false))

(define (get-function name) (primitive-eval name))

(define (import-to-primitives file-name)
  (define port (open-input-file file-name))
  (define (work) 
   (let ((line (read port)))
     (if (eof-object? line)
		 "Imported Primitives\n"
					(begin
						;(display line) (newline)
						(if (pair? (cadr line)) 
						(let ((name (caadr line)))
						(set! primitive-procedures (cons (list name (get-function name)) primitive-procedures)))) ;(display number) (display "\t") (display line) (newline)  
									(work)))))
  (display (work))
  (close-input-port port))
	
(import-to-primitives "evaluator.base.scm")

(define the-global-environment (setup-environment))

(define (compile exp target linkage compile-time-env)
 (cond 
 ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
 ((quoted? exp) (compile-quoted exp target linkage))
 ((variable? exp) (compile-variable exp target linkage compile-time-env))
 ((assignment? exp) (compile-assignment exp target linkage compile-time-env))
 ((definition? exp) (compile-definition exp target linkage compile-time-env))
 ((if? exp) (compile-if exp target linkage compile-time-env))
 ((lambda? exp) (compile-lambda exp target linkage compile-time-env))
 ((begin? exp) (compile-sequence (begin-actions exp) target linkage compile-time-env))
 ((cond? exp) (compile-cond exp target linkage compile-time-env))
; ((open-coded-primitive? exp) (compile-open-primitive-2 exp target linkage compile-time-env))
 ((application? exp) (compile-application exp target linkage compile-time-env))
 (else (error "Unknown expression type -- COMPILE" exp))))

(define open-primitives '(+ / - * ))
(define (open-coded-primitive? exp) (contains? open-primitives (operator exp)))
(define (location var compile-time-env)
 (define (traverse-frames frame-counter env)
	(define (traverse-frame displacement-counter frame)
	 (cond 
			((null? frame) false)
			((eq? var (car frame)) (cons frame-counter displacement-counter))
			(else (traverse-frame (+ 1 displacement-counter) (cdr frame)))))
	(if (null? env) 
		'not-found
		(let ((result (traverse-frame 0 (car env))))
		 (if result result (traverse-frames (+ 1 frame-counter) (cdr env))))))
 (traverse-frames 0 compile-time-env))

(define (make-instruction-sequence needs modifies statements) (list needs modifies statements))

(define (compile-linkage linkage)
 (cond 
 ((eq? linkage 'return) (make-instruction-sequence '(continue) '() '((goto (reg continue)))))
 ((eq? linkage 'next) (empty-instruction-sequence))
 (else (make-instruction-sequence '() '() `((goto (label ,linkage)))))))
 
(define (end-with-linkage linkage instruction-sequence)
 (preserving '(continue) instruction-sequence (compile-linkage linkage)))
(define (compile-self-evaluating exp target linkage)
 (end-with-linkage linkage 
 (make-instruction-sequence '() (list target) `((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage)
 (end-with-linkage linkage
 (make-instruction-sequence '() (list target) `((assign ,target (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage compile-time-env)
 (define compile-time-address (location exp compile-time-env))
 (end-with-linkage linkage
 (make-instruction-sequence '(env) (list target)
	(if (eq? compile-time-address 'not-found)
	 `((save env) 
	   (assign (reg env) (op get-global-environment))	 
		 (assign ,target (op lookup-variable-value) (const ,exp) (reg env))
		 (restore env)))))
	 `((assign ,target (op lookup-lexical-address) (const ,compile-time-address) (reg env))))
 
(define (compile-assignment exp target linkage compile-time-env)
 (let ((var (assignment-variable exp))
 (define (compile-time-address (location var compile-time-env)))
 (get-value-code
 (compile (assignment-value exp) 'val 'next compile-time-env)))
 (end-with-linkage linkage 
 (preserving '(env) get-value-code
 (make-instruction-sequence '(env val) (list target)
 (cons
	(if (eq? compile-time-address 'not-found)
	 `((save env) 
		 (assign (reg env) (op get-global-environment)) 
		 (perform (op set-variable-value!) (const ,var) (reg val) (reg env))
		 (restore env)) 
	 `((perform (op lexical-address-set!) (const ,var) (reg compile-time-address) (reg env)))) 
	   `(assign ,target (const ok))))))))

(define (compile-definition exp target linkage compile-time-env)
 (let 
 ((var (definition-variable exp))
	(get-value-code (compile (definition-value exp) 'val 'next compile-time-env)))
 (end-with-linkage linkage 
 (preserving '(env) get-value-code
 (make-instruction-sequence '(env val) (list target)
 `((perform (op define-variable!)
 (const ,var)
 (reg val)
 (reg env))
 (assign ,target (const ok))))))))

(define label-counter 0)
(define (new-label-number) (set! label-counter (+ 1 label-counter)) label-counter)
(define (make-label name)
 (string->symbol
 (string-append (symbol->string name)
 (number->string (new-label-number)))))

(define (make-line symbol) (string-append (symbol->string symbol) "\n"))

(define (compile-if exp target linkage compile-time-env)
 (let (
	 (t-branch (make-label 'true-branch))
	 (f-branch (make-label 'false-branch))
	 (after-if (make-label 'after-if)))
	 (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
	 (let (
		 (p-code (compile (if-predicate exp) 'val 'next compile-time-env))
		 (c-code (compile (if-consequent exp) target consequent-linkage compile-time-env))
		 (a-code (compile (if-alternative exp) target linkage compile-time-env)))
		  (preserving '(env continue) p-code
			(append-instruction-sequences
			(make-instruction-sequence '(val) '() `((test (op false?) (reg val)) (branch (label ,f-branch))))
			(parallel-instruction-sequences
			(append-instruction-sequences t-branch c-code)
			(append-instruction-sequences f-branch a-code))
			after-if))))))

(define (compile-cond exp target linkage compile-time-env)
	(compile-if (cond->if exp) target linkage compile-time-env))


(define (compile-sequence seq target linkage compile-time-env)
 (if (last-exp? seq)
 (compile (first-exp seq) target linkage compile-time-env)
 (preserving '(env continue)
 (compile (first-exp seq) target 'next compile-time-env)
 (compile-sequence (rest-exps seq) target linkage))))


(define (make-compiled-procedure entry env) (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
 (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda exp target linkage compile-time-env)
 (let (
	 (proc-entry (make-label 'entry))
	 (after-lambda (make-label 'after-lambda)))
 (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
	 (append-instruction-sequences
		 (tack-on-instruction-sequence
			 (end-with-linkage lambda-linkage
				 (make-instruction-sequence '(env) (list target) 
					 `((assign ,target (op make-compiled-procedure) (label ,proc-entry) (reg env)))))
					 (compile-lambda-body exp proc-entry compile-time-env))
				after-lambda))))

(define (compile-lambda-body exp proc-entry compile-time-env)
 (let ((formals (lambda-parameters exp)))
 (append-instruction-sequences
	 (make-instruction-sequence '(env proc argl) '(env)
	 `(,proc-entry 
	 (assign env (op compiled-procedure-env) (reg proc))
	 (assign env (op extend-environment) (const ,formals) (reg argl) (reg env))))
	 (compile-sequence (lambda-body exp) 'val 'return (cons formals compile-time-env)))))

(define (compile-application exp target linkage compile-time-env)
  (let (
		(proc-code (compile (operator exp) 'proc 'next compile-time-env))
		(operand-codes (map (lambda (operand) (compile operand 'val 'next compile-time-env)) (operands exp))))
			(preserving '(env continue) proc-code
				(preserving 
					'(proc continue) 
					(construct-arglist operand-codes) 
					(compile-procedure-call target linkage compile-time-env)))))


(define (construct-arglist operand-codes)
 (let ((operand-codes (reverse operand-codes))) 
	(if (null? operand-codes) (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
	(let ((code-to-get-last-arg 
		(append-instruction-sequences (car operand-codes)
		(make-instruction-sequence '(val) '(argl) '((assign argl (op list) (reg val)))))))
		(if (null? (cdr operand-codes))
		code-to-get-last-arg
		(preserving '(env) code-to-get-last-arg (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
 (let (
	 (code-for-next-arg (preserving '(argl) (car operand-codes)
 (make-instruction-sequence '(val argl) '(argl) '((assign argl (op cons) (reg val) (reg argl)))))))
 (if (null? (cdr operand-codes))
 code-for-next-arg
 (preserving '(env) code-for-next-arg (code-to-get-rest-args (cdr operand-codes))))))

(define (construct-arglist-left operand-codes)
 (if (null? operand-codes) (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
 (let ((code-to-get-first-arg (append-instruction-sequences (car operand-codes) 
	(make-instrction-sequence '(val) '(argl) '((assign argl (op list) (reg val)))))))
	(if (null? (cdr operand-codes)) code-to-get-first-arg 
		(preserving '(env) code-to-get-first-arg (code-to-get-rest-args-left (cdr operand-codes)))))))

(define (code-to-get-rest-args-left operand-codes)
 (let ((code-for-next-arg (preserving '(argl) (car operand-codes)
  (make-instruction-sequence '(val argl) '(argl) '((assign argl (op append) (reg argl) (reg val)))))))
  (if (null? (cdr operand-codes))
   code-for-next-arg
   (preserving '(env) code-for-next-arg (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage compile-time-env)
	 (let (
		 (primitive-branch (make-label 'primitive-branch))
		 (compiled-branch (make-label 'compiled-branch))
		 (after-call (make-label 'after-call)))
		 (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
				 (append-instruction-sequences
					 (make-instruction-sequence '(proc) '() `((test (op primitive-procedure?) (reg proc)) (branch (label ,primitive-branch))))
					 (parallel-instruction-sequences
						 (append-instruction-sequences compiled-branch (compile-proc-appl target compiled-linkage compile-time-env))
						 (append-instruction-sequences primitive-branch (end-with-linkage linkage
							 (make-instruction-sequence '(proc argl) (list target)
							 `((assign ,target (op apply-primitive-procedure) (reg proc) (reg argl)))))))
							 after-call))))

(define (compile-proc-appl target linkage compile-time-env) 
	(cond 
		((and (eq? target 'val) (not (eq? linkage 'return)))
		(make-instruction-sequence '(proc) all-regs 
			`((assign continue (label ,linkage))
			(assign val (op compiled-procedure-entry) (reg proc))
			(goto (reg val)))))
		((and (not (eq? target 'val))(not (eq? linkage 'return)))
		(let ((proc-return (make-label 'proc-return)))
		(make-instruction-sequence '(proc) all-regs
			`((assign continue (label ,proc-return))
			(assign val (op compiled-procedure-entry) (reg proc))
			(goto (reg val))
			,proc-return
			(assign ,target (reg val))
			(goto (label ,linkage))))))
		((and (eq? target 'val) (eq? linkage 'return))
		(make-instruction-sequence '(proc continue) all-regs
			`((assign val (op compiled-procedure-entry)
			(reg proc))
			(goto (reg val)))))
		((and (not (eq? target 'val)) (eq? linkage 'return))
		(error "return linkage, target not val -- COMPILE" target))))


 (define (registers-needed s)
 (if (symbol? s) '() (car s)))
(define (registers-modified s)
 (if (symbol? s) '() (cadr s)))
(define (statements s)
 (if (symbol? s) (list s) (caddr s)))
 (define (needs-register? seq reg)
 (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
 (memq reg (registers-modified seq)))

(define (list-union s1 s2)
 (cond 
	 ((null? s1) s2) ((memq (car s1) s2) (list-union (cdr s1) s2))
	 (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
 (cond 
	 ((null? s1) '())
	 ((memq (car s1) s2) (list-difference (cdr s1) s2))
	 (else (cons (car s1) (list-difference (cdr s1) s2)))))

(define (append-instruction-sequences . seqs)
 (define (append-2-sequences seq1 seq2)
 (make-instruction-sequence
 (list-union (registers-needed seq1) (list-difference (registers-needed seq2) (registers-modified seq1)))
 (list-union (registers-modified seq1) (registers-modified seq2))
 (append (statements seq1) (statements seq2))))
 (define (append-seq-list seqs)
	(if (null? seqs)
	(empty-instruction-sequence)
  (append-2-sequences (car seqs) (append-seq-list (cdr seqs)))))
 (append-seq-list seqs))

(define (tack-on-instruction-sequence seq body-seq)
 (make-instruction-sequence
 (registers-needed seq)
 (registers-modified seq)
 (append (statements seq) (statements body-seq))))

(define (empty-instruction-sequence) (make-instruction-sequence '() '() '()))

(define (preserving regs seq1 seq2)
 (if (null? regs) (append-instruction-sequences seq1 seq2)
 (let ((first-reg (car regs)))
 (if (and (needs-register? seq2 first-reg) (modifies-register? seq1 first-reg))
 (preserving (cdr regs)
 (make-instruction-sequence
 (list-union (list first-reg) (registers-needed seq1)) 
 (list-difference (registers-modified seq1) (list first-reg))
 (append `((save ,first-reg)) (statements seq1) `((restore ,first-reg)))) 
 seq2)
 (preserving (cdr regs) seq1 seq2))))) 

 (define (parallel-instruction-sequences seq1 seq2)
 (make-instruction-sequence
 (list-union (registers-needed seq1) (registers-needed seq2))
 (list-union (registers-modified seq1) (registers-modified seq2))
 (append (statements seq1) (statements seq2))))


(define (compile-open-primitive-2 exp target linkage compile-time-env)
 (display exp) 
 (end-with-linkage linkage 
	(append-instruction-sequences
		(spread-arguments-2 (car (operands exp)) (cadr (operands exp))) 
			(make-instruction-sequence 
				'() 
			   (list target) 
			  `((assign ,target (op ,(operator exp)) (reg arg1) (reg arg2)))))))

(define (spread-arguments-2 op1 op2) 
	(preserving '(env arg1 arg2 val) (compile op1 'arg1 'next '()) (compile op2 'arg2 'next '())))


(define all-regs '(env proc val argl unev continue arg1 arg2) )

(define (compile-to code file)
 (define port (open-output-file file))
 (display (compile code 'val 'next '()) port)
 (close-output-port port))

;(compile-to '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n)))  "firstFac.txt")
;(compile-to '(define (factorial-alt n) (if (= n 1) 1 (* n (factorial-alt (- n 1))))) "secondFac.txt")
;(compile-to '(define (factorial n) (define (iter product counter) (if (> counter n) product (iter (* counter product) (+ counter 1)))) (iter 1 1)) "thirdFac.txt")

(define (make-machine-from-file file)
 (define port (open-input-file file))
 (define line (read port))
 (close-input-port port)
 (make-machine primitive-procedures (caddr line)))

;(define fac-machine-1 (make-machine-from-file "FirstFac.txt"))
(define eceval (make-machine-from-file "Eceval.txt"))

(define (make-lexical-address frame displacement) (cons frame displacement))
(define (frame lexical-address) (car lexical-address))
(define (displacement lexical-address) (cdr lexical-address))

(define (lookup-lexical-address lexical-address run-time-env)
 (define (traverse-frames counter env) 
	(if (eq? 0 counter) 
		(traverse-frame (displacement lexical-address) (car env)) 
		(traverse-frames (- counter 1) (cdr env)))) 
 (define (traverse-frame counter frame)
	(if (eq? 0 counter) 
		(if (eq? '*unassigned* (car run-time-env)) 
			(error "Unassigned variable" lexical-address) 
			(car frame))
		(traverse-frame (- counter 1) (cdr frame))))
	(traverse-frames (frame lexical-address) run-time-env)
	)

(define (lexical-address-set! value lexical-address run-time-env)
 (define (traverse-frames counter env) 
	(if (eq? 0 counter) 
		(traverse-frame (displacement lexical-address) (car env)) 
		(traverse-frames (- counter 1) (cdr env)))) 
 (define (traverse-frame counter frame)
	(if (eq? 0 counter) 
		(set! (car frame) value)
		(traverse-frame (- counter 1) (cdr frame))))
	(traverse-frames (frame lexical-address) run-time-env)
	)

(define (start-eceval)
 (set! the-global-environment (setup-environment))
 (set-register-contents! eceval 'flag false)
 (start eceval))

(define (compile-and-go expression)
 (let ((instructions (assemble (statements (compile expression 'val 'return '())) eceval)))
	(set! the-global-environment (setup-environment))
	(set-register-contents! eceval 'val instructions)
	(set-register-contents! eceval 'flag true)
	(start eceval)))

;(compile-and-go '(define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n))))
;(compile-and-go '(define (factorial-alt n) (if (= n 1) 1 (* n (factorial-alt (- n 1))))))
;(compile-and-go '(define (factorial n) (define (iter product counter) (if (> counter n) product (iter (* counter product) (+ counter 1)))) (iter 1 1)))
