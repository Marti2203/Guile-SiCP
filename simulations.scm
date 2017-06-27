(controller 
  start-fact
  (assign counter (const 0))
  (assign prod (const 1))
  (test (op >) (reg counter) (reg n))
  (branch (label end-fact))
  (assign prod (op *) (reg prod) (reg counter))
  (assign counter (op +) (reg counter) (const 1))
  (goto start-fact)
  end-fact)

(controller 
  (assign continue (label end-fact))
  
  start-fact
  (test (op =) (reg counter) (const 1))
  (branch (label base))
  (save n)
  (assign n (op -) (reg n) (const 1))
  (save continue)
  (assign continue (label after-base))
  (goto (label start-fact))
  
  after-base
  (restore n)
  (assign val (op *) (reg n) (reg val))
  (restore continue)
  (goto (reg continue))

  base
  (assign val (const 1))
  (goto (reg continue))
  end-fact)

(controller 
  start-fact
  (test (op >) (reg counter) (reg n))
  (branch (label end-fact))
  (assign new-prod (op *) (reg prod) (reg counter))
  (assign new-counter (op +) (reg counter) (const 1))
  (assign prod (reg new-prod))
  (assign counter (reg new-counter))
  (goto start-fact)
  end-fact)

(controller 
  start-fact
  (test (op >) (reg counter) (reg n))
  (branch (label end-fact))
  (assign prod (op *) (reg prod) (reg counter))
  (assign counter (op +) (reg counter) (const 1))
  (goto start-fact)
  end-fact)

(controller
  sqrt-start
  sqrt-iter-start
 (test (op good-enough?) (reg guess))
 (branch (label sqrt-done))
 (assign guess (op improve) (reg guess))
 (goto sqrt-iter-start)
 sqrt-done)

(controller
  sqrt-start
  sqrt-iter-start
 (test (op good-enough?) (reg guess))
 (branch (label sqrt-done))
 (assign guess (op improve) (reg guess))
 (goto sqrt-iter-start)
 sqrt-done)

(controller
  sqrt-start
  sqrt-iter-start
  (assign squared (op square) (reg guess))
  (assign diff (op -) (reg squared) (reg x))
  (assign absolute (op abs) (reg diff))
 (test (op <) (reg absolute)  (const 0.001))
 (branch (label sqrt-done))
 (assign div (op /) (reg x) (reg guess))
 (assign guess (op average) (reg guess) (reg div))
 (goto sqrt-iter-start)
 sqrt-done)

(controller
  sqrt-start
  sqrt-iter-start
  (assign squared (op square) (reg guess))
  (assign diff (op -) (reg squared) (reg x))
  (assign absolute (op abs) (reg diff))
 (test (op <) (reg absolute)  (const 0.001))
 (branch (label sqrt-done))
 (assign div (op /) (reg x) (reg guess))
 (assign sum (op +) (reg guess) (reg div))
 (assign average (op /) (reg sum) (const 2))
 (assign guess average)
 (goto sqrt-iter-start)
 sqrt-done)


(controller
  sqrt-start
  sqrt-iter-start
 (test (op <) (op abs) (op -) (op square) (reg guess) (reg x)  (const 0.001))
 (branch (label sqrt-done))
 (assign guess (op average) (reg guess) (op /) (reg x) (reg guess))
 (goto sqrt-iter-start)
 sqrt-done)


(controller
  (assign continue (label fact-done)) ; set up final return address
  fact-loop
  (test (op =) (reg n) (const 1))
  (branch (label base-case))
  ;; Set up for the recursive call by saving n and continue.
  ;; Set up continue so that the computation will continue
  ;; at after-fact when the subroutine returns.
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label fact-loop))
  after-fact
  (restore n)
  (restore continue)
  (assign val (op *) (reg n) (reg val)) ; val now contains n(n - 1)!
  (goto (reg continue)) ; return to caller
  base-case
  (assign val (const 1)) ; base case: 1! = 1
  (goto (reg continue)) ; return to caller
  fact-done)

(controller
  (assign continue (label fib-done))
  fib-loop
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))
  ;; set up to compute Fib(n - 1)
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n) ; save old value of n
  (assign n (op -) (reg n) (const 1)); clobber n to n - 1
  (goto (label fib-loop)) ; perform recursive call
  
  afterfib-n-1 ; upon return, val contains Fib(n - 1)
  (restore n)
  ;; set up to compute Fib(n - 2)
  (assign n (op -) (reg n) (const 2))
  (assign continue (label afterfib-n-2))
  (save val) ; save Fib(n - 1)
  (goto (label fib-loop))
  
  afterfib-n-2 ; upon return, val contains Fib(n - 2)
  (assign n (reg val)) ; n now contains Fib(n - 2)
  (restore val) ; val now contains Fib(n - 1)
  (restore continue)
  (assign val ; Fib(n - 1) + Fib(n - 2)
	  (op +) (reg val) (reg n))
  (goto (reg continue)) ; return to caller, answer is in val
  immediate-answer
  (assign val (reg n)) ; base case: Fib(n) = n
  (goto (reg continue))
  fib-done)

(controller
  (assign continue (label fib-done))
  fib-loop
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))
  ;; set up to compute Fib(n - 1)
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n) ; save old value of n
  (assign n (op -) (reg n) (const 1)); clobber n to n - 1
  (goto (label fib-loop)) ; perform recursive call
  
  afterfib-n-1 ; upon return, val contains Fib(n - 1)
  (restore n)
  ;; set up to compute Fib(n - 2)
  (assign n (op -) (reg n) (const 2))
  (assign continue (label afterfib-n-2))
  (save val) ; save Fib(n - 1)
  (goto (label fib-loop))
  
  afterfib-n-2 ; upon return, val contains Fib(n - 2)
  (restore n) ; n now contains Fib(n - 1)
  (restore continue)
  (assign val ; Fib(n - 1) + Fib(n - 2)
	  (op +) (reg val) (reg n))
  (goto (reg continue)) ; return to caller, answer is in val
  
  immediate-answer
  (assign val (reg n)) ; base case: Fib(n) = n
  (goto (reg continue))
  fib-done)

(controller
  (assign continue (label expt-done))
  expt-start
  (test (op =) (reg n) (const 0))
  (branch (label expt-base))
  (save continue)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label expt-after))
  (goto expt-start)
  expt-after
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))
  expt-base
  (assign val 1)
  (goto (reg continue))
  expt-done)

(controller
  (assign counter (reg n))
  (assign product (const 1))
  expt-iter-start
  (test (op =) (reg counter) (const 0))
  (branch (label exp-iter-done))
  (assign product (op *) (reg product) (reg b))
  (assign counter (op -) (reg counter) (const 1))
  (goto (label expt-iter-start))
  expt-iter-done)



begin-garbage-collection
(assign free (const 0))
(assign scan (const 0))
(assign old (reg root))
(assign relocate-continue (label reassign-root))
(goto (label relocate-old-result-in-new))

reassign-root
(assign root (reg new))
(goto (label gc-loop))


gc-loop
(test (op =) (reg scan) (reg free))
(branch (label gc-flip))
(assign old (op vector-ref) (reg new-cars) (reg scan))
(assign relocate-continue (label update-car))
(goto (label relocate-old-result-in-new))


update-car
(perform
(op vector-set!) (reg new-cars) (reg scan) (reg new))
(assign old (op vector-ref) (reg new-cdrs) (reg scan))
(assign relocate-continue (label update-cdr))
 (goto (label relocate-old-result-in-new))

update-cdr
(perform
(op vector-set!) (reg new-cdrs) (reg scan) (reg new))
(assign scan (op +) (reg scan) (const 1))
(goto (label gc-loop))


relocate-old-result-in-new
(test (op pointer-to-pair?) (reg old))
(branch (label pair))
(assign new (reg old))
(goto (reg relocate-continue))

pair
(assign oldcr (op vector-ref) (reg the-cars) (reg old))
(test (op broken-heart?) (reg oldcr))
(branch (label already-moved))
(assign new (reg free)) ; new location for pair
;; Update free pointer.
(assign free (op +) (reg free) (const 1))
;; Copy the car and cdr to new memory.
(perform (op vector-set!) (reg new-cars) (reg new) (reg oldcr))
(assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
(perform (op vector-set!) (reg new-cdrs) (reg new) (reg oldcr))
;; Construct the broken heart.
(perform (op vector-set!) (reg the-cars) (reg old) (const broken-heart))
(perform (op vector-set!) (reg the-cdrs) (reg old) (reg new))
(goto (reg relocate-continue))

already-moved
(assign new (op vector-ref) (reg the-cdrs) (reg old))
(goto (reg relocate-continue))


gc-flip
(assign temp (reg the-cdrs))
(assign the-cdrs (reg new-cdrs))
(assign new-cdrs (reg temp))
(assign temp (reg the-cars))
(assign the-cars (reg new-cars))
(assign new-cars (reg temp))


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
(assign val (op lookup-variable-value) (reg exp) (reg env))
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
(assign env (op extend-environment) (reg unev) (reg argl) (reg env))
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
(perform
(op set-variable-value!) (reg unev) (reg val) (reg env))
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
(perform
(op define-variable!) (reg unev) (reg val) (reg env))
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

read-eval-print-loop
(perform (op initialize-stack))
(perform
(op prompt-for-input) (const ";;; EC-Eval input:"))
(assign exp (op read))
 (assign env (op get-global-environment))
(assign continue (label print-result))
(goto (label eval-dispatch))
print-result
(perform
(op announce-output) (const ";;; EC-Eval value:"))
(perform (op user-print) (reg val))
(goto (label read-eval-print-loop))

unknown-expression-type
(assign val (const unknown-expression-type-error))
(goto (label signal-error))
unknown-procedure-type
(restore continue) ; clean up stack (from apply-dispatch)
(assign val (const unknown-procedure-type-error))
(goto (label signal-error))
signal-error
(perform (op user-print) (reg val))
(goto (label read-eval-print-loop))
