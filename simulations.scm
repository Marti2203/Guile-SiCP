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
