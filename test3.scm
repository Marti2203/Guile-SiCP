(define (square x) (* x x))
(define false #f)
(define true #t)
(define balance 100)
(define (withdraw amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
(define new-withdraw
	(let ((balance 100))
	(lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))))
(define (make-withdraw balance)
	(lambda (amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds")))
(define (make-account balance)
	(define (withdraw amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
	(define (deposit amount) (set! balance (+ balance amount)) balance)
	(define (interest percent) (set! balance (* (+ 1 percent) balance)) balance)
	(define (dispatch m)
	(cond 
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  ((eq? m 'interest) interest)
	  ((eq? m 'display ) balance)
	(else (error "Unknown request -- MAKE-ACCOUNT" m))))
dispatch)

(define (make-account-secured balance password)
  	(define counter 0)
	(define (withdraw amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
	(define (deposit amount) (set! balance (+ balance amount)) balance)
	(define (interest percent) (set! balance (* (+ 1 percent) balance)) balance)
	(define (dispatch m p)
	(if 
	  (eq? p password)
	 (begin 
	   (set! counter 0)
	   (cond 
	     ((eq? m 'withdraw) withdraw)
	     ((eq? m 'deposit) deposit)
	     ((eq? m 'interest) interest)
	     ((eq? m 'display ) balance)
	     ((eq? m 'check) #t)
	     (else (error "Unknown request -- MAKE-ACCOUNT" m))))
	 (begin (set! counter (+ 1 counter)) (if (> counter 7) call-the-police (error "Incorrect Password")))
	))
dispatch)

(define (make-joint acc pass newpass)
   (if (acc 'check pass)
     (lambda (m p) (if (eq? newpass p) (acc m pass) (error "Incorrect Joint Password")))
    (error "Incorrect Password")
  )
   )

(define (make-collector start op)
  (define (work input) (op start input)) 
 work)
(define (make-accumulator start) (make-collector start +))

(define (make-monitored op)
  (define calls 0)
  (define (dispatch m)
    (cond
      ((eq? m 'how-many-calls?) calls)
      (else (begin (set! calls (+ 1 calls)) (op m)))
      )
    )
  dispatch
  )
(define (estimate-pi-c trials) (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test) (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond 
      ((= trials-remaining 0) (/ trials-passed trials))
      ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else (iter (- trials-remaining 1) trials-passed)))) 
  (iter trials 0))

(define (estimate-pi trials) (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
	(cond 
	  ((= trials-remaining 0) (/ trials-passed trials))
	  ((= (gcd x1 x2) 1) (iter (- trials-remaining 1) (+ trials-passed 1) x2))
	  (else (iter (- trials-remaining 1) trials-passed x2))))))
  (iter trials 0 initial-x))

(define (random-in-range low high) (let ((range (- high low))) (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define area  (* (abs (- x1 x2) )(abs (- y1 y2))))
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0) (/ trials-passed trials))
      ((p (random-in-range x1 x2) (random-in-range y1 y2)) (iter (- trials-remaining 1) (+ 1 trials-passed)))
      (else (iter (- trials-remaining 1) trials-passed))
    ))
  (* area (iter trials 0)))

(define (estimate-pi-integral trials) (estimate-integral (lambda (x y) (>= 1.0 (+ (square x) (square y)))) -1.0 1.0 -1.0 1.0 (* 1.0 trials)) )


(define (changer)
  (define state 2)
  (define (work i)
  (if  (= state 2)
   (cond
     ((= i 0) (set! state 0)) 
     ((= i 1) (set! state 1)) 
     (else 2))
   state))
 work)
(define (append! x y) (set-cdr! (last-pair x) (cons y '())) x)
(define (last-pair x) (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x) (set-cdr! (last-pair x) x) x)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let 
	((temp (cdr x)))
	(set-cdr! x y)
	(loop temp x))))
  (loop x '()))

(define (count-pairs-buggy x)
  (if (pair? x)
    (+ (count-pairs-buggy (car x))
       (count-pairs-buggy (cdr x))
       1)
    0
    )
  )

(define (contains? element l) 
  (cond 
    ((null? l) #f) 
    ((eq? element (car l)) #t) 
    (else (contains? element (cdr l)))))

(define (count-pairs x)
  (define container '())
  (define (traverse current)
  (cond 
    ((null? current) #t)
    ((not (pair? (car current))) (begin (traverse (car current)) (traverse (cdr current))))
    (else (if (contains? (car current) container) (traverse (cdr current)) (begin (set! container (cons (car current) container)) (traverse (cdr current)))))))
   (begin (traverse x) (length container)))

(define (cyclic? x)
  (define container '())
  (define (traverse current)
  (cond 
    ((null? current) #f)
    ((contains? (car current) container) #t)
    (else (begin (set! container (cons (car current) container)) (traverse (cdr current))))))
  (traverse x))

(define (cyclic-t? x)
  (define (iter current)
    (cond
      ((null? current) #f)
      ((eq? (car x) (car current)) #t)
      (else iter (cdr current))))
  (iter x))


(define (cons-v x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond 
      ((eq? m 'car) x)
      ((eq? m 'cdr) y)
      ((eq? m 'set-car!) set-x!)
      ((eq? m 'set-cdr!) set-y!)
      (else (error "Undefined operation -- CONS" m))))
  dispatch)
(define (car-v z) (z 'car))
(define (cdr-v z) (z 'cdr))
(define (set-car-v! z new-value) ((z 'set-car!) new-value) z)
(define (set-cdr-v! z new-value) ((z 'set-cdr!) new-value) z)

(define (make-queue) (cons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let 
    ((new-pair (cons item '())))
    (cond 
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue)
      (else 
	(set-cdr! (rear-ptr queue) new-pair)
	(set-rear-ptr! queue new-pair)
	queue))))
(define (delete-queue! queue)
  (cond 
    ((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
    (else (set-front-ptr! queue (cdr (front-ptr queue))) queue)))
(define (print-queue queue) (front-ptr queue))

(define (make-queue)
  (define (empty-queue?) (null? front-ptr))
  (define front-ptr '())
  (define rear-ptr '())
  (define (set-front-ptr! item) (set! front-ptr item))
  (define (set-rear-ptr! item) (set! rear-ptr item))
  (define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car front-ptr)))
  (define (insert-queue! item)
  (let 
    ((new-pair (cons item '())))
    (cond 
      ((empty-queue?)
       (set-front-ptr! new-pair)
       (set-rear-ptr! new-pair)
       dispatch)
      (else 
	(set-cdr! rear-ptr new-pair)
	(set-rear-ptr! queue new-pair)
	dispatch))))
  (define (delete-queue!)
    (cond 
      ((empty-queue?) (error "DELETE! called with an empty queue" queue))
      (else (set-front-ptr! (cdr front-ptr)) dispatch)))
  (define (print-queue) front-ptr)
  (define (dispatch m)
    (cond
      ((eq? m 'empty-queue?) (empty-queue?))
      ((eq? m 'front-ptr) ftont-ptr)
      ((eq? m 'rear-ptr) rear-ptr)
      ((eq? m 'front-queue) (front-queue))
      ((eq? m 'insert-queue!) insert-queue!)
      ((eq? m 'delete-queue!) delete-queue!)
      ((eq? m 'print-queue) (print-queue))))
dispatch)

(define (make-dequeue)
  (define (empty-dequeue?) (null? front-ptr))
  (define front-ptr '())
  (define rear-ptr '())
  (define (set-front-ptr! item) (set! front-ptr item))
  (define (set-rear-ptr! item) (set! rear-ptr item))
  (define (first) (car front-ptr))
  (define (last) (car rear-ptr))
  (define (front-insert item)
    (let ((new (cons item '())))
      (cond
	((empty-dequeue?) (set-front-ptr! new) (set-rear-ptr! new) dispatch)
	(else (set-cdr! item front-ptr) (set-front-ptr! item) dispatch))))
  (define (back-insert item)
    (let ((new (cons item '())))
      (cond
	((empty-dequeue?) (set-front-ptr! new) (set-rear-ptr! new) dispatch)
	(else (set-cdr! rear-ptr item) (set-rear-ptr! item) dispatch))))
  (define (front-delete)
      (cond
	((empty-dequeue?) (error "DELETE! called with an empty queue"))
	(else (set-front-ptr! (cdr front-ptr)) dispatch)))
  (define (back-delete)
      (cond
	((empty-dequeue?) (error "DELETE! called with an empty queue"))))
  (define (dispatch m)
    (error "NOT MADE"))
  dispatch)

(define (make-dequeue-sep)
  (define (empty-dequeue?) (and (null? front) (null? back)))
  (define front '())
  (define back '())
  (define (first) (car front))
  (define (last) (car back))
  (define (insert-front item)
    (let ((new (cons item '())))
      (cond
	((empty-dequeue?) (set! front new) (set! back new) dispatch)
	(else (set-cdr! new front) (set! front new) dispatch))))
  (define (insert-back item)
    (let ((new (cons item '())))
    (cond
      ((empty-dequeue?) (set! front new) (set! back new) dispatch)
      (else (set-cdr! back new) dispatch))))
  (define (delete-front)
    (cond
      ((empty-dequeue?) (error "DELETE! called with an empty queue"))
      (else (set! front (cdr front)) dispatch)))
  (define (delete-back)
    (cond
      ((empty-dequeue?) (error "DELETE! called with an empty queue"))
      (else (set! back (cdr back)) dispatch)))
    (define (dispatch m)
    (error "NOT MADE"))
  dispatch)

(define (make-dequeue-double)
  (define (null-node? node) (and (null? (before node)) (null? (value node)) (null? (after node))))
  (define (make-node-e value) (list '() value '()))
  (define (make-node before value after) (list before value after))
  (define (before node) (car node))
  (define (value node) (cadr node))
  (define (after node) (caddr node))
  (define (set-before! node value) (set-car! node value) node)
  (define (set-after! node value) (set-car! (cddr node) value) node)

  (define (empty-dequeue?) (and (null? head) (null? tail)))
  (define head '())
  (define tail '())
	  (define (first) (value head))
  (define (last) (value tail))
  (define (insert-front item)
    (let ((new (make-node-e item)))
      (cond
	((empty-dequeue?) (set! head new) (set! tail new) dispatch)
	(else (set-after! new head) (set! head new) dispatch))))
  (define (insert-back item)
    (let ((new (make-node-e item)))
      (cond
	((empty-dequeue?) (set! head new) (set! tail new) dispatch)
	(else (set-before! new tail) (set! tail new) dispatch))))
  (define (delete-front)
    (cond
      ((empty-dequeue?) (error "DELETE! called with an empty queue"))
      ((null? (after head))  (set! head '()) (set! tail '()) dispatch)
      (else (set! head (after head)) dispatch)))
  (define (delete-end)
    (cond
      ((empty-dequeue?) (error "DELETE! called with an empty queue"))
      ((null? (before tail)) (set! head '()) (set! tail '()) dispatch)
      (else (set! tail (before tail)) dispatch)))
  (define (dispatch m)
    (cond 
      ((eq? m 'insert-front!) insert-front)
      ((eq? m 'insert-back!) insert-back)
      ((eq? m 'delete-front) delete-front)
      ((eq? m 'delete-back) delete-back)
      ((eq? m 'first) first)
      ((eq? m 'last) last)
      (else (error "NO-such method for the dequeue" m))
      ))
  dispatch
  )

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record (cdr record) false)))

(define (assoc key records)
  (cond 
    ((null? records) false)
    ((equal? key (caar records)) (car records))
    (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record 
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value) (cdr table))))))

(define (make-table) (list '*table*))

(define (lookup-2 key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
	(if record (cdr record) false))
      false)))

(define (insert-2! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
	(if record
	  (set-cdr! record value)
	  (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
      (set-cdr! table
		(cons (list key-1 (cons key-2 value))
		      (cdr table))))))

(define (make-table-d)
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

(define operation-table (make-table-d))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define (make-table-n check?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond 
	((null? records) false)
	((check? key (caar records)) (car records))
	(else (assoc key (cdr records)))))
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

(define (make-table-l)
  (let ((local-table (list '*table*)))
    
    (define (lookup . keys)
      (define (work table k)
	(if (null? k)
	  (cdr table)
	  (let ((element (assoc (car k) (cdr table))))
	    (if element (work element (cdr k)) false))))
      (work local-table keys))

    (define (make-table value keys)
      (if 
	(null? (cdr keys))
	(cons (car keys) value)
	(list (car keys) (make-table value (cdr keys)))))

    (define (insert! value . keys)
      (define (work table k)
	(if (null? k) 
	  (set-cdr! table value)
	    (let ((element (assoc (car k) (cdr table))))
	      (if element
		(work element (cdr k))
	     (set-cdr! table (cons (make-table value k) (cdr table)))))))
	  (work local-table keys))

    (define (dispatch m)
      (cond
	((eq? m 'lookup-proc) lookup)
	((eq? m 'insert-proc!) insert!)
	((eq? m 'debug) local-table)
	(else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
	(or 
	  previously-computed-result
	  (let ((result (f x))) (insert! x result table) result))))))

(define memo-fib
  (memoize (lambda (n)
	     (cond 
	       ((= n 0) 0)
	       ((= n 1) 1)
	       (else (+ 
		       (memo-fib (- n 1))
		       (memo-fib (- n 2))))))))

(define (make-wire)
  (define value #f)
  (define listeners '())
  (define (set-value! v) (if (eq? v value) 0 (begin (set! value v) (inform-listeners))))
  (define (set-operation! op) (set! operation op) (react))
  (define (inform-listeners) (for-each (lambda (x) (x 'react)) listeners) 0)
  (define (add-listener! listener) (set! listeners (cons listener listeners)))
  (define (remove-listener! listener) (set! listener (filter (lambda (element) (not (eq? element listener)) listeners))))
  (define (react) (set-value! (operation)))
  (define operation (lambda () #f)) 
  (define (dispatch m)
  (cond
    ((eq? m 'value) value)
    ((eq? m 'set-value!) set-value!)
    ((eq? m 'add-listener!) add-listener!)
    ((eq? m 'remove-listener!) remove-listener!)
    ((eq? m 'set-operation!) set-operation!)
    ((eq? m 'listeners) listeners)
    ((eq? m 'react) (react))
    (else (error "No such method" m))))
  dispatch)

(define (set-operation! wire op) ((wire 'set-operation!) op))
(define (get-value wire) (wire 'value))
(define (add-listener! wire listener) ((wire 'add-listener!) listener))

(define (not-gate input output)
  (after-delay not-delay (begin
  (add-listener! input output)
  (set-operation! output (lambda () (not (get-value input)))) 
  0)))

(define (xor a b)
  (cond
    ((= a #f) b)
    ((= a #t) (not b))))

(define (and-gate input1 input2 output)
  (after-delay and-delay (begin
  (add-listener! input1 output)
  (add-listener! input2 output)
  (set-operation! output (lambda () (and (get-value input1) (get-value input2))))
  0)))

(define (xor-gate input1 input2 output)
  (after-delay xor-delay (begin
  (add-listener! input1 output)
  (add-listener! input2 output)
  (set-operation! output (lambda () (xor (get-value input1) (get-value input2))))
  0)))

(define (or-gate input1 input2 output)
  (after-delay or-delay (begin
  (add-listener! input1 output)
  (add-listener! input2 output)
  (set-operation! output (lambda () (or (get-value input1) (get-value input2))))
  0)))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (not-gate c e)
    (and-gate d e s)
    0))

(define on (lambda () #t))
(define off (lambda () #f))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    0))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
  (after-delay inverter-delay
	       (lambda ()
		 (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond 
    ((= s 0) 1)
    ((= s 1) 0)
    (else (error "Invalid signal" s))))

(define (and-gate-n a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	    (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and f s)
  (cond 
    ((= f 0) 0)
    ((= f 1) s)
    (else (error "Invalid signals" f s))))

(define (logical-or f s)
  (cond 
    ((= f 1) 1)
    ((= f 0) s)
    (else (error "Invalid signals" f s))))

(define (or-gate-n a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	    (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (or-gate-delay) 0)
(define (and-gate-delay) 0)
(define (inverter-delay) 0)
(define (after-delay d proc) (d) (proc))

(define (different-or a b) (not (and (not a) (not b))))

(define (ripple-carry-adder a-list b-list s-list c-in)
  (define (work a-current b-current s-current c-current-in c-current-out)
    (if (null? a-current) 0
      (begin
	  (full-adder (car a-current) (car b-current) c-current-in (car s-current) c-current-out)
	  (work (cdr a-current) (cdr b-current) (cdr s-current) c-current-out (make-wire)))))
  (work a-list b-list s-list c-in (make-wire)))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
(if (empty-agenda? the-agenda)
  0
  (let ((first-item (first-agenda-item the-agenda)))
    (first-item)
    (remove-first-agenda-item! the-agenda)
    (propagate))))

(define (probe name wire)
  (define (react s)
    (newline)
    (display name)
    (display " ")
    (display (current-time the-agenda))
    (display " New-value = ")
    (display (get-value wire))
    (newline)
    (newline))
  (add-listener! wire react)
  0)


(define not-delay 2)
(define and-delay 3)
(define or-delay 5)
(define xor-delay 4)

(define (make-time-segment time queue)
(cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments)) action)
      (let ((rest (cdr segments)))
	(if (belongs-before? rest)
	  (set-cdr! segments 
		    (cons (make-new-time-segment time action) (cdr segments)))
	  (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
	agenda
	(cons (make-new-time-segment time action) segments)) 
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))

(define (make-connector-m)
  (define listeners '())
  (define value #f)
  (define constant #f)
  (define (set-constant! value) (set! constant value))
  (define operation (lambda () #f))
  (define (set-value! v) (if (not (eq? v value)) (begin (set! value v) (notify-listeners))))
  (define (set-operation! op) (set! operation op) (react))
  (define (add-listener! listener) (set! listeners (cons listener listeners)))
  (define (remove-listener! listener) (set! listeners (filter (lambda (x) (not (eq? x listener))) listeners)))
  (define (notify-listeners) (for-each (lambda (x) (x 'react)) listeners) 0)
  (define (react) (let ((value (operation))) (if (number? value) (set-value! value))))
  (define (forget-value) (set-operaion! (lambda () #f)))
  (define (dispatch m)
    (cond
      ((eq? m 'value) value)
      ((eq? m 'set-value) set-value)
      ((eq? m 'add-listener!) add-listener!)
      ((eq? m 'remove-listener) remove-listener)
      ((eq? m 'react) react)
      ((eq? m 'set-operation!) set-operation!)
      ((eq? m 'set-constant!) set-constant!)
      ((eq? m 'listeners) listeners)
      ((eq? m 'forget-value) forget-value)
      (else "ERROR No such method" m)))
  dispatch)
(define (make-constant value connector) ((connector 'set-constant!) false) (set-operation! connector (lambda () value)))

(define (make-adder input1 input2 output)
   (make-operation input1 input2 + output)
   (make-operation output input2 - input1)
   (make-operation output input1 - input2))

(define (make-multiplier input1 input2 output)
   (make-operation input1 input2 * output)
   (make-operation output input2 / input1)
   (make-operation output input1 / input2))

(define (make-operation left right op output)
  (define (operation) (if (and (has-value? left) (has-value? right)) (op (get-value left) (get-value right)) #f))
  (add-listener! left output)
  (add-listener! right output)
  (set-operation! output operation))

(define (has-value? connector)
(connector 'has-value?))
(define (set-value! connector new-value informant)
((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
((connector 'forget) retractor))
(define (connect connector new-constraint)
((connector 'connect) new-constraint))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond 
      ((and (has-value? a1) (has-value? a2))
       (set-value! sum (+ (get-value a1) (get-value a2)) me))
      ((and (has-value? a1) (has-value? sum))
       (set-value! a2 (- (get-value sum) (get-value a1)) me))
      ((and (has-value? a2) (has-value? sum))
       (set-value! a1 (- (get-value sum) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond 
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
(constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
(constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond 
      ((or (and (has-value? m1) (= (get-value m1) 0))
	   (and (has-value? m2) (= (get-value m2) 0)))
       (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2))
       (set-value! product
		   (* (get-value m1) (get-value m2))
		   me))
      ((and (has-value? product) (has-value? m1))
       (set-value! m2
		   (/ (get-value product) (get-value m1))
		   me))
      ((and (has-value? product) (has-value? m2))
       (set-value! m1
		   (/ (get-value product) (get-value m2))
		   me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond 
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value)(process-forget-value))
      (else (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)


(define (probe-c name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond 
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value)(process-forget-value))
      (else (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond 
	((not (has-value? me)) (set! value newval)
			       (set! informant setter)
			       (for-each-except setter inform-about-value constraints))
	((not (= value newval))(error "Contradiction" (list value newval)))
	(else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	(begin (set! informant false) (for-each-except retractor inform-about-no-value constraints))
	'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	(set! constraints (cons new-constraint constraints)))
      (if (has-value? me) (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond 
	((eq? request 'has-value?) (if informant true false))
	((eq? request 'value) value)
	((eq? request 'set-value!) set-my-value)
	((eq? request 'forget) forget-my-value)
	((eq? request 'connect) connect)
	(else (error "Unknown operation -- CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? (car items) exception) (loop (cdr items)))
	  (else (procedure (car items))
		(loop (cdr items)))))
  (loop list))

(define (averager a b c)
  (define two (make-connector))
  (define result (make-connector))
  (constant 2 two)
  (multiplier two c result)
  (adder a b result)
  'ok)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
	(error "square less than 0 -- SQUARER" (get-value b))
	(set-value! a (sqrt (get-value b)) me))
      (if (has-value? a) (set-value! b (square (get-value a)) me))))
  (define (process-forget-value) 
    (forget-value! a me) 
    (forget-value! b me))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value)(process-forget-value))
      (else (error "Unknown request -- PROBE" request))))
  (connect a me)
  (connect b me)
me)

(define (c+ x y) (let ((z (make-connector))) (adder x y z) z))
(define (c- x y) (let ((z (make-connector))) (adder z y x) z))
(define (c* x y) (let ((z (make-connector))) (multiplier x y z) z))
(define (c/ x y) (let ((z (make-connector))) (multiplier z y x) z))
(define (cv x v) (constant v x))


(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
	(mutex 'acquire)
	(let ((val (apply p args)))
	  (mutex 'release)
	  val))
      serialized-p)))

(define (make-mutex)
  (define cell (list false))
  (define (test-and-set! cell)
  (if (car cell)
    true
    (begin (set-car! cell true)
	   false)))
  (define (clear! cell) (set-car! cell false))
  (define (the-mutex m)
    (cond 
      ((eq? m 'acquire)
       (if (test-and-set! cell)
	 (the-mutex 'acquire))) ; retry
      ((eq? m 'release) (clear! cell))))
  
  the-mutex)

(define (generate-list n v)
  (define (work current) (if (= current 0) '() (cons v (work (- 1 current)))))
  (work n))

(define (generate-list-c n constructor)
  (define (work current) (if (= current 0) '() (cons (constructor) (work (- 1 current)))))
  (work n))

(define (make-semaphore-pure n)
  (define cells (generate-list n false))
  (define pointer cells)
  (define (test-and-set! c)
  (cond
    ((null? c) true)
    ((car c) (test-and-set! (cdr c))
    (else (set-car! cell true) false))))
  (define (the-semaphore m)
    (cond
      ((eq? m 'acquire)
       (if (test-and-set! cells)
	 (the-semaphore 'acquire)))
       ((eq? m 'release)  (set-car! pointer false)
			  (set! pointer (cdr pointer))
			  (if (null? (cdr pointer)) (set! pointer cells)))))
  the-semaphore)

(define (make-semaphore-numeric n)
  (define used 0)
  (define (the-semaphore m)
    (cond
      ((eq? m 'acquire) (if (= used n) (the-semaphore 'acquire) (set! used (+ 1 used))))
       ((eq? m 'release) (if (= used 0) (error "NO TAKEN LOCKS") (set! used (- used 1))))))
  the-semaphore)

(define (make-semaphore-combined n)
  (define used 0)
  (define pointer cells)
  (define cells (generate-list-c n make-mutex))
  (define (test-and-set! c)
    (cond
      ((null? c) true)
      ((car c) (test-and-set! (cdr c)))
      (else (set-car! cell true) (set! used (+ 1 used)) false)))
  (define (the-semaphore m)
  (cond
      ((eq? m 'acquire)
       (if (or (= used n) (test-and-set! cells))
	 (the-semaphore 'acquire)))
       ((eq? m 'release)  ((car pointer) 'release)
			  (set! pointer (cdr pointer))
			  (set! used (- used 1))
			  (if (= used 0) (set! pointer cells)))))
  the-semaphore)

(define (make-account-unique balance)
  	(define id (rand))
	(define (withdraw amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
	(define (deposit amount) (set! balance (+ balance amount)) balance)
	(define (interest percent) (set! balance (* (+ 1 percent) balance)) balance)
	(define (dispatch m)
	(cond 
	  ((eq? m 'id) id)
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  ((eq? m 'interest) interest)
	  ((eq? m 'display ) balance)
	(else (error "Unknown request -- MAKE-ACCOUNT" m))))
dispatch)

(define (memo-proc proc)
  (let ((already-run? false) 
	(result false))
    (lambda ()
      (if (not already-run?)
	(begin (set! result (proc))
	       (set! already-run? true)
	       result)
	result))))

;(define (delay object) (memo-proc (lambda () x)))
;(define-syntax delay (syntax-rules () ((_ object) (memo-proc (lambda () object)))))
;(define (force delayed-object) (delayed-object))

(define (prime? n) (= n (smallest-divisor n)))
(define (smallest-divisor n) (if(divides? 2 n) 2 (find-divisor n 3)))
(define (find-divisor n test-divisor) (cond
					((> (square test-divisor) n) n) 
					((divides? test-divisor n) test-divisor)
					(else (find-divisor n (+ test-divisor 2)))))
(define (divides? a b) (= (remainder b a) 0))

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

(define (show x)
  (display-line x)
  x)

(define (integers-starting-from n) (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define no-sevens (stream-filter (lambda (x) (not (divides? 7 x))) integers))

(define (divisible? x a) (divides? a x))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs-gen (fibgen 0 1))

(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve 
      (stream-filter (lambda (x) (not (divisible? x (stream-car stream))))
		     (stream-cdr stream)))))

(define primes-sieve (sieve (integers-starting-from 2)))
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map-l + s1 s2))
(define (mul-streams s1 s2) (stream-map-l * s1 s2))
(define (div-streams s1 s2) (stream-map-l / s1 s2)) 
(define fibs-con (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs-con) fibs-con))))
(define (scale-stream stream factor) (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(define primes (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond 
      ((> (square (stream-car ps)) n) true)
      ((divisible? n (stream-car ps)) false)
      (else (iter (stream-cdr ps)))))
  (iter primes))
(define factorials (cons-stream 1 (mul-streams factorials integers)))
(define (partial-sums stream) (cons-stream (stream-car stream) (add-streams (partial-sums stream) (stream-cdr stream))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	  (let ((s1car (stream-car s1))
		(s2car (stream-car s2)))
	    (let ((s1-weight (weight s1car))
		  (s2-weight (weight s2car)))
	    (cond 
	      ((< s1-weight s2-weight) (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
	      ((> s1-weight s2-weight) (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
	      (else (cons-stream s1car (cons-stream s2car (merge-weighted (stream-cdr s1) (stream-cdr s2) weight))))))))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	  (let ((s1car (stream-car s1))
		(s2car (stream-car s2)))
	    (cond 
	      ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
	      ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
	      (else (cons-stream s1car (merge (stream-cdr s1)  (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)) )))

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define (integrate-series stream) (div-streams stream integers))

(define exp-series (cons-stream 1 (integrate-series exp-series)))
(define (exp-series-x x) (cons-stream 1 (integrate-series (stream-map (lambda (element) (* x element)) (exp-series-x x)))))

(define cos-helper-hand (cons-stream 0 (cons-stream -1 (cons-stream 0 (cons-stream 1 cos-helper-hand)))))
(define sin-helper-hand (cons-stream 1 (cons-stream 0 (cons-stream -1 (cons-stream 0 sin-helper-hand)))))

(define (cos-series helper) (cons-stream 1 (mul-streams (integrate-series exp-series) helper)))
(define (cos-series-x x helper) (cons-stream 1 (mul-streams (integrate-series (exp-series-x x helper)) helper)))
(define (sin-series helper) (cons-stream 0 (mul-streams (integrate-series exp-series) helper)))
(define (sin-series-x x helper) (cons-stream 0 (mul-streams (integrate-series (exp-series-x x helper)) helper)))

(define cos-helper-sequence (stream-map (lambda (element) (* (- 1 (modulo element 2)) (- 1 (modulo element 4)))) integers))
(define cos-helper-b-sequence (stream-map (lambda (element) (* (- 1 (logand element 1)) (- 1 (logand element 3)))) integers))

(define sin-helper-cos-sequence (stream-map (lambda (element) (* (- 1 (modulo element 2)) (- 1 (modulo element 4)))) (integers-starting-from 4)))
(define sin-helper-b-cos-sequence (stream-map (lambda (element) (* (- 1 (logand element 1)) (- 1 (logand element 3)))) (integers-starting-from 4)))

(define sin-helper-b-sequence (stream-map (lambda (element) (* (logand element 1) (- 2 (logand element 3)))) integers))
(define sin-helper-sequence (stream-map (lambda (element) (* (modulo element 2) (- 2 (modulo element 4)))) integers))

(define (mul-series s1 s2) (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (mul-series (stream-cdr s1) s2) (scale-stream (stream-cdr s2) (stream-car s1)))))

(define (stream-acc proc start stream count)
  (define (work result current-count current-stream)
  (if (= current-count 0) 
    result
    (work (proc result (stream-car current-stream)) 
	  (- 1 count) 
	  (stream-cdr current-stream))))
  (work start count stream))

(define sine (sin-series sin-helper-hand))
(define cosine (cos-series cos-helper-hand))
(define one-stream (add-streams (mul-series sine sine) (mul-series cosine cosine)))

(define (invert-unit-series s)
  (define sr (stream-cdr s))
  (define x (cons-stream 1 (mul-series (scale-stream sr -1) x)))
  x)

(define (div-series-hacky a b) (mul-series a (invert-unit-series b)))

(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
(average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
;(display-stream pi-stream)
;
(define (euler-transform s)
  (let 
    ((s0 (stream-ref s 0)) ; S n-1
     (s1 (stream-ref s 1)) ; S n
     (s2 (stream-ref s 2))) ; S n+1
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(define (sqrt-stream-l x)
  (cons-stream 1.0
	       (stream-map (lambda (guess)
			     (sqrt-improve guess x))
			   (sqrt-stream-l x))))

(define (stream-limit stream tolerance)
  (define (work current)
    (let ((next (stream-cdr current)))
    (if (> tolerance 
	   (abs (- 
		  (stream-car current) 
		  (stream-car next))))
      (stream-car next)
      (work next))))
  (work stream))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
		 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
		  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (pairs-all s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
		  (stream-cdr t))
      (pairs (stream-cdr s) t))))

(define (pairs-lou s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) t)
    (pairs-lou (stream-cdr s) (stream-cdr t))))

(define (triplets s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
	       (interleave
		 (stream-map (lambda (x) (list (stream-car s) (stream-car t) x)) (stream-cdr u))
		 (interleave
		  (triplets s (stream-cdr t) (stream-cdr u))
		  (triplets (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define int-pairs (pairs integers integers))
(define int-triplets (triplets integers integers integers))

(define pythagorean-triplets (stream-filter (lambda (triplet) (= (square (caddr triplet)) (+ (square (cadr triplet)) (square (car triplet))))) int-triplets))
(define prime-sum-pairs (stream-filter (lambda (pair) (prime? (+ (car pair) (cadr pair)))) int-pairs))

(define (weighted-pairs s t weight)
  (cons-stream (list (stream-car s) (stream-car t))
	       (merge-weighted 
		 (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
		 (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
		 weight)))

(define specialised-stream (stream-filter (lambda (x) (not (or (= 0 (remainder x 2)) (= 0 (remainder x 3)) (= 0 (remainder x 5))))) integers))
(define specialised-filter (lambda (x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x)))))

(define (cube x) (* x x x))

;(define (stream-for-each proc s)
;  (if (stream-null? s)
;    'done
;    (begin 	
;      (proc (stream-car s))
;      (stream-for-each proc (stream-cdr s)))))

(define (ramanujan-stream)
  (define (formula x) (+ (cube (car x)) (cube (cadr x))))
  (define start-stream (weighted-pairs integers integers formula))
  (define (work stream)
    (if (= (formula (stream-car stream))
	   (formula (stream-car (stream-cdr stream))))
      (cons-stream (list (stream-car stream) (stream-car (stream-cdr stream)))
		 (work (stream-cdr stream)))
    (work (stream-cdr stream))))
  (work start-stream))

(define (triple-square-representation)
  (define (formula x) (+ (square (car x)) (square (cadr x))))
  (define start-stream (weighted-pairs integers integers formula))
  (define (work stream)
    (let ((first (stream-car stream))
	  (second (stream-car (stream-cdr stream)))
	  (third (stream-car (stream-cdr (stream-cdr stream)))))
    (if (= (formula first)
	   (formula second)
	   (formula third))
      (cons-stream (list first second third)
		 (work (stream-cdr stream)))
    (work (stream-cdr stream))))) 	
  (work start-stream))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)

(define (RC R C dt)
  (define divC (/ 1 C))
  (define (work i v0) (add-streams (scale-stream i R ) (scale-stream (integral i v0 dt) divC)))				   
    work)

;(define zero-crossings (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
		 (make-zero-crossings (stream-cdr stabilized) (stream-car input-stream)))))

(define (smooth s)
  (cons-stream (/ (+ (stream-car s) (stream-car (streamc-cdr s))) 2)
	       (smooth (stream-cdr s))))

(define (make-zero-crossings-smooth input-stream last-value smoother)
  (let ((smoothed (smoother input-strea)))
    (cons-stream (sign-change-detector (stream-car smoothed) last-value)
		 (make-zero-crossings (stream-cdr smoothed) (stream-cdr stabilized)))))

(define (integral-delayed delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (let ((integrand (force delayed-integrand)))
		   (add-streams (scale-stream integrand dt)
				int))))
  int)

(define (solve f y0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (solve-ending f y0 dt)
  (define y (integral-delayed-ending (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral-ending integrand initial-value dt)
  (cons-stream initial-value
	       (if (stream-null? integrand)
		 the-empty-stream
		 (integral-ending (stream-cdr integrand)
			   (+ (* dt (stream-car integrand))
			      initial-value)
			   dt))))

(define (integral-delayed-ending delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))	       
		 (if (stream-null? integrand)
		 the-empty-stream
		 (integral-delayed-ending (stream-cdr integrand)
			   (+ (* dt (stream-car integrand)) initial-value)
			   dt)))))

;(display (stream-ref (solve (lambda (y) y) 1 0.001) 1000))
;(newline)
;(display (stream-ref (solve-ending (lambda (y) y) 1 0.001) 1))


(define (solve-2nd a b dt y0 dy0)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (integral-delayed (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
			   (scale-stream y b)))
  y)
(define (solve-2nd f dt y0 dy0)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (integral-delayed (delay ddy) dy0 dt))
  (define ddy (f (/ dy dt) y ))
  y)

(define (RLC R L C dt)
  (define (work vC0 iL0)
  (define vC (integral-delayed (delay dvC) vC0 dt))
  (define iL (integral-delayed (delay diL) iL0 dt))
  (define dvC (scale-stream iL (/ -1 C)))
  (define diL (add-streams (scale-stream iL (/ (- R) L))
			   (scale-stream vC (/ 1 L))))
  (cons vC iL))
  work)

(define RLC-test ((RLC 1 0.2 1 0.1) 10 0))
(define vC-test (car RLC-test))
(define iL-test (cdr RLC-test))

(define random-init (random 1000000))
(define (rand-update number) (random 1000000))

(define random-numbers
(cons-stream random-init
(stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1)) random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))
(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
	      (monte-carlo cesaro-stream 0 0)))

(define generate (cons-stream (random 1000) (stream-map rand-update generate)))
(define (reset s value)  (define result (cons-stream value (stream-map rand-update result))) result)

(define (estimate-integral p x1 x2 y1 y2)
  (define area  (* (abs (- x1 x2) )(abs (- y1 y2))))
  (define (job passed failed)
    (define (next passed failed) (cons-stream (* area (/ passed (+ passed failed))) (job passed failed)))
  (if (p (random-in-range x1 x2) (random-in-range y1 y2))
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))
  (job 0 0))

(define (stream-withdraw balance amount-stream)
  (cons-stream
    balance
    (stream-withdraw (- balance (stream-car amount-stream))
		     (stream-cdr amount-stream))))

