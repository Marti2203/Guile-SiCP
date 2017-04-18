(define (crt x)
		(define (abs x) (if (< x 0) (- x) x) )
		(define (square x) (* x x) )
		(define (cube x) (* x x x) )
		(define (cubeguess guess)  (/ (+ (* 2.0 guess) (/ x (square guess) ) ) 3 ) )
		(define (cube-guess-good? guess) 
			(if ( < (abs (- guess (cubeguess guess) ) ) 0.0001 ) #t #f ) )
		(define (cube-iter guess) (if (cube-guess-good? guess) guess (cube-iter (cubeguess guess) ) ) )
	(cube-iter 1.0) )
(define (fibtree n) (cond ( (= n 0) 0) ( (= n 1)  1) (else (+ (fibtree (- n 1)) (fibtree (- n 2))))))

(define (fib-iter a b count)
(if (= count 0) b (fib-iter 
			    (+ a b) 
		            a 
		            (- count 1)
		  )))
(define (fib n) (fib-iter 1 0 n) )

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
	(cond 
		((= amount 0) 1)
		((or (< amount 0) (= kinds-of-coins 0)) 0)
		(else (+ 
			(cc amount (- kinds-of-coins 1) )
			(cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))
		      )
		))

(define (first-denomination kinds-of-coins)
	(cond 
		((= kinds-of-coins 1) 1)
		((= kinds-of-coins 2) 5)
		((= kinds-of-coins 3) 10)
		((= kinds-of-coins 4) 25)
		((= kinds-of-coins 5) 50)))

(define (f n) (if (< n 3) n (+ (f (- n 1) ) (* 2 (f (- n 2)) ) (* 3 (f (- n 3) ) ) ) ) )

(define (pascal row index) 
		(cond
			((or (< row 0) (< index 0) (< row index) ) - 1)
			((= index 1) 1)
			((= index row) 1)
			(else (+ (pascal (- row 1) index) (pascal (- row 1) (- index 1) ) ) 
		)))
(define (fact x) (
		cond 
			((or (= x 1) (= x 0) ) 1)
			((< x 0) -1) 
			(else(* x (fact (- x 1))))

		)) 

(define (coef n k) (if (< n k) (- 1) (/ (fact n) (* (fact k) (fact (- n k))))))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (even? n) (= (remainder n 2) 0))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle) (if (not (> (abs angle) 0.1)) angle (p (sine (/ angle 3.0 ) ))))

(define (expt b n) (expt-iter b n 1))

(define (expt-iter b counter product) (if (= counter 0) product (expt-iter b (- counter 1) (* b product))))

(define (fast-expt b n)
	(cond 
		((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (fast-exptiter result b n) 
	(cond
		((= n 0) result)
		((even? n) (fast-exptiter (* result (square b)) b (- n 2)))
		(else ((fast-exptiter (* result b) b (- n 1))))
	))

(define (fast-expt-iter b n) (fast-exptiter 1 b n))

(define (double x) (+ x x))
(define (half x) (/ x 2))

(define (mult a b) (if (= b 0) 0 (+ a (mult a (- b 1)))))
(define (mult-iter result a b) (if (= b 0) result (mult-iter (+ result a ) a (- b 1))))
(define (multiter a b) (mult-iter 0 a b))
(define (mult-fast a b) (cond
				((= b 0) 0)
				((even? b) (+ (double a) (mult-fast a (- b 2))))
				(else (+ a (mult-fast a (- b 1)))) 
			))
(define (mult-iter-fast result a b) (cond
			((= b 0) result)
			((even? b) (mult-iter-fast (+ result (double a)) a (- b 2)))
			(else (mult-iter-fast (+ result a) a (- b 1)))
		))
(define (mult-iterfast a b) (mult-iter-fast 0 a b))

(define (mult-iterfastest a b) (if (< a b) (mult-iterfast b a) (mult-iterfast a b)))

(define (fib-log n) (fib-iter-log 1 0 0 1 n))
(define (fib-iter-log a b p q count)
		(cond 
			((= count 0) b)
			((even? count) (fib-iter-log a b (+ (square p) (square q) ) (+ (square q) (* 2 p q) ) (/ count 2)))
			(else (fib-iter-log 
					(+ (* b q) (* a q) (* a p) ) 
					(+ (* b p) (* a q)) 
					p 
					q
					(- count 1)))))

(define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))

(define (prime? n) (= n (smallest-divisor n)))
(define (smallest-divisor n) (if(divides? 2 n) 2 (find-divisor n 3)))
(define (find-divisor n test-divisor) (cond
					((> (square test-divisor) n) n) 
					((divides? test-divisor n) test-divisor)
					(else (find-divisor n (+ test-divisor 2)))))

(define (divides? a b) (= (remainder b a) 0))


(define (expmod base exp m)
		(cond 
			((= exp 0) 1)
			((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
			(else (remainder (* base (expmod base (- exp 1) m)) m))
		))

(define (fermat-test n)
	(define (try-it a) (= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
	(cond 
		((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)
	))

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (get-internal-real-time))  )

	(define (start-prime-test n start-time) (if (prime? n) (report-prime (- (get-internal-real-time) start-time ) )))

(define (report-prime elapsed-time) (display " *** ") (display elapsed-time) (newline) )

(define (search-for-primes current count start-time )
	(cond
		( (= count 0) (newline) )
		((prime? current)   ( (display current)
				    (report-prime (- (get-internal-real-time) start-time )) 
				    ;(display "CUUUUNT\n")
				    (search-for-primes  (+ current 2) 
				    			(- count 1) 
				    			(get-internal-real-time)
							) )
				  )
		(else ( (display current) (newline) (search-for-primes  
							(+ current 2) 
					  		count 
					  		(get-internal-real-time) 
					  ) ) )
	) )

(define (search-for-primes-fast current count start-time )
	(cond
		( (= count 0) newline )
		((fast-prime? current 10)   ( (display current)
				    (report-prime (- (get-internal-real-time) start-time )) 
				    ;(display "CUUUUNT\n")
				    (search-for-primes-fast  (+ current 2) 
				    			(- count 1) 
				    			(get-internal-real-time)
							) )
				  )
		(else ( (display current) (newline) (search-for-primes-fast  
							(+ current 2) 
					  		count 
					  		(get-internal-real-time) 
					  ) ) )
	) )	

(define (start-search start count) (if (even? start) 
							(search-for-primes (+ 1 start) 
									   count 
									   (get-internal-real-time)
									   ) 
							(searh-for-primes start 
									  count 
									  (get-internal-real-time)
									  )
				   ))

(define (start-search-fast start count) (if (even? start) 
							(search-for-primes-fast (+ 1 start) 
									   count 
									   (get-internal-real-time)
									   ) 
							(searh-for-primes-fast start 
									  count 
									  (get-internal-real-time)
									  )
				   ))

(define (fermat-alternate a n) (= (exp-mod-specialised a (- n 1) n)))

(define (exp-mod-specialised a b n) 
	(cond
		((= b 0) 1)
		((even? b) special-work a (/ b 2) n (remainder (square (exp-mod-specialised a (/ b 2) n) ) n) )  
		(else (remainder (* b (exp-mod-specialised a (- b 1) n ) n ) n ))
	))
(define (special-work a b n r)
	(if (and (= (remainder (square r) n) 1 ) (not (or (= r 1)  (= r (- n 1))) ) ) 
	    0
	    r
	  )) 
	  
