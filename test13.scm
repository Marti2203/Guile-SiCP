(define (square x) (* x x) )

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (identity x) x)

(define (sum term a next b) (if (> a b) 0 (+ (term a) (sum term (next a) next b))))

(define (sum-iter term a next b)
	(define (iter a result) (if (> a b) result (iter (next a) (+ result (term a) ) ) ) )
	(iter a 0))


(define (product term a next b) (if (> a b) 1 (* (term a) (product term (next a) next b) ) ) )

(define (product-iter term a next b)
	(define (iter a result) (if (> a b) result (iter (next a) (* result (term a) ) ) ) )
	(iter a 1)
)

(define (fac n) (product-iter identity 1 inc n))

(define (aggregate main term a next b default)
	(define (work a) (if (> a b) default (main (term a) (work (next a) ))))
	(work a) )

(define (sum-agg-inc a b)
	(aggregate-iter + identity a inc b 0))

(define (aggregate-iter main term a next b default)
	(define (work result a) (if (> a b) result (work (main result (term a)) (next a) )))
	(work default a))

(define (aggregate-filter main filter term a next b default)
	(cond
	((> a b) default)
	((filter (term a)) (main (term a) (aggregate-filter main filter term (next a) next b default)))
	(else (aggregate-filter main filter term (next a) next b default ))
	)
	)

(define (prime? n) (= n (smallest-divisor n)))
(define (smallest-divisor n) (if(divides? 2 n) 2 (find-divisor n 3)))
(define (find-divisor n test-divisor) (cond
					((> (square test-divisor) n) n) 
					((divides? test-divisor n) test-divisor)
					(else (find-divisor n (+ test-divisor 2)))))
(define (divides? a b) (= (remainder b a) 0))

(define (gcd a b) (if(= b 0 ) a (gcd b (remainder a b))))

(define (relative-primes? a b) (= 1 (gcd a b) ) )

(define (prime-sum start end) (aggregate-filter + prime? identity start inc end 0))

(define (sum-relative-primes n)
	(define (relative-prime? a) (relative-primes? a n))
	(if(prime? n) (sum-agg-inc 2 (- n 1) ) (aggregate-filter + relative-prime? identity 2 inc n 0)))

(define (pi-sum a b)
	(define (pi-term x) (/ 1.0 (* x (+ x 2))))
	(define (pi-next x) (+ x 4))
	(sum pi-term a pi-next b))

(define (pi-product b)
	(define (pi-term x) (square (* 2.0 x)))
	(define (pi-term-uneven x) (square (+ 1.0 (* 2.0 x))))
	(define (pi-next x) (+ x 1.0))
	(/ (product pi-term 0.0 pi-next b) (product pi-term-uneven 1.0 pi-next b)) )

(define (integral-pure f a b dx)
	(define (add-dx x) (+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson f a b n current) 
	(define h (/ (- b a) n))
	(define (mult e n) (if (or (= e 0) (= e n) ) 1 (* 2 (+ 1 (remainder e 2) ) ) ))
	(if (> current n) 0 (+ (* (mult current n) (f (+ a (* current h) ))) (simpson f a b n (+ 1 current)) ) ))

(define (integral-simpson f a b n)
(define h (/ (- b a) n ) ) (/ (* h (simpson f a b n 0.0) ) 3.0) )

(define (search f neg-point pos-point)
	(let ((midpoint (average neg-point pos-point)))
(if (close-enough? neg-point pos-point) midpoint
	(let ((test-value (f midpoint)))
	(cond 
	((positive? test-value) (search f neg-point midpoint))
	((negative? test-value) (search f midpoint pos-point)) 
	(else midpoint))))))

(define (average a b) (/ (+ a b) 2) )

(define (average-triple a b c) (/ (+ a b c) 3))

(define (half-interval-method f a b)
	(let (  (a-value (f a))
		(b-value (f b))
	     )
	(cond 
		((and (negative? a-value) (positive? b-value)) (search f a b))
		((and (negative? b-value) (positive? a-value)) (search f b a))
		(else (error "Values are not of opposite sign" a b)))
	))

(define tolerance 0.00000000001)
(define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
	(define (try guess)
	(display guess)
	(newline)
	(let ((next (f guess)))
	(if (close-enough? guess next) next (try next))))
	(try first-guess))

(define (sqrt x) (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;(define golden (fixed-point (lambda (x) (+ 1 (/ 1 x) )) 1.0))
;(define gold (/ 1 golden) )
;(define logx (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0 ) )

(define (cont-frac main n d k)
	(define (work current)
	(if (> current k) 0 (/ (n current) (main (d current) (work (+ 1 current) ) ) ) ))
	(work 1)
)
(define (cont-frac-iter main n d k)
	(define (work current result)
	(if (= current 0) result (work (- current 1) (/ (n current) (main (d current) result)))))
	(work k 0))

(define (get-gold k) (cont-frac-iter + (lambda (i) 1.0) (lambda (i) 1.0) k))
(define (test start end) (if (= start end) 0 ( (get-gold start) (test (+ 1 start) end) ) ) )

(define (divide a b) (if (< a b) 0 (+ 1 (divide (- a b) b))))
(define (divide-iter a b)
	(define (work result a) (if (< a b) result (work (+ result 1) (- a b))))
	(work 0 a))

(define (get-e n) 
	(cont-frac-iter
	+ 
	(lambda (x) 1.0) 
	(lambda (y) (if (= (remainder y 3) 2) (* 2 (+ 1 (divide-iter y 3))) 1))  
	n))

(define (tan-cf x k) (cont-frac-iter - (lambda (y) (* x x)) (lambda (z) (- (* 2 z) 1) ) k))

(define (average-damp f) (lambda (x) (average x (f x))))

(define (cube-root x) (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (pow a b) 
	(define (work current) (if (= current b) 1 (* a (work (+ current 1) ) )))
	(work 0)
)

(define (pow-iter a b)
	(define (work result current) (if (= current b) result (work (* a result) (+ 1 current))  ))
	(work 1 0))

(define dx 0.0000001)
(define (deriv g) (lambda (x) (/ (- (g (+ x dx)) (g x) )dx)))

(define (newton-transform g) (lambda (x)(- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) (fixed-point (newton-transform g) guess))

(define (sqrt-new x) (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess) (fixed-point (transform g) guess))

(define (sqrt-transform x) (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (cubic a b c) (lambda (x) (+ (cube x) (* a (square x) ) (* b x) c) ) )

(define (double a) (compose a a))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f h) 
	(define (work current) (if (= current 1) f (compose f (work (- current 1)))))
	(work h))

(define (smooth f) (lambda (x) (average-triple (f (+ x dx)) (f x) (f (- x dx)))))

(define (smooth-repeat f n) (repeated (smooth f) n))

(define (pow-of-two n) (if (< n 2) 1 (* 2 (pow-of-two (divide n 2)) )))
 
(define (nroot x n) (fixed-point-of-transform (lambda (y) (/ x (pow y (- n 1)))) (repeated average-damp (pow-of-two n)) 1.0))

(define (iterative-improve good-check improve)
	(define (work guess)
	(display guess)
	(newline)
		(let ((next (improve guess)))
		(if (good-check guess next) guess (work next)))) 
	work)
(define (sqrt-improve x) ( (iterative-improve close-enough? (average-damp (lambda (y) (/ x y)))) 1.0) )

(define (remove item sequence) (filter (lambda (x) (not (= x item))) sequence))
