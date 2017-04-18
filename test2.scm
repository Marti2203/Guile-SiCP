(define (identity x) x) 
(define (square x) (* x x))
(define (inc a) (+ 1 a))
(define nil '())

(define (prime? n) (= n (smallest-divisor n)))
(define (smallest-divisor n) (if(divides? 2 n) 2 (find-divisor n 3)))
(define (find-divisor n test-divisor) (cond
					((> (square test-divisor) n) n) 
					((divides? test-divisor n) test-divisor)
					(else (find-divisor n (+ test-divisor 2)))))
(define (divides? a b) (= (remainder b a) 0))

(define (cons-m x y)
	(define (dispatch m)
	(cond 
		((= m 0) x)
		((= m 1) y)
	(else (error "Argument not 0 or 1 -- CONS" m))))
	dispatch)

(define (car-m z) (z 0))
(define (cdr-m z) (z 1))

(define (cons-l x y) (lambda (m) (m x y)))
(define (car-l z) (z (lambda (p q) p)))
(define (cdr-l z) (z (lambda (p q) q)))

(define (pow a b) (if (= b 0) 1 (* a (pow a (- b 1)))))

(define (pow-iter a b)
	(define (work current result)
	(if (= 0 current) result (work (- current 1) (* a result)))
	)
	(work b 1))

(define (cons-mul x y) (* (pow-iter 2 x ) (pow-iter 3 y) ))

(define (multiplier-count number mult)
	(if (not (= (remainder number mult) 0)) 0 (+ 1 (multiplier-count (/ number mult) mult))))

(define (multiplier-count-iter number mult)
	(define (work current result)
		(if (not (= (remainder current mult) 0)) result (work (/ current mult) (+ 1 result))))
	(work number 0))

(define (car-mul z) (multiplier-count-iter z 2))

(define (cdr-mul z) (multiplier-count-iter z 3))

(define (abs x) (if (< x 0) (- x) x))
(define (make-rat n d)
	(let ((g (gcd n d))
	      (nsign (if (< n 0) (- 1) 1))
	      (dsign (if (< d 0) (- 1) 1))
	     )
	     
	     (cons (/ n g dsign) (/ d g dsign))
	))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y)) 
	     	     (* (numer y) (denom x))
		     )
	  	  (* (denom x) (denom y))))
(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
		     (* (numer y) (denom x))
		     )
		  (* (denom x) (denom y))))
(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
		  (* (denom x) (denom y))
		  ))
(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
		  (* (denom x) (numer y))
		  ))
(define (equal-rat? x y)
	(= (* (numer x) (denom y))
	   (* (numer y) (denom x))
	   ))
(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x))
	(newline))
(define (average a b) (/ (+ a b) 2))
(define (make-point x y) (cons x y))
(define (coord-x p) (car p))
(define (coord-y p) (cdr p))
(define (print-point p)
	(newline)
	(display "(")
	(display (coord-x p))
	(display ",")
	(display (coord-y p))
	(display ")")
	(newline))

(define (make-line p1 p2) (cons p1 p2))
(define (get-start l) (car l))
(define (get-end l) (cdr l))
(define (get-mid-coord l coord) (average (coord (get-start l)) (coord (get-end l))))
(define (get-mid l) (make-point (get-mid-coord l coord-x) (get-mid-coord l coord-y) ) )

(define (make-rec left-up-p right-down-p) (cons left-up-p right-down-p))
(define (get-left-up rec) (car rec))
(define (get-right-up rec) (make-point (coord-x (get-right-down rec)) (coord-y (get-left-up rec))))
(define (get-right-down rec) (cdr rec))
(define (get-left-down rec) (make-point (coord-x (get-left-up rec)) (coord-y (get-right-down rec))))
(define (get-distance rec coord) (abs (- (coord (get-left-up rec)) (coord (get-right-down rec)))))
(define (perimeter rec) (* 2 (+ (get-distance rec coord-x) (get-distance rec coord-y))))

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-f x y) (lambda (f) (lambda (z) ((x f) ((y f) z) ))))
(define three (add-f one two))

(define (make-resistor ohms tolerance) (cons ohms tolerance))
(define (get-oms resistor) (car resistor))
(define (get-tolerance resistor) (cdr resistor))
(define (get-medium resistor) (get-oms resistor))
(define (get-maximum resistor) (* (get-oms resistor) (+ 1.0 (get-tolerance resistor) )))
(define (get-minimum resistor) (* (get-oms resistor) (- 1.0 (get-tolerance resistor) )))
(define (get-resistance r1 r2 position) (/ 1 (+ (/ 1 (position r1) ) (/ 1 (position r2) ))))

(define (make-interval-tolerance midpoint tolerance) (make-interval (* midpoint (- 1.0 tolerance)) (* midpoint (+ 1.0 tolerance))))
(define (get-percentage interval) (- (/ (center inderval) (lower-bound interval) ) 1.0))

(define (make-interval start end) (cons start end))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))
(define (add-interval x y) 
		(make-interval 
			(+ (lower-bound x) (lower-bound y)) 
			(+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
	(let 
		((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(define (div-interval x y)
	(if (=0 (width y)) (error "interval divident cannot be 0")
	(mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
		(/ 1.0 (lower-bound y))))))
(define (sub-interval x y)
(make-interval  (- (lower-bound x) (lower-bound y))
		(- (upper-bound x) (upper-bound y))))
(define (in-interval? value interval) (if(and (> value (lower-bound interval) (< value (upper-boudn interval)) )) #t #f ))

(define (width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2 ))
(define (make-center-width c w) (make-interval (- c w) (+ c w)))
(define (center i) (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (list-ref items n) (if (= n 0)(car items)(list-ref (cdr items) (- n 1))))
(define (length items) (if (null? items) 0 (+ 1 (length (cdr items)))))
(define (lenght-iter items)
	(define (work list result)
	(if (null? items) result (work (cdr list) (+ 1 result))))
	(work items 0))

(define (append list1 list2) (if (null? list1) list2 (cons (car list1) (append (cdr list1) list2))))
(define (last-pair list) (if (null? (cdr list)) (car list) (last-pair (cdr list))))
(define (reverse l)
	(define (work current result)
	(if (null? current) result (work (cdr current) (cons (car current) result) )))	
	(work l '())
	)
(define (reverse-rec l) (if (null? l) '() (append (reverse-rec (cdr l)) (list (car l)))))

(define (same-parity-iter e . i)
	(define (work l s)
	(if (null? s) 
		l
		(if (= 
			(remainder e 2) 
			(remainder (car s) 2) 
		    ) 
				(work (append l (list (car s))) (cdr s)) 
				(work l (cdr s)))))
	(work (list e) i)
)

(define (scale-list items factor)
(if (null? items)
nil
(cons (* (car items) factor)
(scale-list (cdr items) factor))))

(define us-coins (list 50 10 25 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount kinds-of-coins)
	(cond 
		((= amount 0) 1)
		((or (< amount 0) (null? kinds-of-coins)) 0)
		(else (+ 
			(cc amount (cdr kinds-of-coins))
			(cc (- amount (car kinds-of-coins)) kinds-of-coins))
		      )
		))

(define (map-my proc items)
	(if (null? items) 
		'()
		(cons 
			(proc (car items)) 
			(map proc (cdr items)))))
(define (square x) (* x x))
(define (scale-list-map items factor) (map (lambda (x) (* x factor)) items))
(define (square-items-map items) (map (lambda (x) (* x x)) items))
(define (square-items items)
(if (null? items) '() (cons (square (car items)) (square-items (cdr items))) ))
(define (for-each proc items) (cond 
				((null? items) #t) 
				(else (proc (car items)) (for-each proc (cdr items)))
			      ))
(define (count-leaves x)
	(cond 
		((null? x) 0)
		((not (pair? x)) 1)
		(else (+ 
			(count-leaves (car x))
			(count-leaves (cdr x))
		      ))))
(define (deep-reverse tree)
	(define (work s)
	 (if (null? s) 
	 	'()
	 	(if (pair? (car s)) 
			(cons (reverse (car s)) (work (cdr s))) 
			(cons (car s) (work (cdr s))))))
	(work (reverse tree)))
(define (leaf? element) (not (pair? element)))

(define (fringe tree)( if (pair? tree) 
		(append (fringe-test (car tree)) 
			(fringe-test (cdr tree))) 
		(if (null? tree) 
			tree 
			(list tree))))

(define x (list (list 2 3) (list 4 5)))

(define (make-mobile left right) (list left right))

(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

(define (mobile? element) (pair? element))

(define (total-weight mobile)
	(if (mobile? mobile)
	(+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile))))
	mobile
	)
)

(define (balanced? mobile)
	(and
	(= 
		(* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
	  	(* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile))))
	)
	(if (mobile? (branch-structure (left-branch mobile))) (balanced? (branch-structure (left-branch mobile))) #t)
	(if (mobile? (branch-structure (right-branch mobile))) (balanced? (branch-structure (right-branch mobile))) #t)
	)
)

(define (scale-tree tree factor)
	(cond 
		((null? tree) nil)
		((not (pair? tree)) (* tree factor))
		(else (cons 
			(scale-tree (car tree) factor)
			(scale-tree (cdr tree) factor))
		)))
(define (scale-tree-map tree factor)
	(map (lambda (sub-tree)
		(if (pair? sub-tree) (scale-tree sub-tree factor) (* sub-tree factor)))
	tree))

(define (square-tree tree)
	(cond 
		((null? tree) nil)
		((not (pair? tree)) (* tree tree))
		(else (cons 
			(square-tree (car tree))
			(square-tree (cdr tree))
		      ))))

(define (square-tree-map tree)
	(map (lambda (sub-tree) (if (pair? sub-tree) (square-tree-map sub-tree) (* sub-tree sub-tree))) tree))

(define (tree-map tree proc)
	(cond 
		((null? tree) nil)
		((not (pair? tree)) (proc tree))
		(else (cons 
			(tree-map (car tree) proc)
			(tree-map (cdr tree) proc))
		)))

(define (tree-map-map tree proc)
	(map (lambda (sub-tree) (if (pair? sub-tree) (tree-map-map sub-tree proc) (proc sub-tree))) tree))

(define (subsets s)
(if (null? s)
	(list nil)
	(let 
		((rest (subsets (cdr s))))
			(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (filter predicate sequence)
	(cond 
		((null? sequence) nil)
		((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
	(if (> low high)
		nil
		(cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
	(cond 
		((null? tree) nil)
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))(enumerate-tree (cdr tree))))))

(define (map-acc p list) (accumulate (lambda (x y) (cons (p x) y)) nil list))

(define (append-acc seq1 seq2) (accumulate cons seq2 seq1))
(define (length-acc sequence) (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(define (horner-eval x cofficient-sequence) (accumulate (lambda (this-coff higher-terms) (+ this-coff (* x higher-terms))) 0 cofficient-sequence))
(define (count-leaves-acc t) (accumulate (lambda (element tree) (+ 1 tree)) 0 (enumerate-tree t) ))

(define (count-leaves-book t) (accumulate (lambda (element rest) (+ 1 rest)) 0 (map (lambda (x) ( ) )  t ) ))
(define that (list 1 (list 2 (list 3 4)) 5) )
(define (accumulate-n op init seqs) (if (null? (car seqs)) nil (cons (accumulate op init (map (lambda (x) (car x)) seqs) )
								     (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define (dot-product v w) (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v) (map (lambda (list) (dot-product list v)) m))
(define (transpose mat) (accumulate-n (lambda (x y) (cons x y)) '() mat))
(define (matrix-*-matrix m n) (let  ((cols (transpose n))) (map (lambda (list) (matrix-*-vector cols list)) m)))

(define (fold-left op initial sequence)
	(define (iter result rest)
	(if (null? rest) result (iter (op result (car rest)) (cdr rest))))
	(iter initial sequence))

(define fold-right accumulate)

(define (reverse-right sequence) (fold-right (lambda (x y) (append y (list x) ) ) nil sequence))
(define (reverse-left sequence) (fold-left (lambda (x y) (cons y x) ) nil sequence))

(define (prime-sum? pair) (prime? (+ (car pair) (cadr pair))))

(define (pairs n) (accumulate append nil
(map (lambda (i) (map 
	(lambda (j) (list i j)) 
		(enumerate-interval 1 (- i 1))))
	(enumerate-interval 1 n))))

(define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (flatmap proc seq) (accumulate append nil (map proc seq)))

(define (prime-sum-pairs n) (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(define (permutations s)
	(if (null? s) (list nil)
	(flatmap (lambda (x)
		(map (lambda (p) (cons x p))
		(permutations (remove x s)))) s)))

(define (unique-pairs n) (flatmap (lambda (i) (map (lambda (j) (if (= i j) '() (list j i))) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(define (triplets-sum n s) (filter (lambda (x) (if (= (+ (car x) (car (cdr x)) (car (cdr (cdr x)))) s) #t #f )) (unique-trips n)))

(define (unique-pairs n) (flatmap (lambda (i) (map (lambda (j) (if (= i j) '() (list j i))) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))
(define (unique-trips n) (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (if (or (= i k) (= j k) (= i j)) '() (list k j i))) (enumerate-interval 1 (- j 1)))) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(define empty-board)
(define (queens board-size)
	(define (queen-cols k)
	(if (= k 0) 
		(list empty-board)
		(filter (lambda (positions) (safe? k positions))
			(flatmap 
				(lambda (rest-of-queens)
				(map (lambda (new-row)
					(adjoin-position new-row k rest-of-queens))
				(enumerate-interval 1 board-size)))
	(queen-cols (- k 1))))))
(queen-cols board-size))

(define (up-split painter n)
	(if (= n 0) 
	painter
	(let 
		((smaller (up-split painter (- n 1))))
		(below painter (beside smaller smaller)))))
(define (split placement combine)
	(define (work painter n)
	(if (= n 0) 
	painter
	(let 
		((smaller (work painter (- n 1))))
		(placement painter (combine smaller smaller)))))
	work
		)

(define (frame-coord-map frame)
	(lambda (v)
	(add-vect
		(origin-frame-l frame)
			(add-vect 
				(scale-vect (xcord v)
				(edge1-frame-l frame))
		(scale-vect (ycord v) (edge2-frame-l frame))))))



(define (make-vect x y) (cons x y) )
(define (xcord vec) (car vec))
(define (ycord vec) (cdr vec))
(define (add-vect vec1 vec2) (make-vect (+ (xcoord vec1) (xcoord vec2)) (+ (ycoord vec1) (ycoord vec2))))
(define (sub-vect vec1 vec2) (maKe-vect (- (xcoord vec1) (xcoord vec2)) (- (ycoord vec1) (ycoord vec2))))
(define (scale-vect vec scale) (make-vect (* scale (xcoord vec)) (* scale (ycoord vec))))

(define (make-frame-l origin edge1 edge2)
(list origin edge1 edge2))

(define (edge2-frame-l frame) (car (cdr (cdr frame))))
(define (edge1-frame-l frame) (car (cdr frame)))
(define (origin-frame-l frame) (car frame))

(define (origin-frame-c frame) (car frame))
(define (edge1-frame-c frame)  (car (cdr frame)))
(define (edge2-frame-c frame) (cdr (cdr frame)))

(define (make-frame-c origin edge1 edge2)
(cons origin (cons edge1 edge2)))

(define (segments->painter segment-list)
	(lambda (frame)
		(for-each 
		(lambda (segment)
		(draw-line 
			 ((frame-coord-map frame) (start-segment segment)) 
			 ((frame-coord-map frame) (end-segment segment)))) 
		segment-list)))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (draw-line start end) (display start) (display " - ") (display end) (newline) )

(define (flipped-pairs painter)
	(let 
		((painter2 (beside painter (flip-vert painter))))
	(below painter2 painter2)))

(define (right-split painter n)
	(if (= n 0)
	painter
	(let 
		((smaller (right-split painter (- n 1))))

	(beside painter (below smaller smaller)))))

(define (up-split painter n)
	(if (= n 0)
	painter
	(let
		((smaller (up-split painter (- n 1))))
		(below painter (beside smaller smaller))
	)))
(define (corner-split painter n)
	(if (= n 0)
	painter
	(let 
		((up (up-split painter))
		(right (right-split painter)))
	(let 
		((top-left up)
		(bottom-right right)
		(corner (corner-split painter (- n 1))))
	(beside (below painter top-left)
	(below bottom-right corner))))))

(define (square-limit painter n)
	(let 
		((quarter (corner-split painter n)))
	(let 
		((half (beside (flip-horiz quarter) quarter)))
	(below (flip-vert half) half))))

(define square (segments->painter (list 
					(make-segment (make-vect 0 0) (make-vect 0 1)) 
					(make-segment (make-vect 0 0) (make-vect 1 0))
					(make-segment (make-vect 0 1) (make-vect 1 1))
					(make-segment (make-vect 1 0) (make-vect 1 1)))))

(define diamond (segments->painter (list 
					(make-segment (make-vect 0 0.5) (make-vect 0.5 0.5)) 
					(make-segment (make-vect 0 0.5) (make-vect 0.5 0))
					(make-segment (make-vect 1 0.5) (make-vect 0.5 0))
					(make-segment (make-vect 1 0.5) (make-vect 0.5 0.5)))))
(define ex (segments->painter (list 
				(make-segment (make-vect 0 0) (make-vect 1 1))
				(make-segment (make-vect 1 0) (make-vect 0 1)))))
(define wave (segments->painter (list 
				(make-segment (make-vect 0.00 0.80) (make-vect 0.21 0.60)) ;left arm upper left 
				(make-segment (make-vect 0.00 0.60) (make-vect 0.29 0.30)) ;left arm lower left
				(make-segment (make-vect 0.21 0.60) (make-vect 0.36 0.65)) ;left arm upper left
				(make-segment (make-vect 0.36 0.65) (make-vect 0.57 0.65)) ;left arm upper to head 
				(make-segment (make-vect 0.36 0.57) (make-vect 0.29 0.30)) ;left arm lower right
				(make-segment (make-vect 0.36 0.57) (make-vect 0.57 0.57)) ;left arm lower to feet
				(make-segment (make-vect 0.57 0.65) (make-vect 0.42 0.90)) ;left head lower
				(make-segment (make-vect 0.42 0.90) (make-vect 0.57 1.00)) ;left head upper
				(make-segment (make-vect 0.57 0.57) (make-vect 0.50 0.40)) ;left foot upper
				(make-segment (make-vect 0.50 0.40) (make-vect 0.42 0.00)) ;left foot lower
				(make-segment (make-vect 0.57 0.00) (make-vect 0.64 0.25)) ;left foot right
				(make-segment (make-vect 0.64 0.25) (make-vect 0.71 0.00)) ;right foot left
				(make-segment (make-vect 0.78 0.00) (make-vect 0.71 0.25)) ;right foot right
				(make-segment (make-vect 0.71 0.25) (make-vect 1.00 0.10)) ;right hand lower
				(make-segment (make-vect 1.00 0.30) (make-vect 0.78 0.65)) ;right hand upper
				(make-segment (make-vect 0.78 0.65) (make-vect 0.64 0.65)) ;right hand to head 
				(make-segment (make-vect 0.64 0.65) (make-vect 0.72 0.90)) ;right head lower
				(make-segment (make-vect 0.72 0.90) (make-vect 0.64 1.00)) ;right head upper
				(make-segment (make-vect 0.50 0.90) (make-vect 0.50 0.90)) ;left eye
				(make-segment (make-vect 0.64 0.90) (make-vect 0.64 0.90)) ;right eye, we come one....
				(make-segment (make-vect 0.50 0.85) (make-vect 0.57 0.80)) ;left mouth
				(make-segment (make-vect 0.57 0.80) (make-vect 0.64 0.85)) ;right mouth
				)))

(define (transform-painter painter origin corner1 corner2)
	(lambda (frame)
		(let ((m (frame-coord-map frame)))
			(let ((new-origin (m origin)))
				(painter (make-frame new-origin (sub-vect (m corner1) new-origin) (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
(transform-painter painter (make-vect 0.0 1.0) (make-vect 1.0 1.0) (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
(transform-painter painter (make-vect 0.5 0.5) (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(define (beside painter1 painter2)
	(let ((split-point (make-vect 0.5 0.0)))
	(let 
		((paint-left
		(transform-painter painter1
				(make-vect 0.0 0.0)
				split-point
				(make-vect 0.0 1.0)))
		(paint-right
		(transform-painter painter2
			split-point
			(make-vect 1.0 0.0)
			(make-vect 0.5 1.0))))
	(lambda (frame) (paint-left frame) (paint-right frame)))))
(define (flip-horiz painter)
	(transform-painter painter (make-vect 1.0 0.0) (make-vect 1.0 1.0) (make-vect 0.0 0.0)))
(define (rot180 painter)
	(transform-painter painter (make-vect 1.0 1.0) (make-vect 1.0 0.0) (make-vect 0.0 1.0)))
(define (rot270-clock painter)
	(transform-painter painter (make-vect 0.0 1.0) (make-vect 1.0 1.0) (make-vect 0.0 0.0)))
(define (rot270-anti painter)
	(transform-painter painter (make-vect 1.0 0.0) (make-vect 0.0 0.0) (make-vect 1.1 1.1)))
(define (below-t painter1 painter2)
	(let ((split-point (make-vect 0.0 0.5)))
	(let 
		((paint-up
		(transform-painter painter1
			split-point
			(make-vect 0.0 1.0)
			(make-vect 1.0 0.5)))

		(paint-down
		(transform-painter painter2
			(make-vect 0.0 0.0)
			split-point
			(make-vect 0.0 1.0))))
	(lambda (frame) (paint-up frame) (paint-down frame)))))

(define (below-b painter1 painter2)
	(beside (rot180 (rot270-anti painter1)) (rot180 (rot270-anti painter2))))

(define (square-limit painter n)
	(let 
		((quarter (corner-split painter n)))
	(let 
		((half (beside (flip-horiz quarter) quarter)))
	(below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
	(lambda (painter)
	(let 
		((top (beside (tl painter) (tr painter)))
		(bottom (beside (bl painter) (br painter))))
	(below bottom top))))

(define (square-limit-s painter n) (let ((quarter (corner-split painter n)))  (square-of-four (flip-horiz quarter) quarter (flip-vert (flip-horiz quarter)) (flip-vert quarter))))


(define (sub-cont item l) 
	(cond 
	((null? l) #f)
	((eq? (car l) item) l)
	(else (sub-cont item (cdr l)))))

(define (symbol? n) (not (pair? n)))

(define (equal? L1 l2)
	(if (and (symbol? L1) (symbol? l2)) (eq? L1 l2)

	(if (not (eq? (length L1) (length l2))) #f
	(cond
		((and (null? L1) (null? l2)) #t)
		((and (list? (car L1)) (list? (car l2))) (and (equal? (car L1) (car l2)) (equal? (cdr L1) (cdr l2))))
		((and (symbol? (car L1)) (symbol? (car l2))) (and (eq? (car L1) (car l2)) (equal? (cdr L1) (cdr l2))))
	(else #f)))))

(define (eq-sym? s1 s2)
	(and (symbol? s1) (symbol? s2) (eq? s1 s2)))


(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
	(cond 
		((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-sum-s a1 a2 . rest)
	;(display rest)
	(let
	((rest-real (cond
			((null? rest) rest)
			((list? (car rest)) (car rest))
			(else rest))))
	(cond
		((null? a2) a1)
		((and (number? a1) (number? a2)) (if (null? rest-real) (+ a1 a2) (make-sum-s (+ a1 a2) (car rest-real) (cdr rest-real))))
		((not (number? a1)) (list '+ a1 (if (null? rest-real) a2 (make-sum-s a2 (car rest-real) (cdr rest-real)))))
		((not (number? a2)) (list '+ a2 (if (null? rest-real) a1 (make-sum-s a1 (car rest-real) (cdr rest-real)))))
	)))

(define (make-product-s a1 a2 . rest)
	(let
		((rest-real (cond
			((null? rest) rest)
			((list? (car rest)) (car rest))
			(else rest))))
	(cond
		((null? a2) a1)
		((and (number? a1) (number? a2)) (if (null? rest-real) (* a1 a2) (make-product-s (* a1 a2) (car rest-real) (cdr rest-real))))
		((not (number? a1)) (list '* a1 (if (null? rest-real) a2 (make-product-s a2 (car rest-real) (cdr rest-real)))))
		((not (number? a2)) (list '* a2 (if (null? rest-real) a1 (make-product-s a1 (car rest-real) (cdr rest-real)))))
	)))


(define (make-product m1 m2)
	(cond 
		((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


(define (exponentiation? x) (and (pair? x) (eq? (car x) '** )))
(define (base exp) (cadr exp))
(define (power exp) (caddr exp))

(define (make-exp a1 a2)
	(cond 
		((=number? a2 0) 1)
		((=number? a1 1) 1)
		((and (number? a1) (number? a2)) (pow a1 a2))
	(else (list '** a1 a2))))

(define (deriv-s exp var)
	(cond 
	((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) 
		(make-sum-s 
			(deriv-s (addend exp) var) 
			(deriv-s (augend exp) var)))
	((product? exp)
		(make-sum-s
		(make-product-s
			(multiplier-s exp)
			(deriv-s (multiplicand exp) var))
		(make-product-s
			(deriv-s (multiplier exp) var)
			(multiplicand-s exp))))
	((exponentiation? exp)
	(make-product-s
		(power exp)
		(make-product-s 
			(base exp)  
			(deriv-s (base exp) var))))

	(else (error "unknown expression type -- DERIV" exp))))


(define (deriv exp var)
	(cond 
	((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) 
		(make-sum 
			(deriv (addend exp) var) 
			(deriv (augend exp) var)))
	((product? exp)
		(make-sum
		(make-product 
			(multiplier exp)
			(deriv (multiplicand exp) var))
		(make-product 
			(deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	(make-product 
		(power exp)
		(make-product 
			(base exp)  
			(deriv (base exp) var))))


	(else (error "unknown expression type -- DERIV" exp))))

(define (make-sum-i a b)
	(cond
	((and (number? a) (number? b)) (+ a b))	
	(else (list a '+ b))
	))
(define (sum-i? s) (and (list? s) (eq? (cadr s) '+)))
(define (addend-i s) (car s))
(define (augend-i s) (caddr s))

(define (make-product-i a b)
	(cond
	((and (number? a) (number? b)) (* a b))
	(else (list a '* b))
	))
(define (prod-i? s) (and (list? s) (eq? (cadr s) '*) ))
(define (multiplier-i s) (car s))
(define (multiplicand-i s) (caddr s))

(define (make-pow-i a b)
	(cond
	((and (number? a) (number? b)) (pow a b))
	(else (list a '** b))
	))
(define (pow-i? s) (and (list? s) (eq? (cadr s) '**)))
(define (base-i s) (car s))
(define (power-i s) (caddr s))

(define (deriv-i exp var)
	(cond 
	((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum-i? exp) 
		(make-sum-i
			(deriv-i (addend-i exp) var) 
			(deriv-i (augend-i exp) var)))
	((prod-i? exp)
		(make-sum-i
		(make-product-i 
			(multiplier-i exp)
			(deriv-i (multiplicand-i exp) var))
		(make-product-i
			(deriv-i (multiplier-i exp) var)
			(multiplicand-i exp))))
	((exponentiation-i? exp)
	(make-product-i 
		(power-i exp)
		(make-product-i 
			(base exp)  
			(deriv-u (base exp) var))))


	(else (error "unknown expression type -- DERIV" exp))))

(define (contains? l s)
	(cond 
		((null? s) #f) 
		((eq? l (car s)) #t) 
		(else (contains? l (cdr s)))
	))

(define (last s) (if (null? (cdr s)) (car s) (last (cdr s))))

(define (make-sum-a a1 a2)
	(cond
	((and (number? a1) (number? a2)) (+ a1 a2))
	((expression? a1) (if (not (sum-a? a1)) (append a1 (list '+ a2)) (if (and (number? a2) (has-number? a1)) (let ((number (number-exp a1))) (replace (+ number a2) number a1)) (append a1 (list '+ a2)))))
	((expression? a2) (if (not (sum-a? a2)) (append a2 (list '+ a1)) (if (and (number? a1) (has-number? a2)) (let ((number (number-exp a2))) (replace (+ number a1) number a2)) (append a2 (list '+ a1)))))
	(else (list a1 '+ a2)) 
	))
(define (make-product-a a1 a2)
	(cond
	((and (number? a1) (number? a2)) (* a1 a2))
	((expression? a1) (if (not (multiplication-a? a1)) (list a2 '* a1) (if (and (number? a2) (has-number? a1)) (let ((number (number-exp a1))) (replace (* number a2) number a1)) (append a1 (list '* a2)))))
	((expression? a2) (if (not (multiplication-a? a2)) (list a1 '* a2) (if (and (number? a1) (has-number? a2)) (let ((number (number-exp a2))) (replace (* number a1) number a2)) (append a2 (list '* a1)))))
	(else (list a1 '* a2)) 
	))
(define (addend-a s) (let ((index (index-of '+ s))) (if (= index (- 1)) '() (list-ref s (- index 1)))))
(define (augend-a s) (let ((index (index-of '+ s))) (if (= index (- 1)) '() (list-ref s (+ index 1)))))
(define (multiplier-a s) (let ((index (index-of '* s))) (if (= index (- 1)) '() (list-ref s (- index 1)))))
(define (multiplicand-a s) (let ((index (index-of '* s))) (if (= index (- 1)) '() (list-ref s (+ index 1)))))
(define (multiplication-a? s) (contains? '* s))
(define (sum-a? s) (contains? '+ s))
(define (product-a? s) (contains? '* s))
(define (exponentian-a? s) (contains? '** s))

(define (deriv-a exp var)
	(cond 
	((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum-a? exp) 
		(make-sum-a
			(deriv-a (addend-a exp) var) 
			(deriv-a (augend-a exp) var)))
	((product-a? exp)
		(make-sum-a
		(make-product-a 
			(multiplier-a exp)
			(deriv-a (multiplicand-a exp) var))
		(make-product-a
			(deriv-a (multiplier-a exp) var)
			(multiplicand-a exp))))
	((exponentiation-a? exp)
	(make-product-a
		(power-a exp)
		(make-product-a 
			(base exp)  
			(deriv-a (base exp) var))))


	(else (error "unknown expression type -- DERIV" exp))))
(define (replace i f l)
	(cond 
	((null? l) '())
		((eq? f (car l)) (cons i (cdr l)))
	(else (cons (car l) (replace i f (cdr l))))
	))

(define (replace-i i rep l)
	(define (work current rest)
	(cond
		((> current i) rest)
		((= current i) (cons rep (cdr rest)))
	(else (cons (car rest) (work (+ 1 current) (cdr rest) )))))
	(work 0 l))

(define (index-of s l)
	(define (work current rest)
	(cond
		((null? rest) (- 1))
		((eq? s (car rest)) current)
	(else (work (+ 1 current) (cdr rest) ))
	))	
	(work 0 l))
(define (number-exp exp) (car (filter (lambda (x) (number? x)) (elements exp))))
(define (has-number? exp) (> (length (filter (lambda (x) (number? x)) (elements exp))) 0))
(define (elements exp) (filter (lambda (x) (not (or (eq? x '+) (eq? x '*) (eq? x '**) ))) exp))
(define (expression? s) (not (variable? s)))


(define (element-of-set? x set)
	(cond
	((null? set) #f)
		((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (if (element-of-set? x set) set (cons x set)))

(define (intersection-set set1 set2)
	(cond 
		((or (null? set1) (null? set2) ) '() )
		((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2) ))
	(else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) (if (null? set2) set1 (union-set (adjoin-set (car set2) set1) (cdr set2) )))

(define (union-set-mine set1 set2) (accumulate cons set1 (filter (lambda (x) (element-of-set? x set1)) set2)))
(define (intersection-set-mine set1 set2) (filter (lambda (x) (element-of-set? x set2)) set1))


(define (element-of-ordered-set? x set)
	(cond 
		((null? set) false)
		((= x (car set)) true)
		((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set-ordered set1 set2)
	(if (or (null? set1) (null? set2)) 
	'()
	(let 
		(
		(x1 (car set1)) 
		(x2 (car set2))
		)
	(cond 
	((= x1 x2) (cons x1 (intersection-set-ordered (cdr set1) (cdr set2))))
	((< x1 x2) (intersection-set-ordered (cdr set1) set2))
	((< x2 x1) (intersection-set-ordered set1 (cdr set2)))))))

(define (adjoin-ordered-set set l)
	(cond
	((null? set) (list l))
	((= l (car set)) set)
	((< l (car set)) (cons l set))
	(else (cons (car set) (adjoin-ordered-set (cdr set) l)))
	))

(define (union-ordered-set set1 set2)
	(cond
	((null? set2) set1)
	((null? set1) set2)
	(else (let
		(
		(x1 (car set1))
		(x2 (car set2))
		)
		(cond
		((= x1 x2) (cons x1 (union-ordered-set (cdr set1) (cdr set2) )) )
		((> x1 x2) (cons x2 (cons x1 (union-ordered-set (cdr set1) (cdr set2)))))
		((< x1 x2) (cons x1 (cons x2 (union-ordered-set (cdr set1) (cdr set2)))))
		)
	))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
	(cond 
	((null? set) false)
	((= x (entry set)) true)
	((< x (entry set)) (element-of-set? x (left-branch set)))
	((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set-t x set)
	(cond 
	((null? set) (make-tree x '() '	() ))
	((= x (entry set)) set)
	((< x (entry set))
		(make-tree 
			(entry set)
			(adjoin-set-t x (left-branch set))
			(right-branch set)))
	((> x (entry set))
		(make-tree 
			(entry set)
			(left-branch set)
			(adjoin-set-t x (right-branch set))))))


(define (tree->list-1 tree)
	(if (null? tree)
	'()
	(append 
		(tree->list-1 (left-branch tree))
		(cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
	(if (null? tree)
	result-list
	(copy-to-list 
		(left-branch tree)
		(cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
	(copy-to-list tree '()))

(define (make-empty-node s) (make-tree s '() '()))
(define test-t (make-tree 5 (make-tree 3 (make-empty-node 1) '()) (make-tree 9 (make-empty-node 7) (make-empty-node 11))))
(define test-l (make-tree 1 '() (make-tree 2 '() (make-tree 3 '() (make-tree 4 '() (make-tree 5 '() (make-tree 6 '() (make-tree 7 '() '() ))))))))

(define (list->tree elements) (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
	(if (= n 0)
	(cons '() elts)
	(let ((left-size (quotient (- n 1) 2)))
		(let ((left-result (partial-tree elts left-size)))
			(let 
				(
				(left-tree (car left-result))
				(non-left-elts (cdr left-result))
				(right-size (- n (+ left-size 1)))
				)
				(let 
					(
					(this-entry (car non-left-elts))
					(right-result (partial-tree (cdr non-left-elts) right-size))
					)
					(let 
						(
						(right-tree (car right-result))
						(remaining-elts (cdr right-result))
						)
				(cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

(define (union-ordered-list-tree tree1 tree2) (list->tree (union-ordred-list (tree->list-1 tree1) (tree->list-2 tree2) )))
(define (intersection-list-tree tree1 tree2) (list->tree (intersection-ordred-list (tree->list-1 tree1) (tree->list-2 tree2) )))

(define (union-ordered-list list1 list2) (if (null? list2) list1 (union-ordered-list (insert-ordered (car list2) list1) (cdr list2) )))
(define (intersection-ordered-list list1 list2) (if (null? list2) list1 (intersection-ordered-list (remove-ordered (car list2) list1) (cdr list2))))

(define (insert-ordered element l)
	(cond 
	((null? l) (list element))
	((= element (car l)) l)
	((< element (car l)) (cons element l) )
	((> element (car l)) (cons (car l) (insert-ordered element (cdr l))))
	))

(define (remove-ordered element l)
	(cond
	((null? l) '() )
	((= element (car l)) (cdr l))
	((> element (car l)) l)
	((< element (car l)) (cons (car l) (remove-ordered element (cdr l))))
	))

(define (key node) (entry node))

(define (lookup k tree)
	(cond
	((null? tree) #f)
	((= k (key tree)) (entry tree))
	((< k (key tree)) (left-branch tree))
	((> k (key tree)) (right-branch tree))
	))


(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
(list 
	left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
	(if (leaf? tree)
	(list (symbol-leaf tree))
	(caddr tree)))
(define (weight tree)
	(if (leaf? tree)
	(weight-leaf tree)
	(cadddr tree)))

(define (decode bits tree)
	(define (decode-1 bits current-branch)
	(if (null? bits)
		'()
		(let 
		((next-branch (choose-branch (car bits) current-branch)))
		(if (leaf? next-branch)
			(cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
			(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))
(define (choose-branch bit branch)
(cond 
	((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (adjoin-set-weight x set)
(cond 
	((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	((= (weight x) (weight (car set))) (if (equal? (symbols x) (symbols (car set))) set (cons x set) ))
	(else (cons (car set) (adjoin-set-weight x (cdr set))))))

(define (make-leaf-set pairs)
	(if (null? pairs)
	'()
	(let 
		((pair (car pairs)))
	(adjoin-set-weight (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))


(define sample-tree
(make-code-tree (make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree 
				(make-leaf 'D 1)
				(make-leaf 'C 1)))))

(define sample-pairs (list '(A 4) 
			   '(B 2) 
			   '(C 1) 
			   '(D 1) 
			   ))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode-symbol symbol tree)
	(if (leaf? tree) '()
	(if (contains? symbol (symbols (left-branch tree)))
		(cons 0 (encode-symbol symbol (left-branch tree)))
		(cons 1 (encode-symbol symbol (right-branch tree)))
	)))

(define (encode message tree) (if (null? message) '() (append (encode-symbol (car message) tree) (encode (cdr message) tree))))

(define (successive-merge set)
	(if (= (length set) 2) (if (leaf? (car set)) (make-code-tree (car set) (cadr set)) (make-code-tree (cadr set) (car set)))
	(let
		(
		(first (car set)) 
		(second (cadr set)) 
		(rest (cddr set))
		)
	(successive-merge (adjoin-set-weight (if (leaf? second) (make-code-tree second first) (make-code-tree first second)) rest))
	)))
(define (generate-huffman-tree pairs) (successive-merge (make-leaf-set pairs)))

(define rock-symbols '( (A 2) (BOOM 1) (NA 16) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1) ))

(define (make-from-real-imag-m real-part imaginary-part) (list 'real-imag real-part imaginary-part))
(define (make-from-ang-mag-m angle magnitude) (list 'ang-mag angle magnitude))

(define (real-imag-m? comp) (eq? 'real-imag (car comp)))
(define (ang-mag-m? comp) (eq? 'ang-mag (car comp)))

(define (magnitude-m comp) 
	(cond
	((and-mag-m? comp) (caddr comp))
	((real-imag-m? comp) (sqrt (+ (square (real-part-m comp)) (square (imag-part-m comp)))))
	(else (error "INVALID TYPE FOR MAGNITUDE"))
	))
(define (angle-m comp) 
	(cond  
	((ang-mag-m? comp) (caddr comp))
	((real-imag-m? comp) (atan (real-part-m comp) (imag-part-m comp)))
	(else (error "INVALID TYPE FOR ANGLE"))
	))
(define (real-part-m comp) 
	(cond 
	((real-imag-m? comp) (cadr comp)) 
	((ang-mag-m? comp) (* (magnitude-m comp) (cos (angle-m comp))))
	(else (error "INVALID TYPE FOR REAL"))
	))

(define (imag-part-m comp) 
	(cond 
	((real-imag-m? comp) (caddr comp))
	((ang-mag-m? comp) (* (magnitude-m comp) (sin (angle-m comp)) ))
	(else (error "INVALID TYPE FOR IMAGINARY"))
	))

(define (add-complex-m z1 z2)
(make-from-real-imag-m
	(+ (real-part-m z1) (real-part-m z2))
	(+ (imag-part-m z1) (imag-part-m z2))
))
(define (sub-complex-m z1 z2)
(make-from-real-imag-m
	(- (real-part-m z1) (real-part-m z2))
	(- (imag-part-m z1) (imag-part-m z2))
))
(define (mul-complex-m z1 z2)
(make-from-mag-ang-m
	(* (magnitude-m z1) (magnitude-m z2))
	(+ (angle-m z1) (angle-m z2))

))
(define (div-complex-m z1 z2)
(make-from-mag-ang-m
	(/ (magnitude-m z1) (magnitude-m z2))
	(- (angle-m z1) (angle-m z2))
))


(define (attach-tag type-tag contents) (if (number? contents) contents (cons type-tag contents)))

(define (type-tag datum)
(cond 
	((integer? datum) 'scheme-number)
	((real? datum) 'real)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
(cond
	((number? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum -- CONTENTS" datum))))

(define (install-rectangular-package)
;; internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
	(define (angle z) (atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
;; interface to the rest of the system
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part)
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a)))) 
'done)

(define (install-polar-package)
;; internal procedures
	(define (magnitude z) (car z))
 	(define (angle z) (cdr z))
	(define (make-from-mag-ang r a) (cons r a))
	(define (real-part z) (* (magnitude z) (cos (angle z))))
	(define (imag-part z) (* (magnitude z) (sin (angle z))))
	(define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
;; interface to the rest of the system
	(define (tag x) (attach-tag 'polar x))
	(put 'real-part '(polar) real-part)
	(put 'imag-part '(polar) imag-part)
	(put 'magnitude '(polar) magnitude)
	(put 'angle '(polar) angle)
	(put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
		(if proc (apply proc (map contents args))
		(if (= (length args) 2)
			(let 
			((type1 (car type-tags))
			(type2 (cadr type-tags))
			(a1 (car args))
			(a2 (cadr args)))
				(let 
				((t1->t2 (get-coercion type1 type2))
				(t2->t1 (get-coercion type2 type1)))
			(cond
			((eq? type1 type2) (error "No method for this type" (list type1 op type-args)))
			(t1->t2 (apply-generic op (t1->t2 a1) a2))
			(t2->t1 (apply-generic op a1 (t2->t1 a2)))
			(else (error "No method for these types" (list op type-tags))))))
		(error "No method for these types" (list op type-tags)))))))

(define (apply-generic-r op . args)
	(let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
		(if proc (apply proc (map contents args)) 
		(let ((highest (list-ref (find-highest type-tags) tower))) 
		(let ((operation ((get op (map (lambda (x) highest) type-tags)))))
		(if operation (apply operation (map contents args))
		(error "No method for these types" (list op type-tags)))))))))

(define (apply-generic-d op . args)
	(let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
		(if proc (apply proc (map contents args)) 
		(let ((highest (list-ref (find-highest type-tags) tower))) 
		(let ((operation ((get op (map (lambda (x) highest) type-tags)))))
		(if operation (drop (apply operation (map contents args)) )
		(error "No method for these types" (list op type-tags)))))))))


(define (apply-generic-m op . args)
	(let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
		(if proc (apply proc (map contents args))
		(let ((coercions (try-coercions args 1))) 
		(if coercions ((get op (type-tag (car coercions))) (map1-1 arg coercions))
		(error "No method for these types" (list op type-tags))))))))

(define (find-highest list)
	(define (work rest current)
	(if (null? rest) current (let ((next (index-of (car rest) tower))) (work (cdr rest) (if (> next current) next current)))))
	(work list 0))   

(define (try-coercions types index)
	(let ((current (list-ref types index)))
	((let ((coercions (map (lambda (x) (get-coercion x current)))) (size (length types)) ) 
	(if (= size index) #f (if (= (length types) (length coercions)) coercions (try-coercions types (+1 index))))))))

(define (map1-1 list procs)
	(if (null? list) '() (cons ((car proc) (car list)) (map1-1 (cdr list) (cdr procs)))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add-complex z1 z2)
(make-from-real-imag 
	(+ (real-part z1) (real-part z2))
	(+ (imag-part z1) (imag-part z2))
))
(define (sub-complex z1 z2)
(make-from-real-imag 
	(- (real-part z1) (real-part z2))
	(- (imag-part z1) (imag-part z2))
))
(define (mul-complex z1 z2)
(make-from-mag-ang 
	(* (magnitude z1) (magnitude z2))
	(+ (angle z1) (angle z2))

))
(define (div-complex z1 z2)
(make-from-mag-ang 
	(/ (magnitude z1) (magnitude z2))
	(- (angle z1) (angle z2))
))


(define (deriv-d exp var)
	(cond 
	((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (install-deriv-def-package)
	(define (make-sum a1 a2) (if (and (number? a1) (number? a2)) (+ a1 a2) (cons a1 a2)))
	(define (make-prod a1 a2) (if (and (number? a1) (number? a2)) (* a1 a2) (cons a1 a2)))
	(define (make-pow a1 a2) (if (and (number? a1) (number? a2)) (pow a1 a2) (cons a1 a2)))
	(define (base s) (car s))
	(define (power s) (cdr s))
	(define (addend s) (car s))
	(define (augend s) (cdr s))
	(define (multiplier s) (car s))
	(define (multiplicant s) (cdr s))

	(define signature 'deriv)
	(define (tag x) (attach-tag signature x))

	(put signature '+ (lambda (exp var) (attach-tag '+ (make-sum (deriv-d (addend exp) var) (deriv-d (augend exp) var)))))
	(put signature '* (lambda (exp var) (attach-tag '* (make-sum (make-prod (multiplier exp) (deriv-d (multiplicand exp) var)) (make-prod (multiplicand exp) (deriv-d exp) var)))))
	(put signature '** (lambda (exp var) (attach-tag '** (make-prod (power exp) (make-prod (base exp) (deriv-d (base exp) var))))))
	(put signature 'multiplier mutiplier)
	(put signature 'multiplicant multiplicant)
	(put signature 'addend addend)
	(put signature 'augend augend)
'done)

(define (make-from-real-imag-d x y)
	(define (dispatch op)
	(cond 
	((eq? op 'real-part) x)
	((eq? op 'imag-part) y)
 	((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
	((eq? op 'angle) (atan y x))
	(else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
dispatch)

(define (make-from-mag-ang-d magnitude angle)
	(define (displatch op)
	(cond
	((eq? op 'real-part) (* magnitude (sin angle)))
	((eq? op 'imag-part) (* magnitude (cos angle)))
	((eq? op 'angle) angle)
	((eq? op 'magnitude) magnitude)
	(else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
dispatch)

(define (apply-generic-d op arg) (arg op))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
	(define (reduce n d)
	(let ((g (gcd n d)))
	(list (/ n g) (/ d g))))
	(put 'add '(scheme-number scheme-number) (lambda (x y) (+ x y)))
	(put 'sub '(scheme-number scheme-number) (lambda (x y) (- x y)))
	(put 'mul '(scheme-number scheme-number) (lambda (x y) (* x y)))
	(put 'div '(scheme-number scheme-number) (lambda (x y) (quotient x y)))
	(put 'make 'scheme-number (lambda (x) x))
	(put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
	(put '=zero? 'scheme-number (lambda (x) (= x 0)))
	(put 'neg 'scheme-number (lambda (x) (- x)))
	(put 'gcd '(scheme-number scheme-number) gcd)
	(put 'reduce '(scheme-number scheme-number) reduce)
	(put-coercion 'scheme-number 'scheme-number identity)
'done)

(define (install-real-package)
	(put 'add '(real real) (lambda (x y) (+ x y)))
	(put 'sub '(real real) (lambda (x y) (- x y)))
	(put 'mul '(real real) (lambda (x y) (* x y)))
	(put 'div '(real real) (lambda (x y) (/ x y)))
	(put 'make 'real (lambda (x) x))
	(put 'equ? '(real real) (lambda (x y) (= x y)))
	(put '=zero? 'real (lambda (x) (= x 0)))
	(put 'neg 'real (lambda (x) (- x)))
	(put-coercion 'real 'real identity)
'done)


(define (install-connecting-arithmetic-package)
	(define (scheme-number->complex n) (make-complex-from-real-imag n 0))
	(define (scheme-number->rational n) (make-rational n 1))
	(define (complex->real n) (if (= (imag-part n) 0) (* 1.0 (real-part n)) n))
	(define (complex->real-checked n) (* 1.0 (real-part n)))
	(define (real->rational n) (make-rational (floor n) 1))
	(define (rational->scheme-number n) (if (= (denom n) 1) (numer n) n))
	(define (rational->scheme-number-checked n) (/ (numer n) (denom n)))
	(define (add-complex-to-schemenum z x) (make-from-real-imag (+ (real-part z) x) (imag-part z)))
	(define (rational->real n) (/ (* 1.0 (numer n)) (denom n)))

	(put 'equ? '(complex rational) (lambda (x y) (and (= (real-part x) (numer x)) (= (denom x) 1) (= (imag-part y) 0))))
	(put 'equ? '(rational complex) (lambda (y x) (and (= (real-part x) (numer x)) (= (denom x) 1) (= (imag-part y) 0))))
	(put 'equ? '(scheme-number rational) (lambda (x y) (and (= x (numer y)) (= (denom y) 1))))
	(put 'equ? '(rational scheme-number) (lambda (y x) (and (= x (numer y)) (= (denom y) 1))))
	(put 'equ? '(scheme-number complex) (lambda (x y) (and  (= x (real-part y) (= 0 (imag-part y))))))
	(put 'equ? '(complex scheme-number) (lambda (y x) (and  (= x (real-part y) (= 0 (imag-part y))))))
	(put 'equ? '(scheme-number real) (lambda (x y) (= x y)))
	(put 'equ? '(real scheme-number) (lambda (x y) (= x y)))

	(put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
	
	(put 'add '(complex scheme-number) (lambda (z x) (tag (add-complex-to-schemenum z x))))
	(put 'add '(scheme-number complex) (lambda (x z) (tag (add-complex-to-schemenum z x))))

	(put 'equ? '(real rational) (lambda (x y) (and (= x (numer y)) (= (denom y) 1))))
	(put 'equ? '(rational real) (lambda (y x) (and (= x (numer y)) (= (denom y) 1))))
	(put 'equ? '(real complex) (lambda (x y) (and  (= x (real-part y) (= 0 (imag-part y))))))
	(put 'equ? '(complex real) (lambda (y x) (and  (= x (real-part y) (= 0 (imag-part y))))))

	(put 'exp '(real real) (lambda (x y) (tag (expt x y))))
	
	(put 'add '(complex real) (lambda (z x) (tag (add-complex-to-schemenum z x))))
	(put 'add '(real complex) (lambda (x z) (tag (add-complex-to-schemenum z x))))

	(put 'sine 'complex (lambda (x) (sin (angle x))))
	(put 'cosine 'complex (lambda (x) (cos (angle x))))
	(put 'sine 'scheme-number (lambda (x) (sin x)))
	(put 'cosine 'scheme-number (lambda (x) (cos x)))
	(put 'sine 'real (lambda (x) (sin x)))
	(put 'cosine 'real (lambda (x) (cos x)))
	(put 'sine 'rational (lambda (x) (sin (rational->real x))))
	(put 'cosine 'rational (lambda (x) (cos (rational->real x))))



	(put-coercion 'scheme-number 'complex scheme-number->complex)
	(put-coercion 'scheme-number 'rational scheme-number->rational)
	(put-coercion 'real 'complex scheme-number->complex)
	(put-coercion 'rational 'real rational->real)
	(put-coercion '(complex checked) 'real complex->real-checked)
	(put-coercion 'complex 'real complex->real)
	(put-coercion 'real 'rational real->rational)
	(put-coercion 'rational 'scheme-number rational->scheme-number)
	(put-coercion '(rational checked) 'scheme-number rational->scheme-number-checked)
'done)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

(define (install-rational-package)
;; internal procedures
	(define (numer x) (car x))
	(define (denom x) (cdr x))
	(define (make n d) (let ((g (gcd n d))) (cons (/ n g) (/ d g))))
	(define (add x y)(make-rat 
		(+ (* (numer x) (denom y)) (* (numer y) (denom x))) 
		(* (denom x) (denom y))))
	(define (sub x y) (make-rat 
		(- (* (numer x) (denom y)) (* (numer y) (denom x)))
		(* (denom x) (denom y))))
	(define (mul x y) (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
	(define (div x y) (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
;; interface to rest of the system
	(define (tag x) (attach-tag 'rational x))
	(put 'add '(rational rational) (lambda (x y) (tag (add x y))))
	(put 'sub '(rational rational) (lambda (x y) (tag (sub x y))))
	(put 'mul '(rational rational) (lambda (x y) (tag (mul x y))))
	(put 'div '(rational rational) (lambda (x y) (tag (div x y))))
	(put 'equ? '(rational rational) (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
	(put 'rational 'numer numer)
	(put 'rational 'denom denom)
	(put '=zero? 'rational (lambda (x) (= (numer x) 0)))	
	(put 'make 'rational (lambda (n d) (tag (make n d))))
	(put 'neg 'rational (lambda (x) (make (- (numer x)) (denom x) )))
	(put-coercion 'rational 'rational identity)
'done)

(define (make-rational n d) ((get 'make 'rational) n d))
(define (numer n) ((get 'rational 'numer) n))
(define (denom n) ((get 'rational 'denom) n))

(define tower '(scheme-number rational real complex))	

(define (raise number)
	(let ((tag (type-tag number)))
	((if (eq? tag 'complex) number ((get-coercion tag (list-ref tower (+ (index-of tag tower) 1))) number)))))
(define (raise-to number desired)
	(if (eq? (type-tag number) desired) number (raise-to (raise number) desired)))

(define (project number)
	(let ((tag (type-tag number))) 
	(if (eq? tag 'scheme-number) number ((get-coercion tag (list-ref tower (- (index-of tag tower) 1))) number))))

(define (drop-checked number)
	(let 
	((tag (type-tag number)))
	(let
	((checked (get-coercion (list tag 'checked) (list-ref (- (index-of tag tower) 1))))
	(default (get-coercion tag (list-ref (- (index-of tag tower) 1)))))
	(let 
	((projected (if checked (checked number) (default number)))) 
	((if (eq? tag (type-tag projected)) number (drop-checked projected)))))))


(define (project-to number desired) (if (eq? (type-tag number) desired) number (project-to (project number) desired)))

(define (drop number) (let ((projected (project number))) ((if (eq? (type-tag number) (type-tag projected)) number (drop projected)))))

(define (drop? number) (eq? (type-tag number) (type-tag (project number))))

(define (install-complex-package)
	(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
	(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
;; internal procedures
	(define (add z1 z2) (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
	(define (sub z1 z2) (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
	(define (mul z1 z2) (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
	(define (div z1 z2) (make-from-mag-ang (/ (magnitude z1) (magnitude z2))(- (angle z1) (angle z2))))
;; interface to rest of the system
	(define (tag z) (attach-tag 'complex z))
	(install-polar-package)
	(install-rectanglular-package)
	(put 'add '(complex complex) (lambda (z1 z2) (tag (add z1 z2))))
	(put 'sub '(complex complex) (lambda (z1 z2) (tag (sub z1 z2))))
	(put 'mul '(complex complex) (lambda (z1 z2) (tag (mul z1 z2))))
	(put 'div '(complex complex) (lambda (z1 z2) (tag (div z1 z2))))
	(put 'real-part '(complex) real-part)
	(put 'imag-part '(complex) imag-part)
	(put 'magnitude '(complex) magnitude)
	(put 'angle '(complex) angle)
	(put 'equ? '(complex complex) (lambda (x y) (and (= (real-part x) (real-rart y)) (= (imag-part x) (imag-part y)))))
	(put '=zero? 'complex (lambda (x) (and (= (imag-part x) 0) (= (real-part x) 0))))
	(put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
	(put-coercion 'complex 'complex identity)
'done)

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(define (empty-termlist? l) (null? l))
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (the-empty-termlist) '())

(define (install-spare-terms-package)
	(define (adjoin-term term term-list) (if (=zero? (coff term term-list)) term-list (cons term term-list)))
	(define (adjoin-term-r term term-list) (if (=zero? (coff term term-list)) term-list (append term-list (list term))))
	(define (make-term order coef) (list order coef))
	(define (order term l) (car term))
	(define (coff term l) (cadr term))
	(define (neg-term term) (make-term order (neg coef)))

	(put 'adjoin-term 'sparse (lambda (coff order term-list) (adjoin-term (make-term (order coff)) term-list)))
	(put 'order 'sparse (lambda (term list) (order term)))	
	(put 'coff 'sparse (lambda (term list) (coff term)))
	(put 'make-term 'sparse make-term)
	(put 'neg-term 'sparse neg-term)
done)

(define (index-list l) (define (work list current) (if (null? list) '() (cons current (work (cdr list) (+ 1 current))))) (work l 0))

(define (install-dense-terms-package)
	(define (adjoin-term order coef term-list)
		(define (create-empty size) (if (= size 1) (list 0) (cons 0 (create-empty (- size 1)))))
	(cond 
	((null? term-list) (cons coef (create-empty order)))
	((> order (length term-list)) (cons coef (append (create-empty (- order (length term-list))) term-list)))
	((< order (length term-list)) (cons (first-term term-list) (adjoin-term order coef (rest-terms term-list))))
	((= order (length term-list)) (cons coff (rest-terms term-list)))))
	(define (order term term-list) (- (length term-list) 1 (index-of term term-list)))
	(define (coff order term-list) (list-ref order term-list))
	
	(put 'adjoin-term 'dense adjoin-term)
	(put 'order 'dense order)
	(put 'coff 'dense coff)
	(put 'make-term 'dense (lambda (order coff) (cons 'dense (adjoin-term order coff '()))))
	(put 'neg-term 'dense (lambda (x) (neg x)))
'done)


(define (install-polynomial-package)
	(define (make-poly variable term-list) (cons variable term-list))
	(define (variable p) (car p))
	(define (term-list p) (cdr p))
	(define (add-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
		(error "Polys not in same var -- ADD-POLY" (list p1 p2))))
	(define (add-terms L1 L2)
	(cond 
	((empty-termlist? L1) L2)
	((empty-termlist? L2) L1)
	(else
	(let 
		((t1 (first-term L1)) 
		(t2 (first-term L2)))
	(cond 
	((> (order t1 L1) (order t2 L2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
 	((< (order t1 L1) (order t2 L2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
	(else (adjoin-term (make-term (order t1 L1) (add (coff t1 L1) (coff t2 L2)) ) (add-terms (rest-terms L1) (rest-terms L2)))))))))


	(define (sub-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1) (sub-terms (term-list p1) (term-list p2)))
		(error "Polys not in same var -- ADD-POLY" (list p1 p2))))
	(define (sub-terms L1 L2)
	(cond 
	((empty-termlist? L1) L2)
	((empty-termlist? L2) L1)
	(else
	(let 
		((t1 (first-term L1)) 
		(t2 (first-term L2)))
	(cond 
	((> (order t1 L1) (order t2 L1)) (adjoin-term t1 (sub-terms (rest-terms L1) L2)))
 	((< (order t1 L1) (order t2 L1)) (adjoin-term (neg t2) (sub-terms L1 (rest-terms L2))))
	(else (adjoin-term (make-term (order t1 L1) (sub (coff t1 L1) (coff t2 L1))) (sub-terms (rest-terms L1) (rest-terms L2)))))))))

	(define (mul-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
		(error "Polys not in same var -- MUL-POLY" (list p1 p2))))
	(define (mul-terms L1 L2) (if (empty-termlist? L1) (the-empty-termlist) (add-terms (mul-term-by-all-terms (first-term L1) L2) (mul-terms (rest-terms L1) L2))))
	(define (mul-term-by-all-terms t1 L) (if (empty-termlist? L) (the-empty-termlist) 
	(let ((t2 (first-term L))) (adjoin-term  (make-term (+ (order t1 L) (order t2 L)) (mul (coff t1 L) (coff t2 L))) (mul-term-by-all-terms t1 (rest-terms L))))))

	(define (div-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(map (lambda (x) (make-poly (variable p1) x)) (div-terms-i (term-list p1) (term-list p2)) )
		(error "Polys not in same var -- DIV-POLY" (list p1 p2))))
	
	(define (reduce-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(map (lambda (x) (make-poly (variable p1) x)) (reduce-terms (term-list p1) (term-list p2)))
		(error "Polys not in same var -- REDUCE-POLY" (list p1 p2))))

	(define (gcd-poly p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
		(error "Polys not in same var -- GCD-POLY" (list p1 p2))))

	(define (div-terms L1 L2)
	(if (empty-termlist? L1) (list (the-empty-termlist) (the-empty-termlist))
	(let 
	(
	(t1 (first-term L1))
	(t2 (first-term L2))
	)
	(if (> (order t2 L2) (order t1 L1)) 
		(list (the-empty-termlist) L1)

		(let (
			(new-c (div (coff t1 L1) (coff t2 L2)))
			(new-o (- (order t1 L1) (order t2 L2)))
		     )
		     (let (
		          (rest-of-result (div-terms (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2)) L2))
		     	  (new-term (make-term new-o new-c))
			  )
		     (cons (adjoin-term new-term (car rest-of-result)) (cdr rest-of-result))))))))

	(define (div-terms-i L1 L2)
	(define (work result l1)
	(if (empty-termlist? l1) (list result l1)
	(let ((t1 (first-term l1))(t2 (first-term L2)))
	(if (> (order t2 L2) (order t1 l1)) 
		(list result l1)
		(let ( (new-c (div (coff t1 l1) (coff t2 L2))) (new-o (- (order t1 l1) (order t2 L2))) )
		     (let (
		          (rest-of-result (sub-terms l1 (mul-term-by-all-terms (make-term new-o new-c) L2)))
		     	  (new-term (make-term new-o new-c))
			  )
		     (work (adjoin-term-r new-term result) rest-of-result)))))))
		    
		    (work (the-empty-termlist) L1))

	(define (gcd-poly-p p1 p2)
	(if (same-variable? (variable p1) (variable p2))
		(make-poly (variable p1) (gcd-terms-p (term-list p1) (term-list p2)))
		(error "Polys not in same var -- GCD-POLY" (list p1 p2))))

	(define (gcd-terms a b) (if (empty-termlist? b) a (gcd-terms b (remainder-terms a b))))
	
	(define (gcd-terms-p a b) (if (empty-termlist? b) (simplify a) (gcd-terms-p b (pseudoremainder-terms a b))))

	(define (simplify-t l) (accumulate gcd (car l) (cdr l)))

	(define (simplify l) 
	(let ((terms (map (lambda (x) (coff x l)) l)))  
	(let ((cd (accumulate gcd (car terms) (cdr terms)))) 
	(map (lambda (x) (list (order x l) (/ (coff x l) cd))) l))))
	
	(define (simplify-l n d)
	(let 
	((coffs-n (map (lambda (x) (coff x n)) n))
	(coffs-d (map (lambda (x) (coff x n)) d)))
	(let
	((cd-n (simplify-t coffs-n))
	 (cd-d (simplify-t coffs-d)))
	(let ((cd (gcd cd-n cd-d)))
	(list (map (lambda (term) (make-term (order term n) (/ (coff term n) cd))) n)
	      (map (lambda (term) (make-term (order term n) (/ (coff term n) cd))) d))))))
	
	(define (remainder-terms a b) (cadr (div-terms-i a b)))

	(define (pseudoremainder-terms a b)
	(let
		(
		(t1 (first-term a))
		(t2 (first-term b))
		)
	(let ((factor (expt (coff t2 b) (- (+ 1 (order t1 a)) (order t2 b))))) (cadr (div-terms-i (mul-term-by-all-terms (make-term 0 factor) a) b)))))

	(define (reduce-terms nn dd)
	(let ((cd (gcd-terms-p nn dd)))
	(let ((factor (expt 
			(coff (first-term cd) cd) 
			(+ 
			1
			(if (> (order (first-term nn) nn) (order (first-term dd) dd)) 
				(order (first-term nn) nn) 
				(order (first-term dd) dd)) 
			(- (order (first-term cd) cd))))))		
	(simplify-l (car (div-terms-i (mul-term-by-all-terms (make-term 0 factor) nn) cd)) (car (div-terms-i (mul-term-by-all-terms (make-term 0 factor) dd) cd))))))

	(define p1 (make-poly 'x '((1 1)(0 1))))
	(define p2 (make-poly 'x '((3 1)(0 -1))))
	(define p3 (make-poly 'x '((1 1))))
	(define p4 (make-poly 'x '((2 1)(0 -1))))
	
	(define (tag p) (attach-tag 'polynomial p))
	(put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
	(put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
	(put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
	(put 'mul '(polynomial scheme-number) (lambda (p s) (tag (mul-poly p (make-poly (variable p) (make-term 0 s))))))
	(put 'div '(polynomial polynomial) (lambda (p1 p2) (map (lambda (x) (tag x)) (div-poly p1 p2))))
	(put 'div '(polynomial scheme-number) (lamda (p s) (lambda (p s) (tag (div-poly p (make-poly (variable p) (make-term 0 s))))))) 
	(put 'gcd '(polynomial polynomial) gcd-poly)
	(put '=zero? 'polynomial (lambda (p) (= (length (term-list p)) (length (filter (lambda (x) (=zero? x)) (terml-list p))))))
	(put 'reduce '(polynomial polynomial) reduce-poly)
	(display "done")
	(newline)
)

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
			  (cdr local-table)))))
'ok)
    (define (dispatch m)
      (cond 
	((eq? m 'lookup-proc) lookup)
	((eq? m 'insert-proc!) insert!)
	(else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define operation-table (make-table-d))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (=zero? number) ((get '=zero? (type-tag number)) number))

(define (equ? number1 number2) ((get 'equ? (type-tag number1) (type-tag number2)) number1 number2))
(define (equ? number1 number2) (= number1 number2))
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

(define (tower-v variable)
	(cond
	((eq? 'x variable) 1)
	((eq? 'y variable) 2)
	(else 3)))

(define (make-term coff order list) ((get 'make-term (type-tag list)) order coff list))

(define (greatest-common-divisor x y) ((get 'gcd (list (type-tag x) (type-tag y))) x y))
