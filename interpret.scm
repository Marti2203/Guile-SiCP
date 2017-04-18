(define (square x) (* x x))
(define (cube x) (* x x x))
(define (require p) (if (not p) (amb)))
(define (for-each proc items) (if (null? items) 'done (begin (proc (car items)) (for-each proc (cdr items))))) 
;(define (cons x y) (lambda (m) 12 (m x y)))  ; C(hex)=12(dec)
;(define (car z) (z (lambda (p q) p)) )
;(define (cdr z) (z (lambda (p q) q))) 
(define (list-ref items n) (if (= n 0) (car items) (list-ref (cdr items) (- n 1)))) 
(define (map proc items) (if (null? items) '() (cons (proc (car items)) (map proc (cdr items)))))
(define (filter proc items) (if (null? items) '() (if (proc (car items)) (cons (car items) (filter proc (cdr items))) (filter proc (cdr items)))))
(define (scale-list items factor) (map (lambda (x) (* x factor)) items)) 
(define (add-lists list1 list2) (cond ((null? list1) list2) ((null? list2) list1) (else (cons (+ (car list1) (car list2)) (add-lists (cdr list1) (cdr list2)))))) 
;(define ones (cons 1 ones)) 
;(define integers (cons 1 (add-lists ones integers))) 
;(define (integral integrand initial-value dt)(define int (cons initial-value (add-lists (scale-list integrand dt) int))) int)  
;(define (solve f y0 dt)(define y (integral dy y0 dt))(define dy (map f y)) y)  

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between low high) (let ((a (an-integer-starting-from low))) (require (< a high)) a))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))


(define (a-random-element-of l) 
  (require (not (null? l))) 
  (ramb (car l) (a-random-element-of (cdr l))))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
	(b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
(require (and (< k (+ i j)) (= (+ (* i i) (* j j)) (* k k))))
(list i j k)))))

(define (a-pythagorean-triple-all)
  (let ((i (an-integer-starting-from 2)))
    (let ((j (an-integer-between 1 i)))
      (let ((k (an-integer-between i (+ i j))))
(require (= (+ (* i i) (* j j)) (* k k)))
(list i j k)))))

(define (a-pythagorean-triple-between-Ben low high)
  (let ((i (an-integer-between low high))
	(hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
	(require (>= hsq ksq))
	(let ((k (sqrt ksq)))
	  (require (integer? k))
	  (list i j k))))))

(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
	(cooper (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller (amb 1 2 3 4 5))
	(smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))

(define (multiple-dwelling-asmith)
  (let ((baker (amb 1 2 3 4 5))
	(cooper (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller (amb 1 2 3 4 5))
	(smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))

(define (multiple-dwelling-fast)
  (let (
	(fletcher (amb 2 4))
	(baker (amb 1 2 3 4))
	(miller (amb 3 4 5))
	(cooper (amb 2 3 4))
	(smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))

(define adjectives '(adjective big old orange huge fat boring))
(define adverbs '(adverb slowly fast loudly lightly)) 
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define connectors '(connector and or while but whereas))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
	(parse-word prepositions)
	(parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
	(parse-noun-phrase)
	(parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (define (loop input)
	(amb (loop (list input (parse-word adjectives)))
	     input))

  (list 'noun-phrase
	(parse-word articles)
	(amb (list 'adjective-phrase (loop (parse-word adjectives)) (parse-word nouns))
	     (parse-word nouns))))

;for a noun - preposition adjectives noun

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
	 (maybe-extend (list 'noun-phrase
			     noun-phrase
			     (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-simple-verb-phrase)
  (amb (list 'verb-phrase 
	     (parse-word adverbs)
	     (parse-word verbs))
       (parse-word verbs)))

(define (parse-verb-phrase)
(define (maybe-extend verb-phrase)
  (amb verb-phrase
       (maybe-extend (list 'verb-phrase
			   verb-phrase
			   (parse-prepositional-phrase)))))
(maybe-extend (parse-simple-verb-phrase)))


(define (parse-compound-sentence)
  (define (maybe-extend-sentence sentence)
  (amb
   (maybe-extend-sentence (list 'compound-sentence 
				sentence
				(parse-word connectors)
				(parse-sentence)))
   sentence))
  (maybe-extend-sentence (parse-sentence)))

(define *unparsed* '())
(define (parse input) (set! *unparsed* input)
  (let ((sent (parse-sentence))) (require (null? *unparsed*)) sent))

(define (parse-compound input) (set! *unparsed* input)
  (let ((sent (parse-compound-sentence))) (require (null? *unparsed*)) sent))

(define (generate-word word-list) (a-random-element-of (cdr word-list)))

(define (generate-prepositional-phrase)
  (list	(generate-word prepositions)
	(generate-noun-phrase)))

(define (generate-sentence)
  (list	(generate-noun-phrase)
	(generate-verb-phrase)))

(define (generate-compound-sentence)
  (define (maybe-extend-sentence sentence)
    (ramb sentence
	 (maybe-extend-sentence 
	   (list
		 sentence
		 (generate-word connectors)
		 (generate-sentence)))))
    (maybe-extend-sentence (generate-sentence)))

(define (generate-verb-phrase)
  (define (generate-adverbs-verb-phrase)
    (ramb (list (generate-word adverbs)
		(generate-word verbs))
	 (generate-word verbs)))
  (define (maybe-extend-prepositions verb-phrase)
    (ramb verb-phrase
	 (maybe-extend-prepositions (list verb-phrase
					  (generate-prepositional-phrase)))))
  (maybe-extend-prepositions (generate-adverbs-verb-phrase)))

(define (generate-noun-phrase)
  (define (maybe-extend-adjectives input)
    (ramb (list input (generate-word adjectives))
	 input))
  (list (maybe-extend-adjectives (generate-word articles)) 
				   (generate-word nouns)))

(define (work-test-permanent-set)
  (define count 0)
  (let ((x (an-element-of '(a b c)))
	(y (an-element-of '(a b c))))
    (permanent-set! count (+ count 1))
    (require (not (eq? x y)))
    (list x y count)))

(define (work-test-if-fail)
  (let ((pairs '()))
    (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
	       (permanent-set! pairs (cons p pairs))
	       (amb))
	     pairs)))
