(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))

(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))

(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))

(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))

(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))

(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))

(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))

(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))
(assert! (can-do-job (computer programmer) (computer programmer trainee)))
(assert! (can-do-job (administration secretary) (administration big wheel)))

;(job ?x (computer programmer))
;(address ?x ?y)
;(supervisor ?x ?x)
;(job ?x (computer ?type))
;(job ?x (computer . ?type))
;(and (supervisor ?name (Bitdiddle Ben)) (address ?name ?address))
;(and (salary ?person ?amount) (salary (Bitdidle Ben) ?salary-Ben) (lisp-value < ?amount ?salary-Ben))
;(and (supervisor ?person ?supervisor) (not (job ?supervisor (computer . ?type))))

(assert! (rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
	   (address ?person-2 (?town . ?rest-2))
	   (not (same ?person-1 ?person-2)))))


(assert! (rule (same ?x ?x)))

(assert! (rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
	   (supervisor ?x ?middle-manager))))

(assert! (rule (can-replace ?person-1 ?person-2)
      (and (and (job ?person-1 ?type-1) 
		(job ?person-2 ?type-2))
	   (or (same ?type-1 ?type-2)
	       (can-do-job ?type-1 ?type-2))
	   (not (same ?person-1 ?person-2)))))

;(can-replace ?person-1 (Fect Cy D))
;(and (can-replace ?person-1 ?person-2)
;     (salary ?person-1 ?amount-1)
;     (salary ?person-2 ?amount-2)
;     (lisp-value > ?amount-1 ?amount-2))

(assert! (rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
	  (and (supervisor ?staff-person ?middle-manager)
	       (outranked-by ?middle-manager ?boss)))))

(assert! (rule (bigshot ?person)
      (and (supervisor ?person ?supervisor)
	   (job ?person (?type-person . ?rest-1) )
	   (job ?supervisor (?type-supervisor . ?rest-2))
	   (not (same ?type-person ?type-supervisor)))))

(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

;(meeting ?type (Friday ?time))

(assert! (rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division . ?rest))
	   (or (meeting whole-company ?day-and-time)
	     (meeting ?division ?day-and-time)))))

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z)))

(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z)))

(assert! (rule (last-pair (?element) (?element))))
(assert! (rule (last-pair (?element . ?elements) ?x) (last-pair ?elements ?x)))

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson ?G ?S)
      (and (son ?G ?F)
	   (son ?F ?S))))
(assert! (rule (son ?M ?S) 
      (and (wife ?M ?W)
	   (son ?W ?S))))

(assert! (rule (ends-with (?check) ?check)))
(assert! (rule (ends-with (?x . ?rest) ?check)
      (ends-with ?rest ?check)))

(assert! (rule ((great . ?rest) ?GP ?K)
      (and (ends-with ?rest grandson)
	   (son ?GP ?P)
	   (?rest ?P ?K))))

(assert! (rule (reverse (?x) (?x))))
(assert! (rule (reverse (?x . ?rest) ?output) 
      (and (reverse ?rest ?result) 
	   (append-to-form ?result (?x) ?output))))
