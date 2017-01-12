; PROBLEMA 4c

(defun InversSolve(Lista Colector)
	(cond
		((NULL Lista) Colector)
		((ATOM (car Lista))
				(InversSolve (cdr Lista) (cons (car Lista) Colector))
		)
		(T
			(append
				Colector
				(list(InversSolve(car Lista) nil))
				(InversSolve(cdr Lista) nil)
			)
		)
	)
)

(defun InverseazaAtomi(Lista)
	(InversSolve Lista nil)
)


;PROBLEMA 5c

(defun InverseazaLista(Lista)
	(cond
		((NULL Lista) nil)
		(T
			(append
				(InverseazaLista (cdr Lista))
				(list (car Lista))
			)
		)
	)
)

(defun Catul(A B)
	(multiple-value-bind (q r) (floor A B) q)
)

(defun Restul(A B)
	(multiple-value-bind (q r) (floor A B) r)
)

(defun Suma(Lista1 Lista2 Carry)
	(cond
		((and (NULL Lista1) (NULL Lista2) (equal 0 Carry)) nil)
		((and (NULL Lista1) (NULL Lista2)) (list Carry))
		((NULL Lista1)
			(append
				(list (Restul (+ (car Lista2) Carry) 10))
				(Suma Lista1 (cdr Lista2) (Catul (+ (car Lista2) Carry) 10))
			) 
		)
		((NULL Lista2)
			(append
				(list (Restul (+ (car Lista1) Carry) 10))
				(Suma (cdr Lista1) Lista2 (Catul (+ (car Lista1) Carry) 10))
			)
		)
		(T
			(append
				(list (Restul (+ (car Lista1) (car Lista2) Carry) 10))
				(Suma (cdr Lista1) (cdr Lista2) (Catul (+ (car Lista1) (car Lista2) Carry) 10) )
			)
		)
	)
)

(defun CalculeazaSuma(Lista1 Lista2)
	(InverseazaLista
		(Suma
			(InverseazaLista Lista1)
			(InverseazaLista Lista2)
			0
		)
	)
)

;PROBLEMA 7c

(defun UltimulElement(Lista Element)
	(cond
		((NULL Lista) Element)
		(T (UltimulElement (cdr Lista) (car Lista)))
	)
)

(defun InlocuiesteCuUltimul(Lista)
	(cond
		((NULL Lista) nil)
		((ATOM (car Lista))
			(cons
				(car Lista)
				(InlocuiesteCuUltimul (cdr Lista))
			)
		)
		(T
			(append
				(list (UltimulElement (InlocuiesteCuUltimul(car Lista)) nil))
				(InlocuiesteCuUltimul (cdr Lista))
			)
		)
	)
)

;PROBLEMA 9C

(defun Lungime(Lista)
	(cond
		((NULL Lista) 0)
		(T (+ 1 (lungime (cdr Lista))))
	)
)

(defun PrimeleElemente(Lista)
	(cond 
		((NULL Lista) nil)
		((ATOM (car Lista)) (PrimeleElemente (cdr Lista)))
		( (equal
				(Mod
					(Lungime (car Lista))
					2
				) 
				0
		  )
			(append
				(PrimeleElemente (car Lista))
				(PrimeleElemente (cdr Lista))
			)
		)
		(T
			(append
				(list (caar Lista))
				(PrimeleElemente (car Lista))
				(PrimeleElemente (cdr Lista))
			)
		)
	)
)

(defun Solve9(Lista)
	(cond
		((equal
				(Mod
					(Lungime Lista)
					2
				) 
				0
		 )
		 (PrimeleElemente Lista))
		(T
			(append
				(list (car Lista))
				(PrimeleElemente Lista)
			)
		)
	)
)

; PROBLEMA 10C

(defun EvalueazaExpresie(Lista Stiva)
	(cond
		((NULL Lista) (car Stiva))
		((numberp (car Lista))
			(EvalueazaExpresie (cdr Lista) (cons (car Lista) Stiva))
		)
		(T
			(EvalueazaExpresie
				(cdr Lista)
				(cons
					(eval (list (car Lista) (car Stiva) (cadr Stiva)))
					(cddr Stiva)
				)
			)
		)
	)
)

(defun Solve10(Lista)
	(EvalueazaExpresie(InverseazaLista Lista) nil)
)


; PROBLEMA 11B

(defun Munte(Lista ElementPrecedent Directie)
	(cond
		((and (NULL Lista) (equal Directie -1) T))
		((NULL Lista) NIL)
		((and (< ElementPrecedent (car Lista)) (equal Directie -1)) NIL)
		((< ElementPrecedent (car lista)) 
			(Munte 
				(cdr lista)
				(car lista)
				1
			)
		)
		(T
			(Munte
				(cdr lista)
				(car lista)
				-1
			) 
		)
	)
)

(defun SolveMunte(Lista)
	(cond
		((null Lista) nil)
		((null (cdr Lista)) nil)
		((> (car Lista) (cadr Lista)) nil)
		(T (Munte (cdr Lista) (car Lista) 1))
	)
)