(defun Parcurgere(Arbore Muchii Noduri SubarboreStang)
		(cond
			((equal Noduri (+ Muchii 1))
				(list
					SubarboreStang
					Arbore
				)
			)
			((null Arbore) nil)
			(T
				(Parcurgere
					(cddr Arbore) 
					(+ Muchii (cadr Arbore))
					(+ Noduri 1)
					(append SubarboreStang (list (car Arbore) (cadr Arbore))) 
				)
			)
		)
)

(defun SubarboreStang(Arbore)
	(car (Parcurgere (cddr Arbore) 0 0 NIL))
)

(defun SubarboreDrept(Arbore)
	(cadr (Parcurgere (cddr Arbore) 0 0 NIL))
)

; PROBLEMA 1 

(defun CaleSolver(Arbore Nod CaleCurenta)
	(cond
		((null Arbore) nil)
		((equal (car Arbore) Nod) 
			(append
				CaleCurenta
				(list (car Arbore))
			)
		)
		(T
			(append
				(CaleSolver 
					(SubarboreStang Arbore) 
					Nod
					(append CaleCurenta (list (car Arbore)))
				)
				(CaleSolver 
					(SubarboreDrept Arbore) 
					Nod
					(append CaleCurenta (list (car Arbore)))
				)		
			)
		)
	)
)

(defun Cale(Arbore Nod)
	(CaleSolver Arbore Nod NIL)
)

; PROBLEMA 2

(defun NoduriPeNivelulK(Arbore Nivel)
	(cond
		((null Arbore) nil)
		((equal Nivel 0) (list (car Arbore)))
		(T
			(append
				(NoduriPeNivelulK (SubarboreStang Arbore) (- Nivel 1))
				(NoduriPeNivelulK (SubarboreDrept Arbore) (- Nivel 1))
			)
		)
	)
)


; PROBLEMA 3

(defun NiveluriInArboreSolver(Arbore NivelCurent)
	(cond
		((NULL Arbore) NivelCurent)
		(T
			(max
				(NiveluriInArboreSolver (SubarboreStang Arbore) (+ NivelCurent 1))
				(NiveluriInArboreSolver (SubarboreDrept Arbore) (+ NivelCurent 1))
			)
		)
	)
)

(defun NiveluriInArbore(Arbore)
	(NiveluriInArboreSolver Arbore 0)
)

; PROBLEMA 5

(defun AdancimeaInArboreSolver(Arbore Nod AdancimeCurenta)
	(cond
		((NULL Arbore) -1)
		((equal Nod (car Arbore)) AdancimeCurenta)
		(T
			(max
				(AdancimeaInArboreSolver (SubarboreStang Arbore) Nod (+ AdancimeCurenta 1))
				(AdancimeaInArboreSolver (SubarboreDrept Arbore) Nod (+ AdancimeCurenta 1))
			)
		)
	)
)

(defun AdancimeaInArbore(Arbore Nod)
	(AdancimeaInArboreSolver Arbore Nod 0)
)

; PROBLEMA 6

(defun Inordine(Arbore)
	(cond
		((null Arbore) nil)
		(T
			(append
				(Inordine (SubarboreStang Arbore))
				(list (car Arbore))
				(Inordine (SubarboreDrept Arbore))
			)
		)
	)
)

; PROBLEMA 9

(defun Conversie(Arbore)
	(cond
		((NULL Arbore) NIL)
		(T
			(append
				(list (car Arbore))
				(list (Conversie (SubarboreStang Arbore)))
				(list (Conversie (SubarboreDrept Arbore)))
			)
		)
	)
)

; PROBLEMA 15

(defun Postordine(Arbore)
	(cond
		((null Arbore) nil)
		(T
			(append
				(Inordine (SubarboreStang Arbore))
				(Inordine (SubarboreDrept Arbore))
				(list (car Arbore))
			)
		)
	)
)