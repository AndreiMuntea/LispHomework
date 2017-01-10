(defun SubarboreStang(Arbore)
	(cadr Arbore)
)

(defun SubarboreDrept(Arbore)
	(caddr Arbore)
)


; PROBLEMA 4 

(defun Conversie(Arbore)
	(cond
		((NULL Arbore) NIL)
		((and 
			(NULL (SubarboreStang Arbore))
		  	(NULL (SubarboreDrept Arbore))
		  )
			(list (car Arbore) 0)
		)
		((NULL (SubarboreStang Arbore))
			(append
				(list (car Arbore))
				(list 1)
				(Conversie (SubarboreDrept Arbore))
			)
		)
		((NULL (SubarboreDrept Arbore))
			(append
				(list (car Arbore))
				(list 1)
				(Conversie (SubarboreStang Arbore))
			)
		)
		(T
			(append
				(list (car Arbore))
				(list 2)
				(Conversie (SubarboreStang Arbore))
				(Conversie (SubarboreDrept Arbore))
			)
		)
	)
)

; PROBLEMA 8

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