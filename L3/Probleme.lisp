;PROBLEMA 1
(defun Adancime(L)
	(cond
		((NULL L) 0)
		((ATOM L) 0)
		(T
			(+ 1 (apply #'max (mapcar #'Adancime L)))
		)
	)
)

;PROBLEMA 2
(defun Atomas(L)
	(cond
		((NULL L) NIL)
		((ATOM L) (list L))
		(T
			(mapcan #'Atomas L)
		)
	)
)

;PROBLEMA 3
(defun Exista(L E)
	(cond
		((NULL L) NIL)
		((AND (ATOM L) (EQUAL L E)) (list T)) 
		((ATOM L) NIL)
		(T
			(mapcan (lambda (lista) (Exista lista E)) L)
		)
	)
)

;PROBLEMA 4

(defun Suma(L)
	(cond
		((NULL L) 0)
		((NUMBERP L) (list L))
		((ATOM L) 0)
		(T
			(apply #'+ (mapcan #'Suma L))
		)
	)
)


;PROBLEMA 5

(defun Apartenenta(L Nod)
	(cond
		((NULL L) nil)
		((AND (ATOM L) (EQUAL L Nod)) (list T))
		((ATOM L) nil)
		(T
			(mapcan (lambda (x) (Apartenenta x Nod) ) L)
		)
	)
)

;PROBLEMA 9

(defun Inlocuieste (L Nod L1)
	(cond
		((NULL L) nil)
		((AND (ATOM L) (EQUAL L NOD)) L1)
		((ATOM L) L)
		(T
			(mapcar (lambda (x) (Inlocuieste x nod L1)) L)
		)
	)
)

;PROBLEMA 10

(defun Noduri(L K)
	(cond
		((NULL L) 0)
		((AND (ATOM L) (EQUAL K -1) 1))
		((ATOM L) 0)
		(T
			(apply #'+ (mapcar (lambda (x) (Noduri x (- K 1))) L))
		)
	)
)


;PROBLEMA 11

(defun Sterge(L K)
	(cond
		((NULL L) (list nil))
		((EQUAL K L) nil)
		((ATOM L) (list L))
		(T
			(list (mapcan (lambda (x) (Sterge x K)) L))
		)
	)
)

(defun WraperSterge(L K)
	(car (Sterge L K))
)


;PROBLEMA 16

(defun Inverseaza(L)
	(cond
		((NULL L) nil)
		((ATOM L) L)
		(T
			(reverse (mapcar #'Inverseaza L))
		)
	)
)