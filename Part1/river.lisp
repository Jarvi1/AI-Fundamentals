;Jarvis Cole 19930144

;globals, and start
(defparameter *start-state* (list 1 2 3 'e)) 

(defparameter *goal-state* (list 0 0 0 'w))

(defun make-east-state (w c d f)   ;wolf cats ducks farmer on east side
	(list w c d f)				   ;may need to make whole state nil if whole if one side nil
)

;Accesors
(defun wolf-e-side (state)
	(nth 0 state)
)

(defun cats-e-side (state)
	(nth 1 state)
)

(defun ducks-e-side (state)
	(nth 2 state)
)

(defun wolf-w-side (state)
	(- 1 (nth 0 state))
)

(defun cats-w-side (state)
	(- 2 (nth 1 state))
)

(defun ducks-w-side (state)
	(- 3 (nth 2 state))
)

(defun farmer-side (state)
	(nth 3 state)
)

;Solution
(defun solution-state? (state)
	(equalp *goal-state* state)
)

;Switch the side in list all methods may not relevant in this section!
(defun opp-farmer (side)
	(cond ((equalp side 'e) 'w)
		  ((equalp side 'w) 'e)
	)
)

;Helper is safe
(defun safe (state)
	(let ((fs   (farmer-side state))
		  (wes  (wolf-e-side state))
		  (ces  (cats-e-side state))
		  (des (ducks-e-side state))
		  (wws  (wolf-w-side state))
		  (cws  (cats-w-side state))
		  (dws (ducks-w-side state)))
		  
		(cond ((and (equalp fs 'e) (or (and (>= wws cws) (> cws 0)) (and (>= cws dws) (> dws 0))))
				nil)
			  ((and (equalp fs 'w) (or (and (>= wes ces) (> ces 0)) (and (>= ces des) (> des 0))))
				nil)
			(t state)
		)
	)
)

;Operators
(defun f-takes-self (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		  
		(safe
			(make-east-state
				 wes
				 ces
				 des
				(opp-farmer fs)
			)
		)
	)
)

(defun f-takes-wolf (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		
		(cond ((and (equalp fs 'e) (>= wes 1))
					(safe
						(make-east-state
							(- wes 1)
							 ces
							 des
							(opp-farmer fs)
						)
					)						
			  )
			  ((and (equalp fs 'w) (>= wws 1))
					(safe
						(make-east-state
							(+ wes 1)
							 ces
							 des
							(opp-farmer fs)
						)
					)	
			  )	
			  (t nil)
		)
	)
)

(defun f-takes-wolf-cat (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		  
		(cond ((and (equalp fs 'e) (and (>= wes 1) (>= ces 1)))
					(safe
						(make-east-state
							(- wes 1)
							(- ces 1)
							 des
							(opp-farmer fs)
						)
					)						
			  )
			  ((and (equalp fs 'w) (and (>= wws 1) (>= cws 1)))
					(safe
						(make-east-state
							(+ wes 1)
							(+ ces 1)
							 des
							(opp-farmer fs)
						)
					)	
			  )	
			  (t nil)
		)
	)
)

(defun f-takes-wolf-duck (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		  
		(cond ((and (equalp fs 'e) (and (>= wes 1) (>= des 1)))
					(safe
						(make-east-state
							(- wes 1)
							 ces
							(- des 1)
							(opp-farmer fs)
						)
					)						
			  )
			  ((and (equalp fs 'w) (and (>= wws 1) (>= dws 1)))
					(safe
						(make-east-state
							(+ wes 1)
							 ces
							(+ des 1)
							(opp-farmer fs)
						)
					)	
			  )	
			  (t nil)
		)
	)
)

(defun f-takes-cat (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		  
		(cond ((and (equalp fs 'e) (>= ces 1))
					(safe
						(make-east-state
							 wes
							(- ces 1)
							 des
							(opp-farmer fs)
						)
					)						
			  )
			  ((and (equalp fs 'w) (>= cws 1))
					(safe
						(make-east-state
							 wes
							(+ ces 1)
							 des
							(opp-farmer fs)
						)
					)	
			  )	
			  (t nil)
		)
	)
)

(defun f-takes-cat-cat (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		  
		(cond ((and (equalp fs 'e) (>= ces 2))
					(safe
						(make-east-state
							 wes
							(- ces 2)
							 des
							(opp-farmer fs)
						)
					)						
			  )
			  ((and (equalp fs 'w) (>= cws 2))
					(safe
						(make-east-state
							 wes
							(+ ces 2)
							 des
							(opp-farmer fs)
						)
					)	
			  )	
			  (t nil)
		)
	)
)

(defun f-takes-cat-duck (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		  
		(cond ((and (equalp fs 'e) (and (>= ces 1) (>= des 1)))
					(safe
						(make-east-state
							 wes 
							(- ces 1)
							(- des 1)
							(opp-farmer fs)
						)
					)						
			  )
			  ((and (equalp fs 'w) (and (>= cws 1) (>= dws 1)))
					(safe
						(make-east-state
							 wes
							(+ ces 1)
							(+ des 1)
							(opp-farmer fs)
						)
					)	
			  )	
			  (t nil)
		)
	)
)

(defun f-takes-duck (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		  
		(cond ((and (equalp fs 'e) (>= des 1))
					(safe
						(make-east-state
							 wes
							 ces
							(- des 1)
							(opp-farmer fs)
						)
					)						
			  )
			  ((and (equalp fs 'w) (>= dws 1))
					(safe
						(make-east-state
							 wes
							 ces
							(+ des 1)
							(opp-farmer fs)
						)
					)	
			  )	
			  (t nil)
		)
	)
)

(defun f-takes-duck-duck (state)
	(let 
		((fs  (farmer-side state))
		(wes  (wolf-e-side state))
		(ces  (cats-e-side state))
		(des (ducks-e-side state))
		(wws  (wolf-w-side state))
		(cws  (cats-w-side state))
		(dws (ducks-w-side state)))
		  
		(cond ((and (equalp fs 'e) (>= des 2))
					(safe
						(make-east-state
							 wes
							 ces
							(- des 2)
							(opp-farmer fs)
						)
					)						
			  )
			  ((and (equalp fs 'w) (>= dws 2))
					(safe
						(make-east-state
							 wes
							 ces
							(+ des 2)
							(opp-farmer fs)
						)
					)	
			  )	
			  (t nil)
		)
	)
)

(defparameter *operators* '(f-takes-self
							f-takes-wolf
							f-takes-wolf-cat
							f-takes-wolf-duck
							f-takes-cat
							f-takes-cat-cat
							f-takes-cat-duck
							f-takes-duck
							f-takes-duck-duck))