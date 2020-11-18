;Jarivs Cole 

;tests
;(all-accepting-sequences-up-to-length *gray-grey-machine* 5)
;(extended-transition-function *gray-grey-machine* 0 '(g r e y)) 

(defun table-element(table row column)
	(nth column (nth row table))
)
;uses nth twice to get the element from a list of a list

(defun transition-function-index(fsm state-index transition)
	(let ((transition-numeric (position transition (fsm-alphabet fsm))))
		(table-element (fsm-table fsm) state-index transition-numeric)
	)
)
;uses position function to get the state index as a result of a transition


(defun extended-transition-function (fsm state-index transition-sequence)
	(cond ((null transition-sequence) (list state-index)) ;base case a empty list
		(t
			(let ((current (transition-function-index fsm state-index (car transition-sequence)))) ;takes the first part of the list
				(append (list current) (extended-transition-function fsm current (cdr transition-sequence))) 
			)
		)
	)
)
;appeneds the transition and moves through the list recursivley

(defun accepted? (fsm transition-sequence)
	(let* ((states (extended-transition-function fsm (fsm-initial fsm) transition-sequence))
		  (end (last states))) ;last elment of the transition
		(cond ((equalp (member (car end) (fsm-accepting fsm)) nil) nil) ;remove end from a list
			  (t states) ;if end is in accepting states return the states
		)
	)
)

(defun filter-accepted-sequences (fsm list-of-sequences)
	(cond ((null list-of-sequences) nil) ;empty list check  here
		(t 
			(let ((current (accepted? fsm (car list-of-sequences)))) ;current list of transitions
				(cond ((equalp nil current)     (filter-accepted-sequences fsm (cdr list-of-sequences))) ;if nil conintue on to next list
					  (t (append (list (car list-of-sequences)) (filter-accepted-sequences fsm (cdr list-of-sequences)))) ;if not nill append list to accepted
				)
			)
		)
	)
)

(defun up-to-length (n)
	(loop for x from 1 to n
		  for y = x
		  collect y)
)
;helper produces a list up to length n, ie 4 (1 2 3 4)

(defun all-sequences-up-to-length (alphabet n)
	(apply #'append
		(mapcar #'(lambda(x) (all-sequences-of-length alphabet x)) (up-to-length n))))
;uses mapcar to run all-sequences-of-length against list (1 2 3 4)

(defun all-accepting-sequences-up-to-length (fsm n)
	(filter-accepted-sequences fsm (all-sequences-up-to-length (fsm-alphabet fsm) n))
)
;conbines up to the filter-accepted-sequences with all sequences generated to produce accepted list