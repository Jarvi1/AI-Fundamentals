;Jarvis Cole 

;iv;
;max depth of 4 created a cputime limit.
;max depth of 2 is fast and hard to beat.
;max depth of 1 is reasonable difficult to beat if you are not paying attention.
;running with (play t) allows for a even better advantage as it gets the first move.
;running the game with a larger board size ie 5, lead to more cpu time, this is likley beacuase of more possibilities.
;summarising, max depth of 2 is a optimal setting for the game

;i		
(defun pieces-in-direction (board coord direction)
	(mapcar #'(lambda(x) (get-element board x)) (range-in-direction coord direction)))
;uses mapcar function to apply the get-element function to the set of coords in range-in-direction

;ii
(defun count-until-not-nil (my-list)
   (cond ((null my-list) 0) 						;base case empty list
		 ((not (equalp (car my-list) nil)) 0)		;base case nil is reached
         (t (+ (count-until-not-nil (cdr my-list)) 	;continue to next
			   1))
   )
)
;is a revursive loop that stops when it reaches a not nil value

;iii
(defun number-of-movements-in-direction (board player direction)
	(let ((elements (pieces-in-direction board (locate board player) direction)))
		(count-until-not-nil elements)
	)
)
;takes the pieces-in-direction then counts until-not-nil

;iv
(defun board-value (board player)
	(apply '+
		(mapcar #'(lambda(x) (number-of-movements-in-direction board player x)) *directions*)))
;uses map car to apply number of movements-in-direction to all directions, then uses apply to sum 

;v
(defun static (board player)
	(let* ((opp (opposite player))
		  (b-val-player (board-value board player))	;some board value for you the player
		  (b-val-opp    (board-value board opp)))	;some board value for the AI
		
		(- b-val-player b-val-opp)
	)
)
;uses board-value of player and opposite then subtracts
