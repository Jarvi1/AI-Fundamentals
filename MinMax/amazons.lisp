#| 
	Functions for creating the initial board.
|#

; creates an n-by-n empty board
(defun empty-board (n) (empty-board-helper n n))

(defun empty-board-helper (n m)
  (if (= m 0) nil (cons (empty-list n) (empty-board-helper n (- m 1)))))
 
; creates a list of n elements, all initialised to nil.
(defun empty-list (n)
  (if (= n 0) nil (cons nil (empty-list (- n 1)))))

; creates an n-by-n board with a nice starting configuration.
(defun start-board (n)
  (let ((board (empty-board n)))
	(setf (nth (- n 2) (first board)) 'o )
	(setf (second (nth (- n 1) board)) 'x )
	board))

#| 
	Parameters
|#

(defparameter *board-size* 4)
(defparameter *start* (start-board *board-size*))

(defun change-board-size (n)
  (setf *board-size* n)
  (setf *start* (start-board n)))

(defparameter *directions*
  '(up down left right up-left up-right down-left down-right))

#| returns a numeric interpretation of the directions. |#
(defun direction-numeric (dir)
  (cond ((equalp dir 'up) '(0 -1))
		((equalp dir 'down) '(0 1))
		((equalp dir 'left) '(-1 0))
		((equalp dir 'right) '(1 0))
		((equalp dir 'up-left) '(-1 -1))
		((equalp dir 'up-right) '(1 -1))
		((equalp dir 'down-left) '(-1 1))
		((equalp dir 'down-right) '(1 1))
  		(t nil)))

#| 
Returns the piece on the board which is in the position specified by coord.
|#
(defun get-element (board coord)
  (let ((x (first coord))
		(y (second coord)))
	(nth x (nth y board))))

#| 
Checks if there is an obstruction on the board between coord1 and coord2
Will return nil if the path between them is undefined
|#
(defun obstruction? (board coord1 coord2)
  (let ((path (path-between coord1 coord2)))
	(if path
	  	(not (every #'null (mapcar #'(lambda(x) (get-element board x)) path)))
		nil)))

#| 
Function legal-movement? takes a board (board), a player (player, which is 'x or 'o)
a move represented by its source (move-from) and destination (move-to).
If the move is legal, it returns the board with the moved piece.
Doesn't account for shooting the arrow.
|#

(defun legal-movement? (board player move-to)
  (let ((move-from (locate board player)))
	(when (and (equalp player (get-element board move-from))
			   (not (equalp move-from move-to))
			   (same-line? move-from move-to)
			   (not (obstruction? board move-from move-to)))
	  (let ((board-copy (copy-tree board)))
		(setf (nth (first move-to) (nth (second move-to) board-copy)) player)
		(setf (nth (first move-from) (nth (second move-from) board-copy)) nil)
		board-copy))))

#| 
Function legal-arrow? takes a board position (board), a position (pos) and an arrow location (arrow).
It checks if there are no obstructions in the path from pos to arrow, returning the new board if it is an acceptable move.
If it is not acceptable, returns nil.
|#

(defun legal-arrow? (board pos arrow)
  (when (and (same-line? pos arrow)
			 (not (obstruction? board pos arrow)))
	  (let ((board-copy (copy-tree board)))
		(setf (nth (first arrow) (nth (second arrow) board-copy)) '@)
		board-copy)))

#|
This function finds the coordinate of the piece specified by player on the given board.
|#

(defun locate (board player)
  (nth 0 (locate-helper board player 0 0)))

; locate-helper actually finds -all- locations of the given piece - adaptable to possibility of more than one piece on the board.  
(defun locate-helper (board player row column)
  (let* ((pos (get-element board (list row column)))
		 (next-column (if (>= column (- *board-size* 1)) 0 (+ column 1)))
		 (next-row (if (= next-column 0) (+ row 1) row)))
		(cond ((or (>= column *board-size*) (>= row *board-size*) nil))
			  ((equalp pos player)
			   (cons (list row column) (locate-helper board player next-row next-column)))
			  (t (locate-helper board player next-row next-column)))))

#|
	Returns true if coord1 and coord2 are on the same line - horizontal, vertical or diagonal
	This is true precisely when at least one of the following are true:
		- same x-coordinate
		- same y-coordinate
		- either y-x or y+x are the same
|#
(defun same-line? (coord1 coord2)
  (let ((x1 (first coord1))
		(x2 (first coord2))
		(y1 (second coord1))
		(y2 (second coord2)))
	   (or (= x1 x2) (= y1 y2) (= (+ x1 y1) (+ x2 y2)) (= (- y1 x1) (- y2 x2))))
  )

#| 
Given two coordinates, returns a coordinate pointing in the same direction as the vector from coord1 to coord2.
e.g. if coord1 is (2,3) and coord2 is (4,1) then the result is (1,-1).
|#
(defun direction-between (coord1 coord2)
  (if (and (not (equalp coord1 coord2)) (same-line? coord1 coord2))
	(let* ((x1 (first coord1))
		(x2 (first coord2))
		(y1 (second coord1))
		(y2 (second coord2))
		(x-change (cond ((= x1 x2) 0) ((> x1 x2) -1) (t 1)))
		(y-change (cond ((= y1 y2) 0) ((> y1 y2) -1) (t 1))))
		(list x-change y-change))
	nil))

#| 
Tests if the coordinate is inside a board with size given by the *board-size* parameter
|#
(defun in-range? (coord)
  (let ((x (first coord))
		(y (second coord)))
	(and (<= 0 x (- *board-size* 1)) (<= 0 y (- *board-size* 1)))))

#| 
Returns all coordinates that are on the board in a given direction from the specified coordinate.
Makes use of *board-size* parameter via in-range?
|#

(defun range-in-direction (coord direction)
  (let* ((change (direction-numeric direction))
		 (next (mapcar #'+ coord change)))
		(if (in-range? next) 
		  	(append (list next) (range-in-direction next direction))
			nil)))

#|
Returns all coordinates on the board in all directions from the specified coordinate.
Makes use of *board-size* parameter via range-in-direction
|#

(defun full-range (coord)
  (apply #'append (remove nil (mapcar #'(lambda (x) (range-in-direction coord x)) *directions*))))

#| 
Finds the coordinates for all squares in the path from coord1 to coord1.
	- does not include coord1
	- but does include coord2 
	- returns nil if they are not on a path.
|#
(defun path-between (coord1 coord2)
  (let* ((change (direction-between coord1 coord2))
		 (next (mapcar #'+ coord1 change)))
		(cond 
		  	((equalp coord1 coord2) nil)
			((equalp next coord2) (list coord2))
		  	(change (append (list next) (path-between next coord2)))
			(t nil)
			)))


#| 
	Printing utilities
|#

#| 
Prints the board...
|# 

(defun print-board (board)
  (print-rows board 0)
  (format t "~%~%")
  )

#|
Returns a string for printing the row.
|# 

(defun row-helper (row column)
  (let ((start (* row *board-size*)))
		(format nil "~3D |~A" 
				(+ start column) 
				(if (< column (- *board-size* 1))
				  	(row-helper row (+ column 1))
					""))))

#|
Prints the given row, and then the next ones, proceeding recursively.
|#
(defun print-rows (board row)
	(let* ((to-print (mapcar #'(lambda (x) (cond ((null x) ".") (t x))) (nth row board)))
		  (str1 (format nil "~%  |~{ ~A |~}" to-print))
		  (str2 (format nil "|~a" (row-helper row 0)))
		  (dash1 (format nil "~v@{~A~:*~}" (- (length str1) 3) "-"))
		  (dash2 (format nil "~v@{~A~:*~}" (length str2) "-"))
		  (tabs (format nil "~v@{~A~:*~}" 2 #\tab)))
	  	 (when (= row 0) (format t "~%  ~a~a~a" dash1 tabs dash2))
	  	 (format t "~a~a~a" str1 tabs str2)
	  	 (format t "~%  ~a~a~a" dash1 tabs dash2)
		 (unless (>= row (- *board-size* 1)) (print-rows board (+ row 1)))))


#| 
	Required for minimax
|#

#|
Function OPPOSITE returns 'x when given 'o, and vice-versa
|#
(defun opposite (player)
  (if (equalp player 'x) 'o 'x))

#| A draw is impossible for Amazons |#
(defun drawn? (pos) nil) 

#|
Function DEEP-ENOUGH takes a board position and a depth and returns t if the search has proceeded deep enough.  
|#

(defparameter *max-depth* 1)

(defun deep-enough (board depth)
  (or (won? board 'x) 
      (won? board 'o)
      (drawn? board)
      (>= depth *max-depth*)))


#| 
Function move-to-coord converts a move number given as input into the coordinate it represents.
|#
(defun move-to-coord (move)
  (let* ((column (mod move *board-size*))
		 (row (nth-value 0 (floor move *board-size*))))
		 (list column row)))

#|
Function make-move takes a board position (board), a player (player, which is 'x or 'o), 
and a move (which is a number between 0 and 24 inclusive). 
If the move is legal it returns a new board position. 
|#
(defun make-move (board player move)
  ; first convert the move number to a coordinate  
  (let* ((coord (move-to-coord move))
		 (legal (legal-movement? board player coord)))
	(cond ((not legal) nil)
		  ; now request the arrow location
		  (t 
			(format t "Enter arrow destination: ")
			(let* ((arrow-move (read))
				   (arrow-coord (move-to-coord arrow-move)))
			  (loop until (setq valid-move (legal-arrow? legal coord arrow-coord))
					do (format t "~%~a Invalid location, try again: " arrow-move)
					(setq arrow-move (read))
					(setq arrow-coord (move-to-coord arrow-move)))
			  valid-move)))))

#| 
Returns a list containing all the cells immediately adjacent to the player's piece
|#
(defun neighbourhood (board player)
  (let ((changes (mapcar #'direction-numeric *directions*))
		(pos (locate board player)))
	(mapcar #'(lambda (x) (get-element board x))
	  (remove-if-not #'in-range? (mapcar #'(lambda (x) (mapcar #'+ pos x)) changes)))))

#|
A player loses if it is completely surrounded; i.e., no NIL elements in its neighbourhood.
|#
(defun won? (board player) 
  (notany #'(lambda (x) (or (equalp x (opposite player)) (null x))) (neighbourhood board (opposite player))))

#|
Given a board, it finds all the arrow destinations from that point.
It removes them if they are not legal.
|#
(defun arrow-destinations (board source)
  (remove-if-not #'(lambda (x) (legal-arrow? board source x)) (full-range source)))

#|
Generates all possible moves for a given player from the given board.
|#
(defun movegen (board player)
  ;first locate player
  ;then loop over full range of that location
  ;sub-loop over full range from that location (arrow possibilities)
  (let* ((location (locate board player))
		 (possible-movements (remove-if-not 
						   #'(lambda (x) (legal-movement? board player x))
						   (full-range location))))
	(movegen-helper board player possible-movements)))

(defun movegen-helper (board player movements)
  (if (null movements) nil
	(let* ((source (first movements))
		  (new-board (legal-movement? board player source)))
	  (append (mapcar #'(lambda(x) (legal-arrow? new-board source x)) 
					  (arrow-destinations new-board source))
			  (movegen-helper board player (rest movements))))))

#|
Function STATIC evaluates a position from the point of view of a particular player.
It returns a number -- the higher the number, the more desirable the position.  
|#

(defun static (pos player) (- (random 20) 10))
