; A struct for managing a finite state machine.
(defstruct fsm
	(alphabet '(a))
	(table '((a) (a)))
	(initial 0)
	(accepting '(0)))

; The hungry machine accepts when it is full.
; It will get hungry if it has to wait.
; It will get full if it eats.
(defparameter *hungry-machine* 
  (make-fsm 
	:alphabet '(eat wait)
	:table '((1 0) (1 0))
	:initial 0
	:accepting '(1)))

; The does-not-end-in-00 machine will accept sequences of letters in the alphabet '(0 1) that do not end with two 0's.
(defparameter *does-not-end-in-00*
  (make-fsm
	:alphabet '(0 1)
	:table '((1 0) (2 0) (2 0))
	:initial 0
	:accepting '(0 1)))

; The binary-divisible-by-3 machine will accept sequences representing binary numbers that are divisible by 3.
(defparameter *binary-divisible-by-3*
  (make-fsm 
	:alphabet '(0 1)
	:table '((0 1) (2 0) (1 2))
	:initial 0
	:accepting '(0)))

; The binary-not-divisible-by-3 machine will accept sequences representing binary numbers that are -not- divisible by 3.
(defparameter *binary-not-divisible-by-3*
  (make-fsm 
	:alphabet '(0 1)
	:table '((0 1) (2 0) (1 2))
	:initial 0
	:accepting '(1 2)))

; The divisible-by-3 machine will accept sequences representing base-10 integers that are divisible by 3.
(defparameter *divisible-by-3* 
  (make-fsm
	:alphabet '(0 1 2 3 4 5 6 7 8 9)
	:table 	 '((0 1 2 0 1 2 0 1 2 0)
			   (1 2 0 1 2 0 1 2 0 1)
			   (2 0 1 2 0 1 2 0 1 2))
	:initial 0
	:accepting '(0)))

; The not-divisible-by-3 machine will accept sequences representing base-10 integers that are divisible by 3.
(defparameter *not-divisible-by-3* 
  (make-fsm
	:alphabet '(0 1 2 3 4 5 6 7 8 9)
	:table 	 '((0 1 2 0 1 2 0 1 2 0)
			   (1 2 0 1 2 0 1 2 0 1)
			   (2 0 1 2 0 1 2 0 1 2))
	:initial 0
	:accepting '(1 2)))


; The gray-grey machine will accept sequences of letters from the alphabet (a e g r y), so long as that sequence contains either "gray" or "grey".
(defparameter *gray-grey-machine*
  (make-fsm
	:alphabet '(a e g r y)
	:table 	 '((0 0 1 0 0)
			   (0 0 1 2 0)
			   (3 3 1 0 0)
			   (0 0 1 0 4)
			   (4 4 4 4 4))
	:initial 0
	:accepting '(4)))

; A mystery machine. What do you think it does?
(defparameter *mystery-machine* 
  (make-fsm
	:alphabet '(0 1)
	:table '((3 1) (3 3) (3 1) (2 2))
	:initial 0
	:accepting '(1 4)))

; The function below will generate all sequences of a given length for a given alphabet. 
; Note: for large alphabets and/or large n, it may take a long time to process - a combinatorial explosion! 
(defun all-sequences-of-length (alphabet n)
  (cond ((null alphabet) nil)
		((= n 0) '(nil))
		(t (let ((smaller (all-sequences-of-length alphabet (- n 1))))
			 (apply #'append (mapcar #'(lambda (y) (mapcar #'(lambda (x) (cons y x)) smaller)) alphabet))))))
