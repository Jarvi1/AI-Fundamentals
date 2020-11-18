;;; Breadth-first search

;;; Assumes you have defined
;;; 1. A set of operators as lisp functions
;;; 2. a function solution-state?(description) to determine if a state is a solution

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UTILITIES TO MEASURE SIZE OF SEARCH SPACE

(defstruct search-statistics
  (nodes-visited 0)
  (maximum-length-of-node-list 0)
  (length-of-solution 0)
  (maximum-depth 0)
  (cost-of-solution 0))

(defparameter *stats* (make-search-statistics))

(defun reset-stats()
  (setq *stats* (make-search-statistics)))
        
(defun update-statistics (stats e node-list)
  "Changes stat structure to reflect new number of nodes visited,
  and optionally new maximum length and depth"
  (setf (search-statistics-nodes-visited stats)
        (+ 1 (search-statistics-nodes-visited stats)))
  (when (> (length node-list)
           (search-statistics-maximum-length-of-node-list stats))
    (setf (search-statistics-maximum-length-of-node-list stats)
          (length node-list)))
  (when (> (length (node-path e))
           (search-statistics-maximum-depth stats))
    (setf (search-statistics-maximum-depth stats)
          (length (node-path e)))))
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SEARCH UTILITIES

(defstruct node ;elements of node-list
  state
  path)

(defun successor-state (description operator)
  (if (fboundp operator)  ;;if it has a function definition
    (funcall (symbol-function operator) description)
    (error "The operator ~a does not have a function definition" operator)))

(defun successor-node(node operator)
  (let ((next (successor-state (node-state node) operator)))
    (when next
      (make-node :state next
                 :path (add-to-end operator (node-path node))))))


(defun add-to-end(atom list)
  "Create a new list with atom at the end of list"
  (append list (list atom)))

(defun add-to-front(atom list)
  "Create a new list with atom at the front of list"
  (cons atom list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BREADTH FIRST SEARCH 

(defun breadth-first-search (initial-state operators)
  (let ((node-list (list (make-node :state initial-state 
                                    :path nil)))
        (solved nil)
        (next-node nil)
        (visited nil)
        (successors nil)
        (e nil))
    (loop until (or (null node-list) ;;no more states
                    solved)
          do (setq e (first node-list))
          (update-statistics *stats* e node-list)
		  (setq visited (cons (node-state e) visited))
          (setq node-list (rest node-list))
          (cond ((solution-state? (node-state e)) 
                 ;;if e is a solution, exit with success  
                 (setq solved t))
                (t ;;otherwise add successors to the end of node-list
                 (setq successors nil)
                 (loop for rule in operators 
                       do (setq next-node (successor-node e rule))
                       (when (and next-node
                                  (not (already-visited? (node-state next-node) visited)))
                         (setq successors (add-to-end next-node successors))))
                 (setq node-list (append node-list successors)))))
    (when solved
      (setf (search-statistics-length-of-solution *stats*)
            (length (node-path e)))
      e)))

(defun already-visited? (state visited)
  (member state visited :test #'equalp))


