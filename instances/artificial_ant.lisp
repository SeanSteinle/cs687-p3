;;; GP ARTIFICIAL ANT CODE
;;; for this part you'll need to implement both the evaluator AND
;;; make up the function that form the function set.

;;; In the artificial ant problem domain, you'll be evolving an s-expression
;;; which moves an ant around a toroidal map shown below.  The ant starts out
;;; at (0,0), facing east (to the right).  The functions in
;;; the expression are:
;;; (if-food-ahead --then-- --else--)   If food is directly ahead of the ant,
;;;                                     then evaluate the THEN expression, else
;;;                                     evaluate the ELSE expression
;;; (progn2 --item1-- --item2--)        Evaluate item1, then evaluate item 2
;;; (progn3 --item1-- --item2-- --item3--)  Evaluate item1, then item2, then item3
;;; (move)                              Move forward one unit
;;;                                     If you pass over a food pellet, it is eaten
;;;                                     and removed from the map.
;;; (left)                              Turn left
;;; (right)                             Turn right
;;;
;;;
;;; the way a tree is evaluated is as follows.  The whole tree is evaluated,
;;; and the functions move the ant about.  If the ant has not made a total of
;;; 600 MOVE, LEFT, and RIGHT operations yet, then the tree is evaluated AGAIN
;;; moving the ant some more from its current position.  This goes on until
;;; 600 operations have been completed, at which time the ant will not move
;;; any more.  If 600 operations are completed in the middle of the evaluation
;;; of a tree, the simplest approach is to have the MOVE command simply
;;; "stop working" so the ant doesn't gobble up any more pellets accidentally.

;;; The fitness of the artificial ant is the number of pellets it ate in the
;;; 600-operation period.  Maps are reset between evaluations of different
;;; individuals of course.

;;; Here's an optimal ant program (there are many such):
;;  (progn3 (if-food-ahead (move) (progn2 (left) (progn2 (left)
;;             (if-food-ahead (move) (right))))) (move) (right)))

;;; This is a hard problem for GP and you may need to run many times before you
;;; get a 100% perfect answer.

;;; Note that in my thesis it says 400 moves and not 600.  We're going with
;;; 600 here.  It's easier.



;;; our ant's food trail map
(defparameter *map-strs* '(
".###............................"
"...#............................"
"...#.....................###...."
"...#....................#....#.."
"...#....................#....#.."
"...####.#####........##........."
"............#................#.."
"............#.......#..........."
"............#.......#..........."
"............#.......#........#.."
"....................#..........."
"............#..................."
"............#................#.."
"............#.......#..........."
"............#.......#.....###..."
".................#.....#........"
"................................"
"............#..................."
"............#...#.......#......."
"............#...#..........#...."
"............#...#..............."
"............#...#..............."
"............#.............#....."
"............#..........#........"
"...##..#####....#..............."
".#..............#..............."
".#..............#..............."
".#......#######................."
".#.....#........................"
".......#........................"
"..####.........................."
"................................"))

(defparameter *map-height* 32)
(defparameter *map-width* 32)


;;; some useful functions for you

(defun make-map (lis)
  "Makes a map out of a string-list such as *MAP-STRS*"
  (let ((map (make-array (list (length (first lis)) (length lis)))))
    (dotimes (y (length lis) map)
      (dotimes (x (length (elt lis y)))
	(setf (aref map x y)
	      (cond ((equalp #\# (elt (elt lis y) x)) nil)
		    (t t)))))))

(defun direction-to-arrow (dir)
  "Returns a character which represents a given direction -- might
be useful for showing the movement along a path perhaps..."
  (cond ((= dir *n*) #\^)
	((= dir *s*) #\v)
	((= dir *e*) #\>)
	(t #\<)))

(defun maparray (function array &optional (same-type nil))
  "Maps function over array in its major order.  If SAME-TYPE,
then the new array will have the same element type as the old
array; but if a function returns an invalid element then an error
may occur.  If SAME-TYPE is NIL, then the array will accommodate
any type."
  (let ((new-array (apply #'make-array (array-dimensions array)
			  :element-type (if same-type
					    (array-element-type array)
					  t)
			  :adjustable (adjustable-array-p array)
			  (if (vectorp array)
			      `(:fill-pointer ,(fill-pointer array))
			    nil))))
    (dotimes (x (array-total-size array) new-array)
      (setf (row-major-aref new-array x)
	    (funcall function (row-major-aref array x))))))

(defun print-map (map)
  "Prints a map, which must be a 2D array.  If a value in the map
is T (indicating a space), then a '.' is printed.  If a value in the map
is NIL (indicating a food pellet), then a '#' is printed.  If a value in
the map is anything else, then it is simply PRINTed.  This allows you to
consider spaces to be all sorts of stuff in case you'd like to print a
trail of spaces on the map for example.  Returns NIL."
  (let ((dim (array-dimensions map)))
    (dotimes (y (second dim) nil)
      (format t "~%")
      (dotimes (x (first dim))
	(format t "~a"
		(let ((v (aref map x y)))
		  (cond ((equal v t) #\.)
			((null v) #\#)
			(t v))))))))


;; The four directions.  For relative direction, you might
;; assume that the ant always PERCEIVES things as if it were
;; facing north.
(defconstant *n* 0)
(defconstant *e* 1)
(defconstant *s* 2)
(defconstant *w* 3)



(defmacro absolute-direction (relative-dir ant-dir)
  "If the ant is facing ANT-DIR, then converts the perceived
RELATIVE-DIR direction into an absolute ('true') direction
and returns that."
  `(mod (+ ,relative-dir ,ant-dir) 4))

(defmacro x-pos-at (x-pos absolute-dir &optional (steps 1))
  "Returns the new x position if one moved STEPS steps the absolute-dir
direction from the given x position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *n*) ,x-pos)         ;; n or s
	      ((= ,absolute-dir *e*) (+ ,x-pos ,steps))     ;; e
	      (t (+ ,x-pos (- ,steps) *map-width*)))         ;; w
	*map-width*))

(defmacro y-pos-at (y-pos absolute-dir &optional (steps 1))
  "Returns the new y position if onee moved STEPS steps in the absolute-dir
direction from the given y position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *e*) ,y-pos)        ;; e or w
	      ((= ,absolute-dir *s*) (+ ,y-pos ,steps))     ;; s
	      (t (+ ,y-pos (- ,steps) *map-height*)))       ;; n
	*map-height*))


(defparameter *current-move* 0 "The move # that the ant is at right now")
(defparameter *num-moves* 600 "How many moves the ant may make")
(defparameter *current-x-pos* 0 "The current X position of the ant")
(defparameter *current-y-pos* 0 "The current Y position of the ant")
(defparameter *current-ant-dir* *e* "The current direction the ant is facing")
(defparameter *eaten-pellets* 0 "How many pellets the ant has eaten so far")
(defparameter *map* (make-map *map-strs*) "The ant's map")




;;; the function set you have to implement

(defmacro if-food-ahead (then else)
  (declare (ignore then))
  (declare (ignore else))
  "If there is food directly ahead of the ant, then THEN is evaluated,
else ELSE is evaluated"
  ;; because this is an if/then statement, it MUST be implemented as a macro.

    
)

;;;REMOVED DECLAIM FOR COMPILATION
(defun progn2 (arg1 arg2)
  (declare (ignore arg1))
  (declare (ignore arg2))
    "Evaluates arg1 and arg2 in succession, then returns the value of arg2"
)  ;; ...though in artificial ant, the return value isn't used ... 


(defun progn3 (arg1 arg2 arg3)
  (declare (ignore arg1))
  (declare (ignore arg2))
  (declare (ignore arg3))
  "Evaluates arg1, arg2, and arg3 in succession, then returns the value of arg3"
)  ;; ...though in artificial ant, the return value isn't used ...

(defun move ()
  "If the move count does not exceed *num-moves*, increments the move count
and moves the ant forward, consuming any pellet under the new square where the
ant is now.  Perhaps it might be nice to leave a little trail in the map showing
where the ant had gone."

      
  )


(defun left ()
  "Increments the move count, and turns the ant left"

      
)

(defun right ()
  "Increments the move count, and turns the ant right"

      
)

(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)

;; I provide this for you
(defun gp-artificial-ant-setup ()
  "Sets up vals"
  (setq *nonterminal-set* '((if-food-ahead 2) (progn2 2) (progn3 3)))
  (setq *terminal-set* '(left right move))
  (setq *map* (make-map *map-strs*))
  (setq *current-move* 0)
  (setq *eaten-pellets* 0))


;; you'll need to implement this as well

(defun gp-artificial-ant-evaluator (ind)
  (declare (ignore ind))
  "Evaluates an individual by putting it in a fresh map and letting it run
for *num-moves* moves.  The fitness is the number of pellets eaten -- thus
more pellets, higher (better) fitness."

      
)

;; you might choose to write your own printer, which prints out the best
;; individual's map.  But it's not required.

#|
(evolve 50 500
 	:setup #'gp-artificial-ant-setup
	:creator #'gp-creator
	:selector #'tournament-selector
	:modifier #'gp-modifier
        :evaluator #'gp-artificial-ant-evaluator
	:printer #'simple-printer)
|#
