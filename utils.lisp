;;;This file defines a grab-bag of functions which are useful across the project. Many but not all were provided by the instructor.

(defmacro swap (elt1 elt2)
  "Swaps elt1 and elt2, using SETF.  Returns nil."
  (let ((temp (gensym)))
    `(let ((,temp ,elt1))
       (setf ,elt1 ,elt2)
       (setf ,elt2 ,temp)
       nil)))

(defmacro while (test return-value &body body)
  "Repeatedly executes body as long as test returns true.
Then evaluates and returns return-value"
  `(progn (loop while ,test do (progn ,@body)) ,return-value))

(defun random-elt (sequence)
  "Returns a random element from sequence"
  (elt sequence (random (length sequence))))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the test used for duplicates."
  (let (bag)
    (while (< (length bag) num) bag
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))))

(defun simple-printer (pop fitnesses)  ;; I'm nice and am providing this for you.  :-)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses))

(defun random-shuffle (ind)
  (loop for i from (length ind) downto 2
        do (rotatef (elt ind (random i)) (elt ind (1- i))))
  ind
  )

(defun nth-subtree-parent (tree n)
  "Given a tree, finds the nth node by depth-first search though
the tree, not including the root node of the tree (0-indexed). If the
nth node is NODE, let the parent node of NODE is PARENT,
and NODE is the ith child of PARENT (starting with 0),
then return a list of the form (PARENT i).  For example, in
the tree (a (b c d) (f (g (h) i) j)), let's say that (g (h) i)
is the chosen node.  Then we return ((f (g (h) i) j) 0).

If n is bigger than the number of nodes in the tree
 (not including the root), then we return n - nodes_in_tree
 (except for root).
 
 NOTE: Requires setting global variable current=-1 before and after invocations.
 We tried writing a wrapper but didn't work right. Such is life.
 "
  (nsp-helper tree n)
;  (if (< n (num-nodes tree)) ;structure other cases here
;
;    (- (num-nodes tree) n)
;  )
  )
  
(defun nsp-helper (tree n)
  """Implements recursive cases (n!=0 AND n<len(tree)) for nth-subparent-tree."""
  (let* ((place 0) (result (list tree 0)))
    (dolist (i (rest tree))
      (setf current (1+ current))
      (if (= current n)
          (progn
            (setf result (list tree place))
            (return)
            )
          )
      (if (< current n)
          (progn
            (if (typep i 'sequence)
                (progn
                  (setf result (nth-subtree-parent i n))
                  )
                )
            )
          )
      (setf place (1+ place))
      )
    result
    )
)
