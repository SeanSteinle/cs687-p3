;;;This file contains code which could apply to many problem types or instances. For example, it includes the evolve() function, tournament selection functions, and genetic computation functions.

;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 

(defun evolve (generations pop-size
	       &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"
  ;; The functions passed in are as follows:
  ;;(SETUP)                     called at the beginning of evolution, to set up
  ;;                            global variables as necessary
  ;;(CREATOR)                   creates a random individual
  ;;(SELECTOR num pop fitneses) given a population and a list of corresponding fitnesses,
  ;;                            selects and returns NUM individuals as a list.
  ;;                            An individual may appear more than once in the list.
  ;;(MODIFIER ind1 ind2)        modifies individuals ind1 and ind2 by crossing them
  ;;                            over and mutating them.  Returns the two children
  ;;                            as a list: (child1 child2).  Nondestructive to
  ;;                            ind1 and ind2.
  ;;(PRINTER pop fitnesses)     prints the best individual in the population, plus
  ;;                            its fitness, and any other interesting statistics
  ;;                            you think interesting for that generation.
  ;;(EVALUATOR individual)      evaluates an individual, and returns its fitness.
  ;;Pop will be guaranteed to be a multiple of 2 in size.
  ;;
  ;; HIGHER FITNESSES ARE BETTER

  ;; your function should call PRINTER each generation, and also print out or the
  ;; best individual discovered over the whole run at the end, plus its fitness
  ;; and any other statistics you think might be nifty.
  (funcall setup)
  (let*
      (population fitnesses (best -9999999999) current)
    (dotimes (j pop-size)
      (setf population (append population (list (funcall creator))))
      )
    (dotimes (i generations)
      (print i)
      (setf fitnesses '())
      (dotimes (j pop-size)
        (setf current (funcall evaluator (elt population j)))
        (if (> current best) (setf best current))
        (setf fitnesses (append fitnesses (list current)))
        )
      (funcall printer population fitnesses)
      (setf population (funcall selector pop-size population fitnesses))
      (let* ((new-pop))
        (dotimes (k (/ pop-size 2))
          (setf new-pop (append new-pop (funcall modifier (random-elt population) (random-elt population)))))
        (setf population new-pop)
        )
      )
    (format t "~% The best found was ~a" best)
    )
  )

;;; TOURNAMENT SELECTION

(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
  (let*(
        (n (random (length population)))
        (best (elt population n)) ;;;get random element from the population
        (bestf (elt fitnesses n)) ;;;get the fitnesses of that element
        )
    (dotimes (i (1- *tournament-size*))
      (let* (
             (n (random (length population)))
             (next (elt population n))
             (nextf (elt fitnesses n))
             )
        (if (> nextf bestf)
            (progn
              (setf best next)
              (setf bestf nextf)
              )
            )
        )
      )
    best
    ) 
  )


(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list"
  (let* (
         (result '())
         )
    (dotimes (i num)
      (setf result (append result (list (tournament-select-one population fitnesses))))
      )
    result
    )
  )

;;;; GP TREE CREATION CODE

;;; GP's tree individuals are considerably more complex to modify than
;;; simple vectors.  Get the GA system working right before tackling
;;; this part, trust me.

;; set up in the gp setup function -- for example, see
;; the code for gp-symbolic-regression-setup
(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)

;;; important hint: to use SETF to change the position of an element in a list
;;; or a tree, you need to pass into SETF an expression starting with the
;;; parent.  For example (setf (car parent) val) .... thus you HAVE to have
;;; access to the parent, not just the child.  This means that to store a
;;; "position", you have to store away the parent and the arg position of
;;; child, not the child itself.

(defun make-queue ()
  "Makes a random-queue"
  (make-array '(0) :adjustable t :fill-pointer t))
(defun enqueue (elt queue)
  "Enqueues an element in the random-queue"
  (progn (vector-push-extend elt queue) queue))
(defun queue-empty-p (queue)
  "Returns t if random-queue is empty"
  (= (length queue) 0))
(defun random-dequeue (queue)
  "Picks a random element in queue and removes and returns it.
Error generated if the queue is empty."
  (let ((index (random (length queue))))
    (swap (elt queue index) (elt queue (1- (length queue))))
    (vector-pop queue)))

(defun enqueue-children (q nonterminal)
  (let* ((arity (cadr nonterminal))
         (children (make-list arity :initial-element nil)))
    (dotimes (i arity)
      (let ((child-pointer (list nil)))
        (enqueue child-pointer q)
        (setf (nth i children) child-pointer)))
    (setf (cdr nonterminal) children)
    nonterminal))

(defun ptc2 (size)
  (declare (ignore size))
  "If size=1, just returns a random terminal. Else builds and
returns a tree by repeatedly extending the tree horizon with
nonterminals until the total number of nonterminals in the tree,
plus the number of unfilled slots in the horizon, is >= size.
Then fills the remaining slots in the horizon with terminals.
Terminals like X should be added to the tree
in function form (X) rather than just X."
  (if (= size 1)
      (random-elt *terminal-set*)  ; if size = 1, return random terminal
      (let ((q (make-queue))
            (root (copy-list (random-elt *nonterminal-set*)))
            (count 1))  ; initialize queue, root, count
        ;(format t "transforming and enqueueing first nonterminal: ~a~%" root)
        (setf root (enqueue-children q root))  ; transform root and enqueue root's children nodes
        ;(format t "done! q looks like: ~a~%" q)
        (while (< (+ count (length q)) size) nil  ; ADD NONTERMINALS UNTIL SIZE IS HIT
          (let ((new-term (copy-list (random-elt *nonterminal-set*)))
                (child-ref (random-dequeue q)))  ; dequeue random existing reference
            (setf (car child-ref) (enqueue-children q new-term))  ; transform new term and enqueue children nodes, then attach to existing tree via child-ref
            (incf count)))  ; increment count
        (while (> (length q) 0) nil
          (let ((new-term (list (random-elt *terminal-set*)))  ; create a list for terminal
                (child-ref (random-dequeue q)))
            (setf (car child-ref) new-term)))  ; attach terminal to existing tree
        root)))  ; return root

(defparameter *size-limit* 20)
(defun gp-creator ()
  "Picks a random number from 1 to 20, then uses ptc2 to create
a tree of that size"
  (ptc2 (1+ (random 20))))

;;; GP TREE MODIFICATION CODE

(defun num-nodes (tree &optional (node-count 0))
  (declare (ignore tree))
  "Returns the number of nodes in tree, including the root"
  (loop for curr in tree
        do (if (atom curr)
              (setf node-count (1+ node-count))
              (setf node-count (num-nodes curr node-count))
            )
  )
  node-count
)

(defvar current 0)
;;;remember to set to 0 before starting subtree?
(setf current -1)

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
 (except for root)."
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
            (if (and (typep i 'sequence) (> (length i) 1))
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
  ;;; this is best described with an example:
     ;; (dotimes (x 12)
     ;;        (print (nth-subtree-parent
     ;;                    '(a (b c) (d e (f (g h i j)) k))
     ;;                     x)))
  ;;; result:
  ;((A (B C) (D E (F (G H I J)) K)) 0) 
  ;((B C) 0) 
  ;((A (B C) (D E (F (G H I J)) K)) 1) 
  ;((D E (F (G H I J)) K) 0) 
  ;((D E (F (G H I J)) K) 1) 
  ;((F (G H I J)) 0) 
  ;((G H I J) 0) 
  ;((G H I J) 1) 
  ;((G H I J) 2) 
  ;((D E (F (G H I J)) K) 2) 
  ;0 
  ;1 
  ;NIL
)
  
(defun nsp-helper (tree n)
  (setf current -1)
  (nth-subtree-parent tree n)
)


(defparameter *mutation-size-limit* 10)
(defun gp-modifier (ind1 ind2)
  "Flips a coin.  If it's heads, then ind1 and ind2 are
crossed over using subtree crossover.  If it's tails, then
ind1 and ind2 are each mutated using subtree mutation, where
the size of the newly-generated subtrees is pickedc at random
from 1 to 10 inclusive.  Doesn't damage ind1 or ind2.  Returns
the two modified versions as a list."
  (let* (
         (i1 (deep-copy-list ind1))
         (i2 (deep-copy-list ind2))
         (i1-rand (random (num-nodes i1)))
         (i1-nsp (nsp-helper i1 i1-rand))
         (i1-parent (first i1-nsp))
         (i1-cindex (second i1-nsp))
         (i1-subtree (elt i1-parent (1+ i1-cindex)))
         
         (i2-rand (random (num-nodes i2)))
         (i2-nsp (nsp-helper i2 i2-rand))
         (i2-parent (first i2-nsp))
         (i2-cindex (second i2-nsp))
         (i2-subtree (elt i2-parent (1+ i2-cindex)))
         )
    (if (random?)
        (progn ;;crossover
          ;(format t "In crossover got parents pi1 (ci=~a): ~a~% and pi2 (ci=~a): ~a~%" i1-cindex i1-parent i2-cindex i2-parent)
          (setf (elt i1-parent (1+ i1-cindex)) i2-subtree)
          (setf (elt i2-parent (1+ i2-cindex)) i1-subtree)
          )
        (progn ;;modify
          ;(format t "In mutate got parents pi1 (ci=~a): ~a~% and pi2 (ci=~a): ~a~%" i1-cindex i1-parent i2-cindex i2-parent)
          (setf (elt i1-parent (1+ i1-cindex)) (ptc2 (1+ (random *mutation-size-limit*))))
          (setf (elt i2-parent (1+ i2-cindex)) (ptc2 (1+ (random *mutation-size-limit*))))
          )
        )
    (list i1 i2)
    )
  )
