(load "utils.lisp") ;functions from instructor (must be imported before core_ec)
(load "core_ec.lisp") ;core evolutionary computation functions

(print "running tests for utils...")
(print (random-shuffle '(11 9 8 7 5 2)))

(print "running tests for high-level EC functions...")
;TOURNAMENT SELECTION
(defparameter *samplepop* '((1 2) (2 2) (4 4)))
(print (tournament-select-one *samplepop* (list 1 1 1))) ;selects a random elem from samplepop
(print (tournament-selector 5 *samplepop* (list 1 1 1))) ;selects a random elem 5 times

(load "instances/boolean_vectors.lisp") ;specific functions for the boolean vectors problem instance
(print "running tests for the boolean_vectors problem instance...")

;UNIT TESTS
;boolean-vector CREATE
(print (boolean-vector-creator))

;boolean-vector MODIFY
(defparameter *bol1* '(1 1 0 0))
(defparameter *bol2* '(0 0 1 1))
(print (boolean-vector-modifier *bol1* *bol2*))

;boolean-vector EVALUATE
(defparameter *bol1* '(1 1 0 0))
(print (boolean-vector-evaluator *bol1*)) ;should be == 2

;END-TO-END TEST
#|
(evolve 50 100
 	:setup #'boolean-vector-sum-setup
	:creator #'boolean-vector-creator
	:selector #'tournament-selector
	:modifier #'boolean-vector-modifier
        :evaluator #'boolean-vector-evaluator
	:printer #'simple-printer)
|#

(load "instances/float_vectors.lisp")
(print "running tests for float_vectors problem instance...")

;UNIT TESTS
;float_vector CREATE
(print (float-vector-creator))

;float_vector MODIFY
(print (float-vector-modifier '(1.28829 2.18818) '(2.18818 1.28829)))

;float_vector EVALUATE
(print (float-vector-sum-evaluator '(1.28829 2.18818)))

;END-TO-END TEST
#|
(evolve 50 100
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'float-vector-sum-evaluator
	:printer #'simple-printer)
|#

(load "instances/symbolic_regression.lisp")
(print "running tests for generic genetic programming problems...")

;basic queue functionality
(gp-symbolic-regression-setup)
(setf q (make-queue))
(enqueue (random-elt *nonterminal-set*) q)
(enqueue (random-elt *nonterminal-set*) q)
(enqueue (random-elt *nonterminal-set*) q)
(print (queue-empty-p q)) ;should be nil
(random-dequeue q)
(random-dequeue q)
(random-dequeue q)
(print (queue-empty-p q)) ;should be true

;basic tree pointers -- trees are lists of lists essentially
(setf mytree '(+ (* (x) (* (+ (x) (* (x) (x))) (x))) (* (+ (x) (cos (- (x) (x)))) (x))))
(elt mytree 0) ;curr node value
(print (typep (elt mytree 0) 'symbol))
(elt mytree 1) ;left child
(print (typep (elt mytree 1) 'cons))
(elt mytree 2) ;right child
(print (typep (elt mytree 2) 'cons))
(print (num-nodes mytree)) ;should be 18

;enqueue-children test - how enqueue-children modifies myq
(setf myq (make-queue))
(enqueue-children myq '(+ 2))
(print (not (queue-empty-p myq))) ;myq should be nonempty
(random-dequeue myq)
(random-dequeue myq)
(print (queue-empty-p myq)) ;myq is now empty

;enqueue-children test - how enqueue-children allows for the modification of myt
(setf myt '(+ 2))
(setf myq (make-queue))
(setf myt (enqueue-children myq myt)) ;enqueue's children + transforms nonterminal into node w/ empty pointers
(print myt) ;(+ (NIL) (NIL))
(print myq) ;((NIL) (NIL))
(let ((child-ref (random-dequeue myq))) ;; Modify the dequeued child-ref directly
  (setf (car child-ref) '(- 2)))  ;; Modify the car of the dequeued list
(print myt) ;(+ (NIL) (- 2))
(print myq) ;((NIL))

;basic tests for ptc2
(typep (ptc2 1) 'symbol) ;for ptc2 1, should return X
(ptc2 2) ;should only have a single non-terminal operator
(ptc2 10) ;should be much larger
(gp-creator)

(setf simple-tree '(* (X) (- (X) (X))))
(setf mytree '(+ (* (x) (* (+ (x) (* (x) (x))) (x))) (* (+ (x) (cos (- (x) (x)))) (x))))

(defvar current -1) ;set global var
(setf current -1) ;;;remember to set to -1 before starting subtree?

(dotimes (x 10)
  (print (list x (nth-subtree-parent simple-tree x)    )  )
  (setf current -1) ;;have to reset special variable before every run or won't work
  )

(dotimes (x 20)
  (print (list x (nth-subtree-parent mytree x)    )  )
  (setf current -1) ;;have to reset special variable before every run or won't work
  )

;testing GP modify -- 
#| TD
;IT LOOKS LIKE mutate is modifying the inds!
;also we're gettting an error where the simple-tree variable becomes unrecognizable?
;might a deep copy save us from both of these?
;SEEMS like this is solved ^... BUT new problem wrt not enough wrapping after modification:
"running tests for symbolic regression problem instance..." Original Tree: (*
                                                                            (X)
                                                                            (-
                                                                             (X)
                                                                             (X)))
T1: (* (X) (- (X) (+ ((X)) ((X)))))
T2: (* (X) (- X (X)))

"running tests for symbolic regression problem instance..." Original Tree: (*
                                                                            (X)
                                                                            (-
                                                                             (X)
                                                                             (X)))
T1: (* (X) (- (X) X))
T2: (* (X) (- (SIN ((COS ((EXP ((- ((X)) ((X))))))))) (X)))

is it just a terminals thing

errg still getting:
my simple tree: (* (X) (- (X) (X)))


; file: /Users/ssteinle/Desktop/gmu/cs687/cs687-p3/tests.lisp
; in:
;      LET* ((NEW-TREES (GP-MODIFIER SIMPLE-TREE SIMPLE-TREE))
;        (TREE1 (FIRST NEW-TREES)) (TREE2 (SECOND NEW-TREES)))
;     (GP-MODIFIER SIMPLE-TREE SIMPLE-TREE)
; 
; caught WARNING:
;   undefined variable: COMMON-LISP-USER::SIMPLE-TREE

;     (FORMAT T "Original Tree: ~a~%T1: ~a~%T2: ~a~%" SIMPLE-TREE TREE1 TREE2)
; 
; caught WARNING:
;   undefined variable: COMMON-LISP-USER::SIMPLE-TREE
; 
; compilation unit finished
;   Undefined variable:
;     SIMPLE-TREE
;   caught 2 WARNING conditions
While evaluating the form starting at line 157, column 0
  of #P"/Users/ssteinle/Desktop/gmu/cs687/cs687-p3/tests.lisp":"running tests for symbolic regression problem instance..." 

debugger invoked on a SB-KERNEL:INDEX-TOO-LARGE-ERROR in thread
#<THREAD tid=259 "main thread" RUNNING {70083802B3}>:
  The index 1 is too large for a list of length 1.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [RETRY   ] Retry EVAL of current toplevel form.
  1: [CONTINUE] Ignore error and continue loading file "/Users/ssteinle/Desktop/gmu/cs687/cs687-p3/tests.lisp".
  2: [ABORT   ] Abort loading file "/Users/ssteinle/Desktop/gmu/cs687/cs687-p3/tests.lisp".
  3:            Exit debugger, returning to top level.

(ELT (X) 1)

I think this error comes from whenever we choose a terminal as a parent:
        (progn ;;crossover
          (setf (elt i1-parent (1+ i1-cindex)) i2-subtree)
          (setf (elt i2-parent (1+ i2-cindex)) i1-subtree)
          )
        (progn ;;modify
          (setf (elt i1-parent (1+ i1-cindex)) (ptc2 (1+ (random *mutation-size-limit*))))
          (setf (elt i2-parent (1+ i2-cindex)) (ptc2 (1+ (random *mutation-size-limit*))))
          )
        )
As you can see, the problem is that the minimum index we use is 1 but when the parent is a terminal we can only index to 0
BUT THEN AGAIN, we shouldn't be getting terminals for the parent spots right?
^this might be an ants vs. symbolic-regression problem
|#

(format t "~%my simple tree: ~a~%" simple-tree)
(print "running tests for symbolic regression problem instance...") ;it's already loaded
(let* (
	(new-trees (gp-modifier simple-tree simple-tree))
	(tree1 (first new-trees))
	(tree2 (second new-trees))
)
	(format t "Original Tree: ~a~%T1: ~a~%T2: ~a~%" simple-tree tree1 tree2)
)

(dotimes (x 100) (gp-modifier simple-tree simple-tree)) ;can we do many modifications without error?

(print "running tests for ants_graph problem instance...")
(load "instances/artificial_ant.lisp")
(gp-artificial-ant-setup);;;a setup needs to be called for ptc2 to work properly, will make the code look weired when it hits the ptc generation though
(print (gp-modifier '(a (b c) (d e (f (g h i j)) k)) '(l (m n o) (p (q (r) s) t))))