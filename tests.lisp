(load "utils.lisp") ;functions from instructor
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
(print "running tests for symbolic_regression problem instance...")

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

;basic tree functionality -- trees are lists of lists essentially
(setf mytree '(a (b c) (d e (f (g h i j)) k)))
(elt mytree 0) ;curr node value
(typep (elt mytree 0) 'symbol)
(elt mytree 1) ;left child
(typep (elt mytree 1) 'cons)
(elt mytree 2) ;right child
(typep (elt mytree 2) 'cons)

(defun search-tree (tree)
  (dotimes (i (length tree)) ; Search children of this node
    (let ((curr (elt tree i)))
      (if (typep curr 'symbol) 
        (print "at a value!") ; curr is a value
        (search-tree curr) ;curr is another list, recurse
      )
    )
  )
) 


(print "running tests for ants_graph problem instance...")