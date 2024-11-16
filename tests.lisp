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

;basic test for ptc2
(typep (ptc2 1) 'symbol) ;for ptc2 1, should return X
(ptc2 2) ;this works most of the time, BUT we're actually modifying the nonterminal set and we don't want to do that


(print "running tests for ants_graph problem instance...")