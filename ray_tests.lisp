(load "given.lisp") ;functions from instructor
(load "core_ec.lisp") ;core evolutionary computation functions
(load "boolean_vectors.lisp") ;specific functions for the boolean vectors problem instance


;;;DEBUG DELETE
;;;(defparameter *samplepop* '((1 2) (2 2) (4 4)))
;;;(print (tournament-select-one *samplepop* #'random-elt    )    )

;;;DEBUG DELETE
;;;(print (tournament-selector 5 *samplepop* #'random-elt))

;;;(print (random-shuffle '(11 9 8 7 5 2)))

;;;debug DELETE
;;;(defparameter *bol1* '(1 1 0 0))
;;;(defparameter *bol2* '(0 0 1 1))
;;;(print (boolean-vector-modifier *bol1* *bol2*))

;;;DEBUG DELETE
;;;(print (boolean-vector-evaluator *bol1*))

#|
(defun random-shuffle (ind)
  (loop for i from (length ind) downto 2
        do (rotatef (elt ind (random i)) (elt ind (1- i))))
  ind
  )
|#

;;; an example way to fire up the GA.  It should easily discover
;;; a 100% correct solution.
#|
(evolve 50 100
 	:setup #'boolean-vector-sum-setup
	:creator #'boolean-vector-creator
	:selector #'tournament-selector
	:modifier #'boolean-vector-modifier
        :evaluator #'boolean-vector-evaluator
	:printer #'simple-printer)
|#

;;;(evolve 50 100 :setup #'boolean-vector-sum-setup :creator #'boolean-vector-creator :selector #'tournament-selector :modifier #'boolean-vector-modifier :evaluator #'boolean-vector-evaluator :printer #'simple-printer)

