;;;;;; BOOLEAN VECTOR GENETIC ALGORTITHM

;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; boolean vectors Problem.  Assume that you are evolving a bit vector
;;; *vector-length* long.  

;;; The default test function is Max-Ones.
;;;; Max-ones is defined in section 11.2.1 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other boolean functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :max-ones
;;; :trap
;;; :leading-ones
;;; :leading-ones-blocks

(defparameter *boolean-vector-length* 100)
(defparameter *boolean-problem* :max-ones)
;; perhaps other problems might include... 

(defun boolean-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this to set up various
(ahem) global variables to define the problem being evaluated, I dunno."
  )

(defun boolean-vector-creator ()
  "Creates a boolean-vector *boolean-vector-length* in size, filled with
random Ts and NILs, or with random 1s and 0s, your option."
  (let
      ((result '()))
    (dotimes (i *boolean-vector-length*)
      (setf result (append result (list (random 2))))
      )
    result
    )
  )

(defparameter *boolean-crossover-probability* 0.2)
(defparameter *boolean-mutation-probability* 0.01)
(defun boolean-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation simply flips the bit of the allele."
  (let* (
         (copy1 (copy-list ind1))
         (copy2 (copy-list ind2))
         )
    (dotimes (i (length copy1))
      (if (random? *boolean-crossover-probability*)
          (swap (elt copy1 i) (elt copy2 i))
          )
;;;can do this after we swap because it's already the new allele up to that point
      (if (random? *boolean-mutation-probability*)
          (setf (elt copy1 i) (mod (1+ (elt copy1 i)) 2))
        )
      (if (random? *boolean-mutation-probability*)
          (setf (elt copy2 i) (mod (1+ (elt copy2 i)) 2))
          )
    )
;;;(setf ind1 (random-shuffle ind1))
;;;(setf ind2 (random-shuffle ind2))
  (list copy1 copy2))
    
  )

(defun boolean-vector-evaluator (ind1)
  "Evaluates an individual, which must be a boolean-vector, and returns
its fitness."
  (apply '+ ind1) ;note--this is for all1's case only, not other cases!
    
)