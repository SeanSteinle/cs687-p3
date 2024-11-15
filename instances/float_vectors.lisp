;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM

;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; GA Max-ONES Problem.  Assume that you are evolving a vector
;;; of floating-point numbers *float-vector-length* long.  


;;; The default test function is Rastrigin.
;;;; Rastrigin is defined in section 11.2.2 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other floating-point functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :rastrigin
;;; :rosenbrock
;;; :griewank
;;; :schwefel

;;; If you were really into this, you might also try testing on
;;; rotated problems -- some of the problems above are linearly
;;; separable and rotation makes them harder and more interesting.
;;; See the end of Section 11.2.2.

;;; I have defined the minimum and maximum gene values for rastrigin
;;; as [-5.12, 5.12].  Other problems have other traditional min/max
;;; values, consult Section 11.2.2.



(defparameter *float-vector-length* 100)
(defparameter *float-problem :rastrigin)
(defparameter *float-min* -5.12) ;; these will change based on the problem
(defparameter *float-max* 5.12)  ;; likewise

(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
(ahem) various global variables which define the problem being evaluated
and the floating-point ranges involved, etc.  I dunno."
  )

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
random numbers in the range appropriate to the given problem"
  (let* ((result))
    (dotimes (i *float-vector-length*)
      (setf result (append result (list (+ (random (- *float-max* *float-min*)) *float-min*))))
      )
    result
    )

;;; you might as well use random uniform numbers from *float-vector-min*
;;; to *float-vector-max*.  
  )


(defparameter *float-crossover-probability* 0.2)
(defparameter *float-mutation-probability* 0.1) ;; I just made up this number
(defparameter *float-mutation-variance* 0.01) ;; I just made up this number
(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."
  (let* (
         (copy1 (copy-list ind1))
         (copy2 (copy-list ind2))
         )
    (dotimes (i (length copy1))
      (if (random? *float-crossover-probability*)
          (swap (elt copy1 i) (elt copy2 i))
          )
      (if (random? *float-mutation-probability*)
          (loop
            (let ((num (box-muller)))
              (if (and (< (+ (elt copy1 i) num) *float-max*) (> (+ (elt copy1 i) num) *float-min*))
                  (progn
                    (setf (elt copy1 i) (+ num (elt copy1 i)))
                    (return)
                    ))
              )
            )
          )
      (if (random? *float-mutation-probability*)
          (loop
            (let ((num (box-muller)))
              (if (and (< (+ (elt copy2 1) num) *float-max*) (> (+ (elt copy2 i) num) *float-min*))
                  (progn
                    (setf (elt copy2 i) (+ num (elt copy2 i)))
                    (return)
                    )
                  )
              )
            )
          )
      )
    (list copy1 copy2)
    )

;;; Note: crossover is probably identical to the bit-vector crossover
;;; See "Gaussian Convolution" (Algorithm 11) in the book for mutation
  )

;;;1 variable random gaussian using 2 uniform
(defun box-muller ()
  (* (sqrt (* -2 (log (random 1.0)))) (cos (* 2 pi (random 1.0))))
  )

(defun float-vector-sum-evaluator (ind1)
  "Evaluates an individual, which must be a floating point vector, and returns
its fitness."
  (let* ((result 0))
    (setf result (+ result (* 10 (length ind1))))
    (dolist (i ind1)
      (setf result (+ result (- (* i i) (* 10 (cos (* 2 pi i))))))
      )
    (* -1 result) ;;;negative because we want higher fitnesses to be better
    )

  )