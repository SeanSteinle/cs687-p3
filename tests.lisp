(load "h3.lisp")

;BOOLEAN FUNCTION TESTS

(defun test-boolean-vector-creator ()
    (defparameter *boolean-vector-length* 5)
    (let ((ind1 (boolean-vector-creator)))
        (if (not (= (length ind1) 5))
            (error "BV-CREATE failed! Produced incorrect length vector: ~a~%" ind1)
        )
    )
)

(defun test-boolean-vector-modifier ()
    (defparameter *boolean-vector-length* 5)
    (let ((i1 (boolean-vector-creator)) (i2 (boolean-vector-creator)))
        (multiple-value-bind (c1 c2) (boolean-vector-modifier i1 i2)
            (if (or (not (= (length c1) 5)) (not (= (length c2) 5)))
                (error "BV-MODIFY failed! Yielded incorrect lengths for children. C1: ~a~%C2:~a~%" c1 c2)
            )
        )
    )
)

(defun test-boolean-vector-evaluator ()
    (let* ((ind1 (list 1 1 1 1 0)) (fitness (boolean-vector-evaluator ind1)))
        (if (= fitness 4)
            (error "BV-EVALUATE failed! Yielded incorrect fitness (~a) for individual: ~a~%" fitness ind1)
        )
    )
)

(defun test-boolean-vector-selection ()
    ;test for tournament size 1, tournament size 1000000
    (let ((mypop (list 1 2 3 4 5)) (myvals (list 10 1 1 1 1)))
        (progn
            (setf *tournament-size* 1000000) ;WARNING: may get really bad luck, try again.
            (let ((champion (tournament-select-one mypop myvals)))
                (if (not (= champion 1))
                    (error "Champion should have been 1.")
                )
            )
        )
    )
)

(test-boolean-vector-creator)
(test-boolean-vector-modifier)
(test-boolean-vector-evaluator)
(test-boolean-vector-selection) ;tournament selection