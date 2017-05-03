;;;; vector arithmetic

 (register-predicate! vector? 'vector)

(define (vector-element-wise element-procedure)
  (lambda vecs ; note: takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
               (not (n:= (vector-length v)
                         first-vec-length)))
             vecs)
        (error "Vector dimension mismatch:" vecs))))

;; checked every time an operator is called
(define use-element-arithmetic-operators? #t)

(define (vector-extender element-arithmetic)
  (define (internal-operator operator)
    ;; procedure for operator has to be wrapped inside a lambda so that
    ;; the value of +, *, etc. will be determined each time the operator
    ;; is called, after the new arithmetic has been installed
    (lambda args
      (apply (if use-element-arithmetic-operators?
                 (operation-procedure
                  (arithmetic-operation operator element-arithmetic))
                 (case operator
                   ((+) +)
                   ((*) *)
                   ((-) -)
                   ((negate) -)
                   ((sqrt) sqrt)))
             args)))
  (define element?
    (arithmetic-domain-predicate element-arithmetic))
  (let ((+ (internal-operator '+))
        (* (internal-operator '*))
        (- (internal-operator '-))
        (negate (internal-operator 'negate))
        (sqrt (internal-operator 'sqrt)))
    (define (coerce a size-reference)
      (if (element? a)
          (make-vector (vector-length size-reference) a)
          a))
    (define (dot-product a b)
      (reduce +
              (arithmetic-constant 'additive-identity element-arithmetic)
              (vector->list ((vector-element-wise *) a b))))
    (define (magnitude v)
      (sqrt (dot-product v v)))
    (define (multiplication a b)
      (if (and (vector? a) (vector? b))
          (dot-product a b)
          ((vector-element-wise *) (coerce a b) (coerce b a))))
    (make-arithmetic 'vector vector? (list element-arithmetic)
      (lambda (name element-constant)
        element-constant)
      (lambda (operator element-operation)
        (let ((all (lambda (procedure)
                     (simple-operation operator vector? procedure)))
              
               (any (lambda (procedure)
                      (make-operation operator
                                       (any-arg (operator-arity operator)
                                                vector?
                                                element?)
                                      procedure))))
          (case operator
            ((+)         (all (vector-element-wise +)))
            ((-)         (all (vector-element-wise -)))
            ((negate)    (all (vector-element-wise negate)))
             ((*)         (any multiplication))
            ((magnitude) (all magnitude))
            (else (all (lambda args
                         (error "Invalid vector operator:" operator))))))))))
