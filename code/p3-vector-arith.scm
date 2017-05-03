;;;; Vector arithmetic

(p3-register-predicate! vector? 'vector) ; vector? is already a defined predicate in the scheme language


(define (vector-element-wise element-procedure)
  (lambda vecs  ; Note: this takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

; this procedure does not return anything. it just raises an error if needed
(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
                (not (n:= (vector-length v)
                          first-vec-length)))
              vecs)
        (error "Vector dimension mismatch:" vecs))))

;; Using base arithmetic
(define (get-op-from-arith operator arithmetic)
  (p3-operation-procedure (arithmetic-operation operator arithmetic)))

(define (vector-extender base-arithmetic)
  (make-arithmetic 'vector vector? (list base-arithmetic)
    (lambda (name base-constant)
      (case name
        ((additive-identity) #(0))
        ((multiplicative-identity) #(1))
        (else (base-constant))))
    (let ((base-predicate
            (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
        (let ((procedure
                (case operator
                  ((+) (vector-element-wise (get-op-from-arith '+ base-arithmetic)))
                  ((-) (vector-element-wise (get-op-from-arith '- base-arithmetic)))
                  ;((*) (dot-product-maker (get-op-from-arith '+ base-arithmetic) (get-op-from-arith '* base-arithmetic)))
                  ((*) (vector-scalar-multiplication (get-op-from-arith '+ base-arithmetic) (get-op-from-arith '* base-arithmetic)))
                  ((negate) (vector-element-wise (get-op-from-arith 'negate base-arithmetic)))
                  ((magnitude) (vector-magnitude-maker (get-op-from-arith '+ base-arithmetic) (get-op-from-arith '* base-arithmetic) (get-op-from-arith 'sqrt base-arithmetic)))
                  (else
                    (lambda args
                      (error "operator undefined in vector"
                        operator))))))
        (p3-make-operation operator
                        (any-arg (operator-arity operator)
                                 vector?
                                 base-predicate)
                        procedure))))))

(define (vector-magnitude-maker + * sqrt)
  (let ((dot-product (dot-product-maker + *)))
    (define (vector-magnitude v)
      (sqrt (dot-product v v)))
    vector-magnitude))

(define (dot-product-maker + *)
  (lambda (u v)
    (reduce-left + 0 (vector->list ((vector-element-wise *) u v)))))


(define (vector-scalar-multiplication + *)
  (lambda (a b)
    (let ((dp (dot-product-maker + *)))
      (cond ((and (not (vector? a)) (not (vector? b))) (* a b))
            ((and (vector? a) (vector? b)) (dp a b))
            ((vector? a) (vector-map (lambda (x) (* x b)) a))
            ((vector? b) (vector-map (lambda (x) (* x a)) b))
            (else (lambda args (error "unknown arguments for vector-scalar-multiplication")))
      )))
)


(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))
(install-arithmetic! combined-arithmetic)

(define vector-arithmetic
  (extend-arithmetic vector-extender combined-arithmetic))

(define vec-before-func
  (extend-arithmetic
    function-extender
    (extend-arithmetic vector-extender combined-arithmetic)))

(define func-before-vec
  (extend-arithmetic
    vector-extender
    (extend-arithmetic function-extender combined-arithmetic)))





