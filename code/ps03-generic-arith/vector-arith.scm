;;;; Vector arithmetic

(register-predicate! vector? 'vector) ; vector? is already a defined predicate in the scheme language


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

; Using installed +
;(define (vector-extender base-arithmetic)
;  (make-arithmetic 'vector vector? (list base-arithmetic)
;    (lambda (name base-constant)
;      (case name
;        ((additive-identity) #(0))
;        ((multiplicative-identity) #(1))
;        (else (base-constant))))
;    (let ((base-predicate
;            (arithmetic-domain-predicate base-arithmetic)))
;      (lambda (operator base-operation)
;        (let ((procedure
;                (case operator
;                  ((+) (vector-element-wise +))
;                  ((-) (vector-element-wise -))
;                  ((negate) (vector-element-wise -))
;                  (else
;                    (lambda args
;                      (error "operator undefined in vector"
;                        operator))))))
;        (make-operation operator
;                        (any-arg (operator-arity operator)
;                                 vector?
;                                 base-predicate)
;                        procedure))))))


;; Using base arithmetic
(define (get-op-from-arith operator arithmetic)
  (operation-procedure (arithmetic-operation operator arithmetic)))

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
                  ;((+) (vector-element-wise (get-op-from-arith '+ base-arithmetic)))
                  ((+) (vector-scalar-plus (get-op-from-arith '+ base-arithmetic)))
                  ((-) (vector-element-wise (get-op-from-arith '- base-arithmetic)))
                  ;((*) (dot-product-maker (get-op-from-arith '+ base-arithmetic) (get-op-from-arith '* base-arithmetic)))
                  ((*) (vector-scalar-multiplication (get-op-from-arith '+ base-arithmetic) (get-op-from-arith '* base-arithmetic)))
                  ((/) (vector-scalar-division (get-op-from-arith '/ base-arithmetic)))
                  ((negate) (vector-element-wise (get-op-from-arith 'negate base-arithmetic)))
                  ((magnitude) (vector-magnitude-maker (get-op-from-arith '+ base-arithmetic) (get-op-from-arith '* base-arithmetic) (get-op-from-arith 'sqrt base-arithmetic)))
                  (else
                    (lambda args
                      (error "operator undefined in vector"
                        operator))))))
        (make-operation operator
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

;(define element?
;    (arithmetic-domain-predicate element-arithmetic))
(define (coerce a size-reference)
      (if (vector? a)
		  a
          (make-vector (vector-length size-reference) a)
          ))

(define (vector-scalar-plus +)
  (lambda (a b)
      (cond ((and (not (vector? a)) (not (vector? b))) (+ a b))
            ((and (vector? a) (vector? b)) ((vector-element-wise +) a b))
            ((vector? a) ((vector-element-wise +) (coerce a b) (coerce b a))) ;(vector-map (lambda (x) (* x b)) a))
            ((vector? b) ((vector-element-wise +) (coerce a b) (coerce b a))) ;(vector-map (lambda (x) (* x a)) b))
            (else (lambda args (error "unknown arguments for vector-scalar-plus")))
      ))
)


(define (vector-scalar-division /)
  (lambda (a b)
      (cond ((and (not (vector? a)) (not (vector? b))) (/ a b))
            ((and (vector? a) (vector? b)) ((vector-element-wise /) a b))
            ((vector? a) ((vector-element-wise /) (coerce a b) (coerce b a))) ;(vector-map (lambda (x) (* x b)) a))
            ((vector? b) ((vector-element-wise /) (coerce a b) (coerce b a))) ;(vector-map (lambda (x) (* x a)) b))
            (else (lambda args (error "unknown arguments for vector-scalar-division")))
      ))
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





