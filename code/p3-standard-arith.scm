;;;; Symbolic arithmetic

(define (symbolic? object)
  (or (symbol? object)
      (pair? object)))
(p3-register-predicate! symbolic? 'symbolic)

(define (symbolic-extender base-arithmetic)
  (make-arithmetic 'symbolic symbolic? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
           (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
        (p3-make-operation operator
                        (any-arg (operator-arity operator)
                                 symbolic?
                                 base-predicate)
                        (lambda args (cons operator args)))))))

;;;; Function arithmetic

(define function? procedure?)
(p3-register-predicate! function? 'function)

(define (function-extender codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic 'function
                     (disjoin codomain-predicate function?)
                     (list codomain-arithmetic)
      (lambda (name codomain-constant)
        codomain-constant)
      (lambda (operator codomain-operation) 
        ; Note! in generic-arithmetic, you will loop over a bunch of operations and apply define-generic-procedure-handler to them
        ; and here is where the operations get defined
        ; so these operations operate with any-arg
        ; a handler is built with each such operation
        (p3-make-operation operator
                        (any-arg (operator-arity operator)
                                 function?
                                 codomain-predicate)
          (lambda things
            (lambda args
              (p3-apply-operation codomain-operation
                               (map (lambda (thing)
                                      (if (function? thing)
                                          (apply thing args)
                                          thing))
                                    things)))))))))

;;;; Book examples

(define (make-arithmetic-1 name get-operation)
  (make-arithmetic name p3-any-object? '()
    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))
    (lambda (operator)
      (p3-simple-operation operator
                        p3-any-object?
                        (get-operation operator)))))

(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic
    (lambda (operator)
      (lambda args (cons operator args)))))
