;;;; Operation abstraction

(define (p3-operation? object)
  (and (n:list? object)
       (n:= 4 (length object))
       (eq? 'operation (car object))
       (operator? (cadr object))
       (applicability? (caddr object))
       (procedure? (cadddr object))))

(define (p3-make-operation operator applicability procedure)
  (list 'operation operator applicability procedure))

;;; API
(define (p3-operation-applicability operation)
  (caddr operation))

;;; API
(define (p3-operation-procedure operation)
  (cadddr operation))

;;; API
(define (p3-apply-operation operation args)
  (apply (p3-operation-procedure operation) args))

;;; API
(define (p3-make-installable-operation-procedure procedure
                                              new-procedure)
  new-procedure)

;;; API
(define (p3-operation-components operation)
  (list operation))

;;; API
(define (p3-constant-union name . constants)
  (let ((unique
         (remove default-object?
                 (delete-duplicates constants eqv?))))
    (if (n:pair? unique)
        (car unique)
        (default-object))))

;;; API
(define (p3-operation-union operator . operations)
  (p3-operation-union* operator operations))

;;; API
(define (p3-operation-union* operator operations)
  (p3-make-operation operator
                  (applicability-union*
                   (map p3-operation-applicability operations))
                  (lambda args
                    (p3-operation-union-dispatch operator
                                              operations
                                              args))))

;; helper to make book description clearer
(define (p3-operation-union-dispatch operator operations args)
  (let ((operation
         (find (lambda (operation)
                 (p3-is-operation-applicable? operation args))
               operations)))
    (if (not operation)
        (error "Inapplicable operation:" operator args))
    (p3-apply-operation operation args)))

;; helper to make book description clearer
(define (p3-is-operation-applicable? operation args)
  (is-applicable? (p3-operation-applicability operation) args))

;;; API
(define (p3-simple-operation operator predicate procedure)
  (p3-make-operation operator
                  (all-args (operator-arity operator)
                            predicate)
                  procedure))

;;; API
(define (p3-simple-operation-procedure operation)
  (p3-operation-procedure operation))