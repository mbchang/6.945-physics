;;;; Generic arithmetic

(define (make-generic-arithmetic dispatcher)
  (make-arithmetic 'generic p3-any-object? '() ; the predict for this arithmetic is any-object, which will be the base predicate if we want to extend on generic-arithmetic
    (lambda (name)
      (p3-constant-union name))
    (lambda (operator)
      (p3-simple-operation operator
                        p3-any-object?
                        (make-generic-procedure
                          operator
                          (operator-arity operator)
                          dispatcher)))))

(define (add-to-generic-arithmetic! generic-arithmetic
                                    arithmetic)
  ;; TODO: We have a choice here: do we merge constants with
  ;; non-standard names into the generic arithmetic?  For now, we
  ;; will ignore such constants.
  (for-each
   (lambda (name)
     (let ((binding
            (arithmetic-constant-binding name
                                         generic-arithmetic))
           (element (find-arithmetic-constant name arithmetic)))
       (set-cdr! binding
                 (p3-constant-union name
                                 (cdr binding)
                                 element))))
   (arithmetic-constant-names generic-arithmetic))
  (for-each
   (lambda (operator)
     (let ((generic-procedure
            (p3-simple-operation-procedure
             (arithmetic-operation operator
                                   generic-arithmetic))))
       (for-each (lambda (operation)
                   (define-generic-procedure-handler
                       generic-procedure
                       (p3-operation-applicability operation)
                       (p3-operation-procedure operation)))
                 (p3-operation-components
                  (arithmetic-operation operator arithmetic)))))
   (arithmetic-operators arithmetic)))

(define (extend-generic-arithmetic! generic-arithmetic extension)
  (add-to-generic-arithmetic! generic-arithmetic
                              (extension generic-arithmetic)))  ;; you extend it over the entire arithmetic!