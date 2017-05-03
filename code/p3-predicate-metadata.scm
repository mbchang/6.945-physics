;;;; Simple predicate metadata

(define (p3-register-predicate! predicate name)
  (set-predicate-metadata! predicate name)
  predicate)

(define (p3-register-compound-predicate! predicate type components)
  (p3-register-predicate! predicate
                       (cons type
                             (map p3-predicate-name components))))

(define p3-predicate-name get-predicate-metadata)


;;;(define any-object? (conjoin))

(define (p3-any-object? object) #t)
(p3-register-predicate! p3-any-object? 'any-object)

(p3-register-predicate! number? 'number)
(p3-register-predicate! symbol? 'symbol)
(p3-register-predicate! boolean? 'boolean)
