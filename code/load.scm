(load (list 


            "utils"                     ;from common
            "indexes"                   ;from common
	    "collections"               ;from common
            "memoizers"                 ;from common
            "predicates"                ;from common
	    "applicability"             ;from common
            "generic-procedures"        ;from common
            "pretty-printer"            ;from common
	    "operators"                 ;from common
	    "package"                   ;from common
	    "predicate-counter"         ;from common
            "trie"                      ;from common


            ;;;COMMENT THIS OUT and run temp
     ;       "generics"
     ;       "tagging"
	    ;"predicates1"
     ;       "templates"
     ;       "values"
     ;       "tags"
     ;       "functions"

	    ;"operations"    
     ;               "arith"             ;from common
     ;     "numeric-arith"           ;from common            
     ;       "vector-extender"
     ;       "generic-arith"             ;from generic-arithmetic
	    ;"standard-arith"            

          ;;;COMMENT THIS OUT and run project_run
          "p3-predicate-metadata"        ;from common
          "p3-operations"                ;from common
          "arith"             ;from common
          "numeric-arith"           ;from common
          "p3-standard-arith"            ;from arithmetic-combinators
          "p3-function-variants"         ;from arithmetic-combinators
            "p3-generic-arith"             ;from generic-arithmetic
            "p3-generics"                  ;from generic-arithmetic
          "p3-vector-arith"        ; problem




            "physics-substrate"
            ; "physics-objects"
            ; "physics-world"
	    ))


; from ps3
; operations, arith, numeric-arith, vector-extender, generic-arith, standard-arith

; arith is fine to keep, numeric-arith is fine to keep, generic-arith is fine to keep, operators is fine to keep.
; replace standard-arith, replace vector-arith, replace operations


;(define trie-full-generic-arithmetic-vector
;    (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
;            (add-to-generic-arithmetic! g numeric-arithmetic)
;            (extend-generic-arithmetic! g function-extender)
;            (extend-generic-arithmetic! g vector-extender)
;            (add-to-generic-arithmetic! g (symbolic-extender numeric-arithmetic))
;    g))

;;(define trie-full-generic-arithmetic-vector
;;    (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
;;            (add-to-generic-arithmetic! g numeric-arithmetic)
;;            ;(extend-generic-arithmetic! g function-extender)
;;            (add-to-generic-arithmetic! g (make-symbolic-arithmetic numeric-arithmetic))
;;    g))

;(install-arithmetic! trie-full-generic-arithmetic-vector)

;(((* 3
;    (lambda (x) (lambda (y) (+ x y))) 
;    (lambda (x) (lambda (y) (vector y x))))
;  'a) 
;4)

