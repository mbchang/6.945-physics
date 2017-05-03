;(set! user-initial-environment (make-top-level-environment))
;(environment-define user-initial-environment 
;                    'generic-evaluation-environment
;                    (extend-top-level-environment user-initial-environment))
;(define generic-evaluation-environment 
;  (access generic-evaluation-environment user-initial-environment))

(define arith-environment (make-top-level-environment))

;(load (list 
;          "p3-predicate-metadata"        ;from common
;          "p3-operations"                ;from common
;          "arith"             ;from common
;          "numeric-arith"           ;from common
;          "p3-standard-arith"            ;from arithmetic-combinators
;          "p3-function-variants"         ;from arithmetic-combinators
;            "p3-generic-arith"             ;from generic-arithmetic
;            "p3-generics"                  ;from generic-arithmetic
;          "p3-vector-arith"        ; problem
;) 
;arith-environment)

(load "load-arith" arith-environment)
(define * (access * arith-environment))
(define + (access + arith-environment))
(define - (access - arith-environment))
(define / (access / arith-environment))
(define < (access < arith-environment))
(define <= (access <= arith-environment))
(define = (access = arith-environment))
(define > (access > arith-environment))
(define >= (access >= arith-environment))
(define abs (access abs arith-environment))
(define acos (access acos arith-environment))
(define angle (access angle arith-environment))
(define asin (access asin arith-environment))
(define atan (access atan arith-environment))
(define ceiling (access ceiling arith-environment))
(define cos (access cos arith-environment))
(define exp (access exp arith-environment))
(define expt (access expt arith-environment))
(define floor (access floor arith-environment))
(define imag-part (access imag-part arith-environment))
(define log (access log arith-environment))
(define magnitude (access magnitude arith-environment))
(define make-polar (access make-polar arith-environment))
(define make-rectangular (access make-rectangular arith-environment))
(define max (access max arith-environment))
(define min (access min arith-environment))
(define negative? (access negative? arith-environment))
(define positive? (access positive? arith-environment))
(define real-part (access real-part arith-environment))
(define remainder (access remainder arith-environment))
(define round (access round arith-environment))
(define sin (access sin arith-environment))
(define sqrt (access sqrt arith-environment))
(define square (access square arith-environment))
(define tan (access tan arith-environment))
(define truncate (access truncate arith-environment))
(define zero? (access zero? arith-environment))
(define boolean? (access boolean? arith-environment))
(define complex? (access complex? arith-environment))
(define exact-integer? (access exact-integer? arith-environment))
(define exact-nonnegative-integer? (access exact-nonnegative-integer? arith-environment))
(define exact-positive-integer? (access exact-positive-integer? arith-environment))
(define exact-rational? (access exact-rational? arith-environment))
(define integer? (access integer? arith-environment))
(define list? (access list? arith-environment))
(define null? (access null? arith-environment))
(define number? (access number? arith-environment))
(define pair? (access pair? arith-environment))
(define procedure? (access procedure? arith-environment))
(define rational? (access rational? arith-environment))
(define real? (access real? arith-environment))
(define string? (access string? arith-environment))
(define symbol? (access symbol? arith-environment))
(define vector? (access vector? arith-environment))




;(load (list 
;            "utils"                     ;from common
;            "indexes"                   ;from common
;          "collections"               ;from common
;            "memoizers"                 ;from common
;            "predicates"                ;from common
;          "applicability"             ;from common
;            "generic-procedures"        ;from common
;            "pretty-printer"            ;from common
;          "operators"                 ;from common
;          "package"                   ;from common
;          "predicate-counter"         ;from common
;            "trie"                      ;from common


;          ;"p3-predicate-metadata"        ;from common
;          ;"p3-operations"                ;from common
;          ;"arith"             ;from common
;          ;"numeric-arith"           ;from common
;          ;"p3-standard-arith"            ;from arithmetic-combinators
;          ;"p3-function-variants"         ;from arithmetic-combinators
;          ;  "p3-generic-arith"             ;from generic-arithmetic
;          ;  "p3-generics"                  ;from generic-arithmetic
;          ;"p3-vector-arith"        ; problem


;          )

;user-initial-environment
;)


;(load (list 
;            "generics"
;            "tagging"
;          "predicates1"
;            "templates"
;            "values"
;            "tags"
;            "functions"
;          "operations"  

;          "physics-substrate"
;	    )

;generic-evaluation-environment 
;)

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
            "generics"
            "tagging"
          "predicates1"
            "templates"
            "values"
            "tags"
            "functions"
          "operations"    
                    ;"arith"             ;from common
          ;"numeric-arith"           ;from common            
            ;"vector-extender"
          ;  "generic-arith"             ;from generic-arithmetic
          ;"standard-arith"            

          ;;;COMMENT THIS OUT and run project_run
          ;"p3-predicate-metadata"        ;from common
          ;"p3-operations"                ;from common
          ;"arith"             ;from common
          ;"numeric-arith"           ;from common
          ;"p3-standard-arith"            ;from arithmetic-combinators
          ;"p3-function-variants"         ;from arithmetic-combinators
          ;  "p3-generic-arith"             ;from generic-arithmetic
          ;  "p3-generics"                  ;from generic-arithmetic
          ;"p3-vector-arith"        ; problem




            "physics-substrate"
            ; "physics-objects"
            ; "physics-world"
          )

;user-initial-environment
)



;(ge generic-evaluation-environment)

