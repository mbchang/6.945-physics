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

          "p3-predicate-metadata"        ;from common
          "p3-operations"                ;from common
          "arith"             ;from common
          "numeric-arith"           ;from common
          "p3-standard-arith"            ;from arithmetic-combinators
          "p3-function-variants"         ;from arithmetic-combinators
            "p3-generic-arith"             ;from generic-arithmetic
            "p3-generics"                  ;from generic-arithmetic
          "p3-vector-arith"        ; problem
))

(define trie-full-generic-arithmetic-vector
    (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
            (add-to-generic-arithmetic! g numeric-arithmetic)
            (extend-generic-arithmetic! g function-extender)
            (extend-generic-arithmetic! g vector-extender)
            (add-to-generic-arithmetic! g (symbolic-extender numeric-arithmetic))
    g))

(install-arithmetic! trie-full-generic-arithmetic-vector)