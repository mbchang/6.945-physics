(load (list "utils"                     ;from common
            "collections"               ;from common
            "memoizers"                 ;from common
            "predicates"                ;from common
            "predicate-metadata"        ;from common
            "applicability"             ;from common
            "generic-procedures"        ;from common
            "operators"                 ;from common
            "operations"                ;from common
            "package"                   ;from common
            "predicate-counter"         ;from common
            "trie"                      ;from common
            "arith"			                ;from common
            "numeric-arith"		          ;from common
            "standard-arith"            ;from arithmetic-combinators
            "function-variants"         ;from arithmetic-combinators
            "generic-arith"             ;from generic-arithmetic
            "generics"                  ;from generic-arithmetic
            "vector-arith"
            ))

(define trie-full-generic-arithmetic-vector
  (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (extend-generic-arithmetic! g vector-extender)
    (add-to-generic-arithmetic! g (symbolic-extender numeric-arithmetic))
    g))

(install-arithmetic! trie-full-generic-arithmetic-vector)
