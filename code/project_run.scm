(ge (make-top-level-environment))
(load "load")

(define trie-full-generic-arithmetic-vector
    (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
            (add-to-generic-arithmetic! g numeric-arithmetic)
            (extend-generic-arithmetic! g function-extender)
            (extend-generic-arithmetic! g vector-extender)
            (add-to-generic-arithmetic! g (symbolic-extender numeric-arithmetic))
    g))

(install-arithmetic! trie-full-generic-arithmetic-vector)

(((* 3
    (lambda (x) (lambda (y) (+ x y))) 
    (lambda (x) (lambda (y) (vector y x))))
  'a) 
4)
