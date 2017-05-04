(ge (make-top-level-environment))
(load "load")

(((* 3
    (lambda (x) (lambda (y) (+ x y))) 
    (lambda (x) (lambda (y) (vector y x))))
  'a) 
4)
