(ge (make-top-level-environment))

(load "load")

(define (reset-repl)
  (ge (make-top-level-environment)))

;(install-arithmetic! (extend-arithmetic vector-extender numeric-arithmetic))

(define (sum values)
  (reduce-left + 0 values))

;;; object type (parent type of everything)

(define object:name
  (make-property 'name))

(define object?
  (make-type 'object (list object:name)))

(define get-name
  (property-getter object:name object?))
(define set-name!
  (property-setter object:name object? string?))


;;; thing type (physical things in world)

;; every thing has these properties

(define thing:interactions
  (make-property 'interactions
                 'default-value '()))
(define thing:mass
  (make-property 'mass))
(define thing:position
  (make-property 'position))
(define thing:velocity
  (make-property 'velocity
                 'default-value 0)) 

(define thing?
  (make-type 'thing (list thing:interactions
                          thing:mass
                          thing:position
                          thing:velocity)))
(set-predicate<=! thing? object?)

(define (make-thing name mass position #!optional velocity)
  ((type-instantiator thing?)
   'name name
   'mass mass
   'position position
   'velocity velocity))

(define get-interactions
  (property-getter thing:interactions thing?))
(define set-interactions!
  (property-setter thing:interactions thing? any-object?))
(define get-mass
  (property-getter thing:mass thing?))
(define set-mass!
  (property-setter thing:mass thing? any-object?))
(define get-position
  (property-getter thing:position thing?))
(define set-position!
  (property-setter thing:position thing? any-object?))
(define get-velocity
  (property-getter thing:velocity thing?))
(define set-velocity!
  (property-setter thing:velocity thing? any-object?))

(define (update-thing thing dt)
  (let* ((net-force (sum (map (lambda (interaction)
                                ((get-interaction-procedure interaction)
                                 thing
                                 (get-interaction-influences interaction)))
                              (get-interactions thing))))
         (mass (get-mass thing))
         (a (* net-force (/ 1 mass)))
         (dv (* a dt))
         (v (+ (get-velocity thing) dv))
         (dx (* v dt))
         (x (+ (get-position thing) dx)))
    (set-velocity! thing v)
    (set-position! thing x)))

;;; ball type

(define ball:radius
  (make-property 'radius))

(define ball?
  (make-type 'ball (list ball:radius)))
(set-predicate<=! ball? thing?)

(define get-ball-radius
  (property-getter ball:radius ball?))
(define set-ball-radius!
  (property-setter ball:radius ball? any-object?))

(define (make-ball name radius mass position #!optional velocity)
  ((type-instantiator ball?)
   'name name
   'radius radius
   'mass mass
   'position position
   'velocity velocity))

(define (add-ball! ball world)
  (set-world-all-things! world (cons ball (get-world-all-things world)))
  (for-each (lambda (thing)
              (let* ((no-gravity
                      (remove (lambda (interaction)
                                (eq? (get-name interaction) 'gravity))
                              (get-interactions thing)))
                     (gravity (make-gravity thing (get-world-all-things world)))
                     (new-interactions
                      (cons gravity no-gravity)))
                (set-interactions! thing new-interactions)))
            (get-world-all-things world)))

;;; interaction type

(define interaction:procedure
  (make-property 'procedure))
(define interaction:influences
  (make-property 'influences))

(define interaction?
  (make-type 'interaction (list interaction:procedure
                                interaction:influences)))
(set-predicate<=! interaction? object?)

(define (make-interaction interaction name procedure influences)
  ((type-instantiator interaction)
   'name name
   'procedure procedure
   'influences influences))

(define get-interaction-procedure
  (property-getter interaction:procedure interaction?))
(define set-interaction-procedure!
  (property-setter interaction:procedure interaction? procedure?))
(define get-interaction-influences
  (property-getter interaction:influences interaction?))
(define set-interaction-influences!
  (property-setter interaction:influences interaction? any-object?))

;;; gravity type

(define gravity?
  (make-type 'gravity '()))
(set-predicate<=! gravity? interaction?)

(define (make-gravity thing all-things)
  (define (procedure thing influences)
    (sum (map (lambda (influence)
                (let* ((m1 (get-mass thing))
                      (m2 (get-mass influence))
                      (G 6.674e-11)
                      (v (- (get-position influence)  ; vector between influence and thing
                                       (get-position thing)))
                      (r (magnitude v)) ; distance between influence and thing
                      (u (/ v r)) ; unit vector
                      (gmag (* (* G m1 m2)  ; magnitude of gravity
                              (/ 1 (square r))))
                    )
                  (* u gmag)  ; multiply unit vector by magnitude of gravitational force
                ))
              influences)))
  (let ((influences (delq thing all-things)))
    (make-interaction gravity? 'gravity procedure influences)))

;;; world type

(define world:all-things
  (make-property 'all-things
                 'default-value '()))
(define world:timestep
  (make-property 'timestep
                 'default-value 1))

(define world?
  (make-type 'world (list world:all-things
                          world:timestep)))

(define (make-world name)
  ((type-instantiator world?)
   'name name))

(define get-world-all-things
  (property-getter world:all-things world?))
(define set-world-all-things!
  (property-setter world:all-things world? any-object?))
(define get-world-timestep
  (property-getter world:timestep world?))
(define set-world-timestep!
  (property-setter world:timestep world? any-object?))

(define (update-world world)
  (for-each (lambda (thing)
              (update-thing thing (get-world-timestep world)))
            (get-world-all-things world)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-binary-stars)
  (define w (make-world "world"))
  (define b1 (make-ball "ball1" 5 1e15 #(-100 -100) #(9 -9)))
  (define b2 (make-ball "ball2" 5 1e15 #(100 100) #(-9 9)))
  (add-ball! b1 w)
  (add-ball! b2 w)
  w
)

(define (earth-moon)
  (define w (make-world "world"))
  (define b1 (make-ball "earth" 30 1e15 #(0 0)))
  (define b2 (make-ball "moon" 5 1e5 #(100 100) #(-15.361 15.361)))
  (add-ball! b1 w)
  (add-ball! b2 w)
  w
)

(define (solar-system)
  (define w (make-world "world"))
  (define s (make-ball "sun" 30 1e15 #(0 0)))
  (define b1 (make-ball "ball1" 5 1e5 #(100 100) #(-15.361 15.361)))
  (define b2 (make-ball "ball2" 5 1e5 #(125 125) #(-15.361 15.361)))
  (define b3 (make-ball "ball3" 5 1e5 #(150 150) #(-15.361 15.361)))

  (add-ball! s w)
  (add-ball! b1 w)
  (add-ball! b2 w)
  (add-ball! b3 w)
  w
)


(define (run-engine world steps)
  ;(reset-graphics)
  (if (> steps 0)
    (begin 
      (for-each 
        (lambda (thing)
            (newline)
            (display (cons (get-name thing) (get-position thing)))
            (render thing)
        )
          (get-world-all-things world))
      (update-world world)
      (run-engine world (- steps 1)))
  )
)

(reset-graphics)

;(run-engine (earth-moon) 500)
;(run-engine (create-binary-stars) 500)
(run-engine (solar-system) 500)




