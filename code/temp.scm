(ge (make-top-level-environment))

(load "load")

;(define (reset-repl)
;  (ge (make-top-level-environment)))

;(install-arithmetic! (extend-arithmetic vector-extender numeric-arithmetic))

(define pi (* 4 (atan 1 1)))

(define (sum values)
  (reduce-left + 0 values))

(define (sign n)
  (cond ((negative? n) -1)
        ((positive? n)  1)
        (else 0)))

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
(define thing:color
  (make-property 'color
                 'default-value "black")) 

(define thing?
  (make-type 'thing (list thing:interactions
                          thing:mass
                          thing:position
                          thing:velocity
                          thing:color)))
(set-predicate<=! thing? object?)

(define (make-thing name mass position velocity color)
  ((type-instantiator thing?)
   'name name
   'mass mass
   'position position
   'velocity velocity
   'color color))

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
(define get-color
  (property-getter thing:color thing?))
(define set-color!
  (property-setter thing:color thing? any-object?))

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

(define (make-ball name radius mass position velocity #!optional color)
  ((type-instantiator ball?)
   'name name
   'radius radius
   'mass mass
   'position position
   'velocity velocity
   'color color))

; add any thing with mass (is affected by gravity)

(define (add-mass! mass world)
  (set-world-all-things! world (cons mass (get-world-all-things world)))
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



;;; magnet type (for point-like magnets)

(define magnet:charge
  (make-property 'charge))

(define magnet?
  (make-type 'magnet (list magnet:charge)))
(set-predicate<=! magnet? ball?)

(define get-magnet-charge
  (property-getter magnet:charge magnet?))
(define set-magnet-charge!
  (property-setter magnet:charge magnet? any-object?))
;(define get-radius
;  (property-getter magnet:radius magnet?))
;(define set-radius!
;  (property-setter magnet:radius magnet? any-object?))

(define (make-magnet name charge mass position velocity #!optional color)
  ((type-instantiator magnet?)
   'name name
   'radius 10      ; predefined small radius (for drawing) since
		     ; assuming point-like magnets
   'charge charge
   'mass mass
   'position position
   'velocity velocity
   'color color))

(define (add-magnet! magnet world)
  (add-mass! magnet world) ; adds gravity
  ;(set-world-all-things! world (cons magnet (get-world-all-things world)))
  (set-world-all-magnets! world (cons magnet (get-world-all-magnets world)))
  (for-each (lambda (magnet)
              (let* ((no-magnetic
                      (remove (lambda (interaction)
                                (eq? (get-name interaction) 'magnetic-force))
                              (get-interactions magnet)))
                     (magnetic (make-magnetic-force magnet (get-world-all-magnets world)))
                     (new-interactions
                      (cons magnetic no-magnetic)))
                (set-interactions! magnet new-interactions)))
            (get-world-all-magnets world)))


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


;;; magnetic-force type (for two point-like magnets)

(define magnetic-force?
  (make-type 'magnetic-force '()))
(set-predicate<=! magnetic-force? interaction?)

(define (make-magnetic-force magnet all-magnets)
  (define (procedure magnet influences)
    (sum (map (lambda (influence)
                (let* ((q1 (get-magnet-charge magnet))
		       (q2 (get-magnet-charge influence))
		       (mu 1.256e-6)  ; permeability of air
		       (v (- (get-position influence)  ; vector between influence and magnet
                                       (get-position magnet)))
		       (r (magnitude v)) ; distance between influence and magnet
		       (u (* -1 (/ v r))) ; unit vector
                                          ; -1 to account for opposite
					  ; signs attract & same signs repel
		       (mmag (* (* mu q1 q2)  ; magnitude of magnetic force
                              (/ 1 (* 4 pi (square r)))))
                    )
                  (* u mmag)  ; multiply unit vector by magnitude of
			      ; force
                ))
              influences)))
  (let ((influences (delq magnet all-magnets)))
    (make-interaction magnetic-force? 'magnetic-force procedure influences)))


;;; global force type (user input)

(define global?
  (make-type 'global '()))
(set-predicate<=! global? interaction?)

(define (make-global force)
  (define (procedure thing influences)
    ;(lambda ()
      force);)
  (make-interaction global? 'global procedure '()))

(define (add-global! force world)
  (for-each (lambda (thing)
              (let* ((global (make-global force))
                     (new-interactions
                      (cons global (get-interactions thing))))
                (set-interactions! thing new-interactions)))
            (get-world-all-things world)))

		 
;;; global gravity

(define global-gravity?
  (make-type 'global-gravity '()))
(set-predicate<=! global-gravity? global?)

(define (make-global-gravity)
  (define (procedure thing influences)
    (* 9.807 (get-mass thing) #(0 -1)))
  (make-interaction global-gravity? 'global-gravity procedure '()))

(define (add-global-gravity! world)
  (for-each (lambda (thing)
              (let* ((global-gravity (make-global-gravity))
                     (new-interactions
                      (cons global-gravity (get-interactions thing))))
                (set-interactions! thing new-interactions)))
            (get-world-all-things world)))

;;; world type

(define world:all-things
  (make-property 'all-things
                 'default-value '()))
(define world:all-magnets
  (make-property 'all-magnets
                 'default-value '()))
(define world:timestep
  (make-property 'timestep
                 'default-value 0.5))

(define world?
  (make-type 'world (list world:all-things
			  world:all-magnets
                          world:timestep)))

(define (make-world name)
  ((type-instantiator world?)
   'name name))

(define get-world-all-things
  (property-getter world:all-things world?))
(define set-world-all-things!
  (property-setter world:all-things world? any-object?))
(define get-world-all-magnets
  (property-getter world:all-magnets world?))
(define set-world-all-magnets!
  (property-setter world:all-magnets world? any-object?))
(define get-world-timestep
  (property-getter world:timestep world?))
(define set-world-timestep!
  (property-setter world:timestep world? any-object?))

(define (update-world world)
  (for-each (lambda (thing)
              (update-thing thing (get-world-timestep world)))
            (get-world-all-things world)))

#|

(define w (make-world "world"))
(define b1 (make-ball "ball1" 10 1 #(0 0)))
;(define b2 (make-ball "ball2" 10 1000000000000 #(10 10)))
(add-mass! b1 w)
;(add-mass! b2 w)

(define m1 (make-magnet "magnet1" 20 1 #(1 1)))
(define m2 (make-magnet "magnet2" 20 1 #(10 10)))
(add-magnet! m1 w) 

(add-magnet! m2 w)

;(get-interactions b1)
(get-interactions m1)
;(eq? b1 (car (get-world-all-things w)))

(get-position b1)
;(get-position b2)
(get-position m1)
(get-position m2)

(update-world w)

;(get-position b1)
;(get-position b2)
(get-position m1)
(get-position m2)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-binary-stars)
  (define w (make-world "world"))
  (define b1 (make-ball "ball1" 5 1e15 #(-100 -100) #(9 -9) "red"))
  (define b2 (make-ball "ball2" 5 1e15 #(100 100) #(-9 9) "green"))

  (add-mass! b1 w)
  (add-mass! b2 w)
  w
)

(define (earth-moon)
  (define w (make-world "world"))
  (define b1 (make-ball "earth" 30 1e15 #(0 0) #(0 0) "blue"))
  (define b2 (make-ball "moon" 5 1e5 #(100 100) #(-15.361 15.361) "#85929E"))

  (add-mass! b1 w)
  (add-mass! b2 w)
  w
)

(define (solar-system)
  (define w (make-world "world"))
  (define s (make-ball "sun" 30 1e15 #(0 0) #(0 0) "yellow"))
  (define b1 (make-ball "ball1" 5 1e5 #(100 100) #(-15.361 15.361) "blue"))
  (define b2 (make-ball "ball2" 5 1e5 #(110 110) #(-15.361 15.361) "red"))
  (define b3 (make-ball "ball3" 5 1e5 #(120 120) #(-15.361 15.361) "green"))
  (define b4 (make-ball "ball4" 5 1e5 #(130 130) #(-15.361 15.361) "purple"))
  (define b5 (make-ball "ball5" 5 1e5 #(140 140) #(-15.361 15.361) "orange"))
  (define b6 (make-ball "ball6" 5 1e5 #(150 150) #(-15.361 15.361) "gray"))


  (add-mass! s w)
  (add-mass! b1 w)
  (add-mass! b2 w)
  (add-mass! b3 w)
  (add-mass! b4 w)
  (add-mass! b5 w)
  (add-mass! b6 w)
  w
)

(define (magnets-1)
  (define w (make-world "world"))
  (define m1 (make-magnet "magnet1" 1e5 1 #(-30 -30) #(0 0) "red"))
  (define m2 (make-magnet "magnet2" 1e5 1 #(30 30) #(0 0) "red"))

  (add-magnet! m1 w)
  (add-magnet! m2 w)
  w
)

(define (magnets-2)
  (define w (make-world "world"))
  (define m1 (make-magnet "magnet1" 5e5 1 #(-100 -100) #(0 0) "red"))
  (define m2 (make-magnet "magnet2" -5e5 1 #(100 100) #(0 0) "blue"))
  (define m3 (make-magnet "magnet3" 5e5 1 #(-100 100) #(0 0) "red"))
  (define m4 (make-magnet "magnet4" -5e5 1 #(100 -100) #(0 0) "blue"))

  (add-magnet! m1 w)
  (add-magnet! m2 w)
  (add-magnet! m3 w)
  (add-magnet! m4 w)
  w
)

(define (magnetic-solar-system)
  (define w (make-world "world"))
  (define s (make-magnet "sun" 5e5 1e15 #(0 0) #(0 0) "blue"))
  (define b1 (make-magnet "ball1" -2e7 1e5 #(100 100) #(-15.361 15.361) "red"))
  (define b2 (make-magnet "ball2" -2e7 1e5 #(110 110) #(-15.361 15.361) "red"))
  (define b3 (make-magnet "ball3" -2e7 1e5 #(120 120) #(-15.361 15.361) "red"))
  (define b4 (make-magnet "ball4" -2e7 1e5 #(130 130) #(-15.361 15.361) "red"))
  (define b5 (make-magnet "ball5" -2e7 1e5 #(140 140) #(-15.361 15.361) "red"))
  (define b6 (make-magnet "ball6" -2e7 1e5 #(150 150) #(-15.361 15.361) "red"))

  (add-magnet! s w)
  (add-magnet! b1 w)
  (add-magnet! b2 w)
  (add-magnet! b3 w)
  (add-magnet! b4 w)
  (add-magnet! b5 w)
  (add-magnet! b6 w)
  w
)

(define (g-gravity)
  (define w (make-world "world"))
  (define b (make-ball "ball" 30 1 #(0 200) #(0 0) "black"))

  (add-mass! b w)
  (add-global-gravity! w)
  w
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running the engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-engine world steps)
  (reset-graphics)
  (if (> steps 0)
    (begin 
      (for-each 
        (lambda (thing)
            (newline)
            (display (cons (get-name thing) (get-position thing)))
            (render thing))
          (get-world-all-things world))
      (update-world world)
      (run-engine world (- steps 1)))))

;(reset-graphics)

;(run-engine (earth-moon) 500)
;(run-engine (create-binary-stars) 500)
;(run-engine (solar-system) 100)
;(run-engine (magnets-1) 100)
;(run-engine (magnets-2) 100)
;(run-engine (magnetic-solar-system) 300)
;(run-engine (g-gravity) 100)

(graphics-close graphics-device)

