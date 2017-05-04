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
(set-predicate<=! magnet? thing?)

(define get-magnet-charge
  (property-getter magnet:charge magnet?))
(define set-magnet-charge!
  (property-setter magnet:charge magnet? any-object?))

(define (make-magnet name charge mass position #!optional velocity)
  ((type-instantiator magnet?)
   'name name
   'radius 0.01      ; predefined small radius (for drawing) since
		     ; assuming point-like magnets; no getter/setter
		     ; for magnet radius
   'charge charge
   'mass mass
   'position position
   'velocity velocity))

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
		      (sgn (cond ((eq? (+ (sign q1) (sign q2)) 0) -1) ; if same charge, -1 (repel);
				  (else 1))) ; opposite charge, 1 (attract)
                      (u (* sgn (/ v r))) ; unit vector
                      (mmag (* (* mu q1 q2)  ; magnitude of magnetic force
                              (/ 1 (* 4 pi (square r)))))
                    )
                  (* u mmag)  ; multiply unit vector by magnitude of force
                ))
              influences)))
  (let ((influences (delq magnet all-magnets)))
    (make-interaction magnetic-force? 'magnetic-force procedure influences)))


;;; world type

(define world:all-things
  (make-property 'all-things
                 'default-value '()))
(define world:all-magnets
  (make-property 'all-magnets
                 'default-value '()))
(define world:timestep
  (make-property 'timestep
                 'default-value 1))

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


(define w (make-world "world"))
;(define b1 (make-ball "ball1" 10 1000000000000 #(0 0)))
;(define b2 (make-ball "ball2" 10 1000000000000 #(10 10)))
;(add-mass! b1 w)
;(add-mass! b2 w)
(define m1 (make-magnet "magnet1" -2 1000000000000 #(1 1)))
(define m2 (make-magnet "magnet2" 2 1000000000000 #(10 10)))
(add-magnet! m1 w)
(add-magnet! m2 w)

;(get-interactions b1)
(get-interactions m1)
;(eq? b1 (car (get-world-all-things w)))

;(get-position b1)
;(get-position b2)
(get-position m1)
(get-position m2)

(update-world w)

;(get-position b1)
;(get-position b2)
(get-position m1)
(get-position m2)

