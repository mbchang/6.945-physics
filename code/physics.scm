(load "load")

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

(define (evaluate-update-thing thing dt)
  (let* ((net-force (sum (map (lambda (interaction)
                                ((get-interaction-procedure interaction)
                                 thing
                                 (get-interaction-influences interaction)))
                              (get-interactions thing))))
         (mass (get-mass thing))
         (a (/ net-force mass))
         (dv (* a dt))
         (v (+ (get-velocity thing) dv))
         (dx (* v dt))
         (x (+ (get-position thing) dx)))
    (list thing x v)))

(define (apply-update-thing update)
  (let ((thing (first update))
        (x (second update))
        (v (third update)))
    (set-velocity! thing v)
    (set-position! thing x)))

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

;;; box type

(define box:length
  (make-property 'length))

(define box:height
  (make-property 'height))

(define box?
  (make-type 'box (list box:length
			box:height)))
(set-predicate<=! box? thing?)

(define get-box-length
  (property-getter box:length box?))
(define set-box-length!
  (property-setter box:length box? any-object?))
(define get-box-height
  (property-getter box:height box?))
(define set-box-height!
  (property-setter box:height box? any-object?))

(define (make-box name length height mass position velocity color)
  ((type-instantiator box?)
   'name name
   'length length
   'height height
   'mass mass
   'position position
   'velocity velocity
   'color color))

;;; point-charge type

(define point-charge:charge
  (make-property 'charge))

(define point-charge?
  (make-type 'point-charge (list point-charge:charge)))
(set-predicate<=! point-charge? ball?)

(define get-point-charge-charge
  (property-getter point-charge:charge point-charge?))
(define set-point-charge-charge!
  (property-setter point-charge:charge point-charge? any-object?))
;(define get-radius
;  (property-getter point-charge:radius point-charge?))
;(define set-radius!
;  (property-setter point-charge:radius point-charge? any-object?))

(define (make-point-charge name charge mass position velocity #!optional color)
  ((type-instantiator point-charge?)
   'name name
   'radius 10      ; predefined small radius (for drawing) since
                   ; assuming point charges
   'charge charge
   'mass mass
   'position position
   'velocity velocity
   'color color))

(define (add-point-charge! point-charge world)
  (add-mass! point-charge world) ; adds gravity
  ;; (set-world-all-things!
  ;;  world
  ;;  (cons point-charge (get-world-all-things world)))
  (set-world-all-point-charges!
   world
   (cons point-charge (get-world-all-point-charges world)))
  (for-each (lambda (point-charge)
              (let* ((no-charges
                      (remove (lambda (interaction)
                                (eq? (get-name interaction) 'electric-force))
                              (get-interactions point-charge)))
                     (electric (make-electric-force
                                point-charge
                                (get-world-all-point-charges world)))
                     (new-interactions
                      (cons electric no-charges)))
                (set-interactions! point-charge new-interactions)))
            (get-world-all-point-charges world)))

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
                       ;; vector between influence and thing
                       (v (- (get-position influence)
                             (get-position thing)))
                       (r (magnitude v)) ; distance between influence and thing
                       (u (/ v r)) ; unit vector
                       (gmag (/ (* G m1 m2) ; magnitude of gravity
                                (square r))))
                  ;; multiply unit vector by magnitude of gravitational force
                  (* u gmag)))
              influences)))
  (let ((influences (delq thing all-things)))
    (make-interaction gravity? 'gravity procedure influences)))

;;; electric-force type (for two point charges)

(define electric-force?
  (make-type 'electric-force '()))
(set-predicate<=! electric-force? interaction?)

(define (make-electric-force point-charge all-point-charges)
  (define (procedure point-charge influences)
    (sum (map (lambda (influence)
                (let* ((q1 (get-point-charge-charge point-charge))
            		       (q2 (get-point-charge-charge influence))
            		       (epsilon0 8.854e-12) ; vacuum permittivity
                       ;; vector between influence and point charge
            		       (v (- (get-position influence)
                             (get-position point-charge)))
                       ;; distance between influence and point charge
            		       (r (magnitude v))
                       ;; unit vector * -1 to account for opposite signs
                       ;; attract & same signs repel
            		       (u (* -1 (/ v r)))
            		       (melec (/ (* q1 q2)  ; magnitude of electric force
                                 (* 4 pi epsilon0 (square r)))))
                  (* u melec))) ; multiply unit vector by magnitude of force
              influences)))
  (let ((influences (delq point-charge all-point-charges)))
    (make-interaction electric-force? 'electric-force procedure influences)))

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
  ;; (set-world-all-global-forces!
  ;;  world
  ;;  (cons (make-global force) (get-world-all-global-forces world)))
  (for-each (lambda (thing)
              (let* ((global (make-global force))
                     (new-interactions
                      (cons global (get-interactions thing))))
                (set-interactions! thing new-interactions)))
            (get-world-all-things world)))

; TODO: how to "inform" things added later of global forces applied before

;;; global gravity

(define global-gravity?
  (make-type 'global-gravity '()))
(set-predicate<=! global-gravity? global?)

(define (make-global-gravity)
  (define (procedure thing influences)
    (* 9.807 (get-mass thing) #(0 -1)))
  (make-interaction global-gravity? 'global-gravity procedure '()))

(define (add-global-gravity! world)
  ;; (set-world-all-global-forces!
  ;;  world
  ;;  (cons (make-global-gravity) (get-world-all-global-forces world)))
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
(define world:all-point-charges
  (make-property 'all-point-charges
                 'default-value '()))
(define world:all-global-forces
  (make-property 'all-global-forces
                 'default-value '()))
(define world:timestep
  (make-property 'timestep
                 'default-value 0.5))

(define world?
  (make-type 'world (list world:all-things
                          world:all-point-charges
                          world:all-global-forces
                          world:timestep)))

(define (make-world name)
  ((type-instantiator world?)
   'name name))

(define get-world-all-things
  (property-getter world:all-things world?))
(define set-world-all-things!
  (property-setter world:all-things world? any-object?))
(define get-world-all-point-charges
  (property-getter world:all-point-charges world?))
(define set-world-all-point-charges!
  (property-setter world:all-point-charges world? any-object?))
(define get-world-all-global-forces
  (property-getter world:all-global-forces world?))
(define set-world-all-global-forces!
  (property-setter world:all-global-forces world? any-object?))
(define get-world-timestep
  (property-getter world:timestep world?))
(define set-world-timestep!
  (property-setter world:timestep world? any-object?))

(define (evaluate-update-world world)
  (map (lambda (thing)
              (evaluate-update-thing thing (get-world-timestep world)))
            (get-world-all-things world)))

(define (update-world world)
  (for-each (lambda (update)
              (apply-update-thing update))
            (evaluate-update-world world)))

#|

(define w (make-world "world"))
(define b1 (make-ball "ball1" 5 1 #(100 100) #(-15.361 15.361) "blue"))
;(define b2 (make-ball "ball2" 10 1000000000000 #(10 10)))
(add-mass! b1 w)
;(add-mass! b2 w)

(define m1 (make-point-charge "charge1" 20 1 #(1 1)))
(define m2 (make-point-charge "charge2" 20 1 #(10 10)))
(add-point-charge! m1 w)

(add-point-charge! m2 w)

(get-interactions b1)
;(get-interactions m1)
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

(define (binary-stars)
  (define w (make-world "world"))
  (define b1 (make-ball "ball1" 30 1e15 #(-90 -90) #(9 -9) "red"))
  (define b2 (make-ball "ball2" 30 1e15 #(90 90) #(-9 9) "green"))

  (add-mass! b1 w)
  (add-mass! b2 w)
  w)

(define (earth-moon)
  (define w (make-world "world"))
  (define b1 (make-ball "earth" 30 1e15 #(0 0) #(0 0) "blue"))
  (define b2 (make-ball "moon" 5 1e5 #(100 100) #(-15.361 15.361) "#85929E"))

  (add-mass! b1 w)
  (add-mass! b2 w)
  w)

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
  w)

(define (charges-1)
  (define w (make-world "world"))
  (define m1 (make-point-charge "charge1" 1e-3 1 #(-30 -30) #(0 0) "red"))
  (define m2 (make-point-charge "charge2" 1e-3 1 #(30 30) #(0 0) "red"))

  (add-point-charge! m1 w)
  (add-point-charge! m2 w)
  w)

(define (charges-2)
  (define w (make-world "world"))
  (define m1 (make-point-charge "charge1" 1e-3 1 #(-100 -100) #(0 0) "red"))
  (define m2 (make-point-charge "charge2" -1e-3 1 #(100 100) #(0 0) "blue"))
  (define m3 (make-point-charge "charge3" 1e-3 1 #(-100 100) #(0 0) "red"))
  (define m4 (make-point-charge "charge4" -1e-3 1 #(100 -100) #(0 0) "blue"))

  (add-point-charge! m1 w)
  (add-point-charge! m2 w)
  (add-point-charge! m3 w)
  (add-point-charge! m4 w)
  w)

(define (charged-solar-system)
  (define w (make-world "world"))
  (define s (make-point-charge "sun" 3e-3 1e15 #(0 0) #(0 0) "blue"))
  (define b1 (make-point-charge "ball1" -1e-1 1e5 #(100 100) #(-10.361 10.361) "red"))
  (define b2 (make-point-charge "ball2" -1e-1 1e5 #(110 110) #(-10.361 10.361) "red"))
  (define b3 (make-point-charge "ball3" -1e-1 1e5 #(120 120) #(-10.361 10.361) "red"))
  (define b4 (make-point-charge "ball4" -1e-1 1e5 #(130 130) #(-10.361 10.361) "red"))
  (define b5 (make-point-charge "ball5" -1e-1 1e5 #(140 140) #(-10.361 10.361) "red"))
  (define b6 (make-point-charge "ball6" -1e-1 1e5 #(150 150) #(-10.361 10.361) "red"))

  (add-point-charge! s w)
  (add-point-charge! b1 w)
  (add-point-charge! b2 w)
  (add-point-charge! b3 w)
  (add-point-charge! b4 w)
  (add-point-charge! b5 w)
  (add-point-charge! b6 w)
  w)

(define (charged-solar-system-color)
  (define w (make-world "world"))
  (define s (make-point-charge "sun" 3e-3 1e15 #(0 0) #(0 0) "blue"))
  (define b1 (make-point-charge "ball1" -1e-1 1e5 #(100 100) #(-10.361 10.361) "red"))
  (define b2 (make-point-charge "ball2" -1e-1 1e5 #(110 110) #(-10.361 10.361) "yellow"))
  (define b3 (make-point-charge "ball3" -1e-1 1e5 #(120 120) #(-10.361 10.361) "green"))
  (define b4 (make-point-charge "ball4" -1e-1 1e5 #(130 130) #(-10.361 10.361) "purple"))
  (define b5 (make-point-charge "ball5" -1e-1 1e5 #(140 140) #(-10.361 10.361) "orange"))
  (define b6 (make-point-charge "ball6" -1e-1 1e5 #(150 150) #(-10.361 10.361) "gray"))

  (add-point-charge! s w)
  (add-point-charge! b1 w)
  (add-point-charge! b2 w)
  (add-point-charge! b3 w)
  (add-point-charge! b4 w)
  (add-point-charge! b5 w)
  (add-point-charge! b6 w)
  w)

(define (g-gravity)
  (define w (make-world "world"))
  (define b (make-ball "ball" 30 1 #(0 200) #(0 0) "black"))

  (add-mass! b w)
  (add-global-gravity! w)
  w)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running the engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define graphics-device (make-graphics))

(define (run-engine world steps)
  (reset-graphics graphics-device)
  (if (> steps 0)
      (begin
        (for-each (lambda (thing)
                    ;(newline)
                    ;(display (cons (get-name thing) (get-position thing)))
                    (render graphics-device thing))
                  (get-world-all-things world))
        (update-world world)
        (run-engine world (- steps 1)))))

;(reset-graphics graphics-device)


(run-engine (earth-moon) 500)
;(run-engine (binary-stars) 500)
;(run-engine (solar-system) 500)
;(run-engine (charges-1) 100)
;(run-engine (charges-2) 100)
;(run-engine (charged-solar-system) 10000)
;(run-engine (charged-solar-system-color) 10000)
;(run-engine (g-gravity) 100)

(graphics-close graphics-device)
