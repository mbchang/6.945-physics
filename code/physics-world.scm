;;;; physics

;;; Set up

(define the-clock)
(define all-objects)
(define all-interactions)
(define world)

; global constants
(define tickrate 0.001)
(define world-width 100)  ; width in pixels
(define world-height 100) ; height in pixels
(define max-velocity 10)

(define (start-physics-engine)
  (set! the-clock (make-clock)) ; sets the global tick rate
  (set! world (create-world))
  (create-all-objects) ; creates all objects
  (set! all-interactions (create-all-interactions)) ; creates all interactions
  (debugging-info)  ; prints state of the world
)

(define (get-all-objects) all-objects)
(define (get-all-interactions) all-interactions)
(define (get-clock) the-clock)

(define (create-all-objects)
  (add-object (create-ball 10 (vector 30 30) (vector 1 1) '1) all-objects)
  (add-object (create-ball 10 (vector 70 70) (vector -1 -1) '1) all-objects)
  all-objects
)

(define (add-object object-maker object-list)
  (set! object-list (cons (object-maker) object-list))) ; prepend the result of object-maker into all-objects

(define (create-ball radius position velocity name)
  (make-thing 'radius radius
              'position position
              'velocity velocity
              'name name)
)

(define (create-all-interactions)

)



;;;; TO BE CONTINUED;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;




(define (create-mit)
  (let ((great-dome (create-place 'great-dome))
        (little-dome (create-place 'little-dome))
        (lobby-10 (create-place 'lobby-10))
        (10-250 (create-place '10-250))
        (barker-library (create-place 'barker-library))
        (lobby-7 (create-place 'lobby-7))
        (infinite (create-place 'infinite-corridor))

        (bldg-26 (create-place 'bldg-26))
        (cp32 (create-place 'bldg-32-cp-hq))
        (tunnel (create-place 'lab-supplies))

        (32-123 (create-place '32-123))
        (32G (create-place 'gates-tower))
        (32D (create-place 'dreyfoos-tower))
        (student-street (create-place 'student-street))
        (great-court (create-place 'great-court))
        (bldg-54 (create-place 'green-building))
        (the-dot (create-place 'the-dot))
        (dorm-row (create-place 'dorm-row)))

    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways barker-library 'up 'down great-dome)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east dorm-row)
    (can-go-both-ways lobby-7 'up 'down little-dome)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways lobby-10 'east 'west infinite)
    (can-go-both-ways infinite 'north 'south bldg-26)
    (can-go-both-ways infinite 'east 'west bldg-54)
    (can-go-both-ways bldg-26 'east 'west student-street)
    (can-go-both-ways student-street 'down 'up cp32)
    (can-go-both-ways cp32 'south 'north tunnel)
    (can-go-both-ways tunnel 'up 'down bldg-54)
    (can-go-both-ways bldg-54 'south 'north the-dot)
    (can-go-both-ways the-dot 'west 'east great-court)
    (can-go-both-ways student-street 'in 'out 32-123)
    (can-go-both-ways student-street 'up 'down 32G)
    (can-go-both-ways student-street 'skew 'down 32D)

    ; Add line-of-sight into the mix
    (can-see bldg-54 32G)
    (can-see bldg-54 32D)
    (can-see bldg-54 great-dome)
    (can-see bldg-54 little-dome)
    (can-see bldg-54 great-court)
    (can-see bldg-54 the-dot)
    (can-see lobby-10 great-court)
    (can-see great-dome great-court)
    (can-see-both-ways 32D 32G)
    (can-see-both-ways great-dome little-dome)
    (can-see-both-ways lobby-10 infinite)
    (can-see-both-ways infinite bldg-26)
    (can-see-both-ways lobby-10 lobby-7)

    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-thing 'calder-sculpture the-dot)
    (create-mobile-thing 'problem-set 32-123)
    (create-mobile-thing 'recitation-problem 32-123)
    (create-mobile-thing 'sicp student-street)
    (create-mobile-thing 'engineering-book barker-library)

    (list great-dome little-dome lobby-10
          10-250 barker-library lobby-7
          infinite bldg-26 cp32
          tunnel 32-123 32D 32G
          student-street bldg-54 the-dot
          dorm-row)))

(define (create-people places)
  (append (create-students places)
          (create-house-masters places)
          (create-trolls places)))

(define (create-students places)
  (map (lambda (name)
         (create-student name
                         (random-choice places)
                         (random-bias 5)
                         (random-bias 5)))
       '(ben-bitdiddle alyssa-hacker course-6-frosh lambda-man)))

(define (create-house-masters places)
  (map (lambda (name)
         (create-house-master name
                              (random-choice places)
                              (random-bias 3)
                              (random-bias 3)))
       '(dr-evil mr-bigglesworth)))

(define (create-trolls places)
  (map (lambda (name)
         (create-troll name
                       (random-choice places)
                       (random-bias 3)
                       (random-bias 3)))
       '(grendel registrar)))

(define (create-thing name location)
  (make-thing 'name name
              'location location))

(define (create-mobile-thing name location)
  (make-mobile-thing 'name name
                     'location location))

(define (create-place name)
  (make-place 'name name))

(define (create-exit from direction to)
  (make-exit 'name 'exit
             'from from
             'direction direction
             'to to))

(define (create-student name home restlessness acquisitiveness)
  (make-student 'name name
                'location home
                'restlessness restlessness
                'acquisitiveness acquisitiveness))

(define (create-house-master name home restlessness irritability)
  (make-house-master 'name name
                     'location home
                     'restlessness restlessness
                     'acquisitiveness 1/10
                     'irritability irritability))

(define (create-troll name home restlessness hunger)
  (make-troll 'name name
              'location home
              'restlessness restlessness
              'acquisitiveness 1/10
              'hunger hunger))

(define (create-avatar name home)
  (make-avatar 'name name
               'location home
               'screen (make-screen 'name 'the-screen)))

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

(define (can-see a b)
  (add-vista! a b))

(define (can-see-both-ways a b)
  (can-see a b)
  (can-see b a))
