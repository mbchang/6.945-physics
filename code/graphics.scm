(define graphics-device (make-graphics-device 'x))
(define device-height)
(define device-width)

(define (reset-graphics)
  (let* ((limits ((graphics-device-coordinate-limits graphics-device)
                  (lambda x x)))
         (height (+ (second limits) 1))
         (width (+ (third limits) 1)))
    (set! device-height height)
    (set! device-width width)
    (graphics-clear graphics-device)
    (graphics-set-coordinate-limits graphics-device
                                    (- (/ width 2))
                                    (- (/ height 2))
                                    (/ width 2)
                                    (/ height 2)))

)

(define (draw-line x1 y1 x2 y2)
  (graphics-draw-line graphics-device x1 y1 x2 y2))

(define (draw-circle position radius)
  (let ((x (vector-first position))
        (y (vector-second position)))
    (graphics-operation graphics-device 'fill-circle x y radius)))

(define (render thing)
  (cond ((ball? thing) (draw-circle (get-position thing) (get-ball-radius thing)))
        (else 

          ; TODO MAKE OTHER OBJECTS
          (draw-circle (get-position thing) (get-ball-radius thing))
        ))
)

(define (random-color)
  (let ((colors (list "blue" "red" "green"))
        (r (random 3)))
        (list-ref colors (random 3))))
