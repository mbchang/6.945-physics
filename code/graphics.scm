(define (make-graphics)
  (make-graphics-device 'x))

(define (reset-graphics device)
  (let* ((limits ((graphics-device-coordinate-limits device)
                  (lambda x x)))
         (height (+ (second limits) 1))
         (width (+ (third limits) 1)))
    (graphics-clear device)
    (graphics-set-coordinate-limits device
                                    (- (/ width 2))
                                    (- (/ height 2))
                                    (/ width 2)
                                    (/ height 2))))

(define (draw-line device x1 y1 x2 y2)
  (graphics-draw-line device x1 y1 x2 y2))

(define (draw-circle device position radius)
  (let ((x (vector-first position))
        (y (vector-second position)))
    (graphics-operation device 'fill-circle x y radius)))

(define (render device thing)
  (graphics-operation device 'set-foreground-color (get-color thing))
  (cond ((ball? thing)
         (draw-circle device (get-position thing) (get-ball-radius thing)))
        (else
          ;; TODO MAKE OTHER OBJECTS
          (draw-circle device (get-position thing) (get-ball-radius thing))
          )))

(define (random-color)
  (let ((colors (list "blue" "red" "green")))
    (list-ref colors (random 3))))
