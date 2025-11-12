;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH  300)
(define HEIGHT 500)
(define INVADER-X-SPEED 1.5)
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)
(define HIT-RANGE 10)
(define INVADE-RATE 100)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TRANSPARENT (rectangle WIDTH HEIGHT 0 "black"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")           
              -5 6
              (ellipse 20 10 "solid"   "blue"))) 

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")      
                       (ellipse 30 10 "solid" "green"))   
              5 -14
              (above (rectangle 5 10 "solid" "black")      
                     (rectangle 20 10 "solid" "black"))))

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define MISSILE (ellipse 5 15 "solid" "red"))
(define RADIUSES (+ (/ (image-width MISSILE) 2) (/ (image-height INVADER) 2)))


(define-struct game (invaders missiles tank))
(define-struct tank (x dir))
(define-struct invader (x y dx))
(define-struct missile (x y))


(define (main s)
  (big-bang s                 
    (on-tick   tock)          
    (to-draw   render)        
    (stop-when invader-lands)
    (on-key    handle-key)　　 
    ))  


(define (tock s)
  (make-game (tock-loinvader (invader-collided (game-invaders s) (game-missiles s)))
             (tock-lom (missile-out (missile-collided (game-missiles s) (game-invaders s))))
             (tock-tank (game-tank s))))


(define (tock-loinvader loi)
  (cond [(empty? loi) empty]
        [else
         (cons (bounce-invader (move-invader (first loi)))
               (tock-loinvader (rest loi)))]))


(define (move-invader invader)
  (make-invader (+ (invader-x invader) (invader-dx invader))
                (- (invader-y invader) INVADER-Y-SPEED)
                (invader-dx invader)))


(define (bounce-invader invader)
  (if (or (and
           (>= (invader-x invader) (- WIDTH (/ (image-width INVADER) 2)))
           (> (invader-dx invader) 0))
          (and
           (<= (invader-x invader) (/ (image-width INVADER) 2))
           (< (invader-dx invader) 0)))
      (make-invader
       (invader-x invader)
       (invader-y invader)
       (* (invader-dx invader) -1))
      invader))


(define (tock-lom lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (tock-lom (rest lom)))]))


(define (move-missile m)
  (make-missile (missile-x m) (+ (missile-y m) MISSILE-SPEED)))


(define (tock-tank t)
  (cond [(and (<= (tank-x t) (/ (image-width TANK) 2))
              (= (tank-dir t) -1))
         (make-tank (/ (image-width TANK) 2) -1)]
        [(and (>= (tank-x t) (- WIDTH (/ (image-width TANK) 2)))
              (= (tank-dir t) 1))
         (make-tank (- WIDTH (/ (image-width TANK) 2)) 1)]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))


(define (render s)
  (overlay (render-loinvader (game-invaders s))
           (render-tank (game-tank s))
           (render-lom (game-missiles s))
           BACKGROUND))


(define (render-lom lom)
  (cond [(empty? lom) TRANSPARENT]
        [else
         (put-image MISSILE
                    (missile-x (first lom))
                    (missile-y (first lom))
                    (render-lom (rest lom)))]))


(define (render-loinvader loi)
  (cond [(empty? loi) TRANSPARENT]
        [else
         (put-image INVADER
                    (invader-x (first loi))
                    (invader-y (first loi))
                    (render-loinvader (rest loi)))]))


(define (render-tank t)
  (put-image TANK (tank-x t) TANK-HEIGHT/2 TRANSPARENT))


(define (invader-collided loi lom)
  (cond [(empty? loi) loi]
        [(empty? lom) loi]
        [else
         (invader-collided (loi-on-missile loi (first lom)) (rest lom))]))


(define (loi-on-missile loi m)
  (cond [(empty? loi) empty]
        [else
         (if (collided? (first loi) m)
             (cons (make-invader (random WIDTH) HEIGHT (random-sign INVADER-X-SPEED)) (rest loi))
             (cons (first loi) (loi-on-missile (rest loi) m)))]))


(define (missile-collided lom loi)
  (cond [(empty? lom) lom]
        [(empty? loi) lom]
        [else
         (missile-collided (lom-on-invader lom (first loi)) (rest loi))]))


(define (lom-on-invader lom i)
  (cond [(empty? lom) empty]
        [else
         (if (collided? i (first lom))
             (rest lom)
             (cons (first lom) (lom-on-invader (rest lom) i)))]))


(define (collided? i m)
  (if (<= (sqrt (+ (expt (- (missile-x m) (invader-x i)) 2)
                   (expt (- (missile-y m) (invader-y i)) 2)))
          RADIUSES)
      true
      false))


(define (random-sign n)
  (if (= (random 2) 0) 
      n              
      (- n)))


(define (missile-out lom)
  (cond [(empty? lom) empty]
        [else
         (if (>= (missile-y (first lom)) (+ HEIGHT (/ (image-height MISSILE) 2)))
             (rest lom)
             (cons  (first lom) (missile-out (rest lom))))
         ]))


(define (invader-lands loi)
  (cond [(empty? (game-invaders loi)) false]
        [else
         (if (<= (invader-y (first (game-invaders loi))) (/ (image-height INVADER) 2))
             true
             (invader-lands (make-game (rest (game-invaders loi)) (game-missiles loi) (game-tank loi))))]))


(define (handle-key g ke)
  (cond [(key=? ke "left") (change-direction g ke)]
        [(key=? ke "right") (change-direction g ke)]
        [(key=? ke " ") (shoot-missile g ke)]
        [else 
         g]))


(define (change-direction s ke)
  (make-game (game-invaders s)
             (game-missiles s)
             (change-direction--tank (game-tank s) ke)))


(define (change-direction--tank t ke)
  (if (or (and (string=? ke "left")
               (= -1 (tank-dir t)))
          (and (string=? ke "right")
               (= 1 (tank-dir t))))
      t
      (make-tank (tank-x t) (* (tank-dir t) -1))))


(define (shoot-missile s ke)
  (make-game  (game-invaders s)
              (add-missile (game-missiles s) ke (game-tank s))
              (game-tank s)))


(define (add-missile lom ke t)
  (if (string=? " " ke)
      (append (list (make-missile (tank-x t) (* TANK-HEIGHT/2 2))) lom)
      lom))


(define start (make-game (list (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)
                               (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)
                               (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)
                               (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)
                               (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED))
                         empty
                         (make-tank (/ WIDTH 2) 1)))

(main start)
