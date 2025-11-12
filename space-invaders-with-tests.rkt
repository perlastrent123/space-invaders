;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-with-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TRANSPARENT (rectangle WIDTH HEIGHT 0 "black"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define RADIUSES (+ (/ (image-width MISSILE) 2) (/ (image-height INVADER) 2)))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define IR (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED))


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define start (make-game (list (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)
                               (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)
                               (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)
                               (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)
                               (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED))
                         empty
                         T0))


;; Functions:

;; Game -> Game
;; start the world with ...

(define (main s)
  (big-bang s                 ; Game
    (on-tick   tock)          ; Game -> WS
    (to-draw   render)        ; Game -> Image
    (stop-when invader-lands) ; Game -> Boolean
    (on-key    handle-key)　　 ; Game KeyEvent -> WS
    ))  

;; Game -> Game
;; updates the state of Game

; (define (tock s) s)

; <Game template>
(define (tock s)
  (make-game (tock-loinvader (invader-collided (game-invaders s) (game-missiles s)))
             (tock-lom (missile-out (missile-collided (game-missiles s) (game-invaders s))))
             (tock-tank (game-tank s))))


;; listof Invader -> listof Invader
;; updates all Invaders movement (x, y position)
(check-expect (tock-loinvader empty) empty)
(check-expect (tock-loinvader (list I1)) (list (make-invader (+ 150 12) (-  100 INVADER-Y-SPEED) 12)))
(check-expect (tock-loinvader (list I1 I2)) (list (make-invader (+ 150 12) (- 100 INVADER-Y-SPEED) 12)
                                                  (make-invader (+ 150 -10) (- HEIGHT INVADER-Y-SPEED) -10)))

; (define (tock-loinvader loi) loi)

#;
(define (tock-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-loinvader (first loi))
              (tock-loinvader (rest loi)))]))

(define (tock-loinvader loi)
  (cond [(empty? loi) empty]
        [else
         (cons (bounce-invader (move-invader (first loi)))
               (tock-loinvader (rest loi)))]))


;; Invader -> Invader
;; updates x by dx value and y by INVADER-Y-SPEED
(check-expect (move-invader I1) (make-invader (+ 150 12) (-  100 INVADER-Y-SPEED) 12))
(check-expect (move-invader I2) (make-invader (+ 150 -10) (- HEIGHT INVADER-Y-SPEED) -10))

; (define (move-invader invader) invader)

; <Invader template>
(define (move-invader invader)
  (make-invader (+ (invader-x invader) (invader-dx invader))
                (- (invader-y invader) INVADER-Y-SPEED)
                (invader-dx invader)))


;; Invader -> Invader
;; changes dx direction (-dx if left, dx if right) when Invader reaches screen border
(check-expect (bounce-invader I1) I1)
(check-expect (bounce-invader
               (make-invader (- WIDTH (/ (image-width INVADER) 2)) 250 10))
              (make-invader (- WIDTH (/ (image-width INVADER) 2)) 250 -10))
(check-expect (bounce-invader
               (make-invader (/ (image-width INVADER) 2) 250 -10))
              (make-invader (/ (image-width INVADER) 2) 250 10))

; (define (bounce-invader invader) invader)

; <Invader template>
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


;; listof Missile -> listof Missile
;; updates the y position of each missle in listof Mmissle
(check-expect (tock-lom empty) empty)
(check-expect (tock-lom (list M1)) (list (make-missile 150 (+ 300 MISSILE-SPEED))))
(check-expect (tock-lom (list M1 (make-missile 170 280) (make-missile 65 138)))
              (list (make-missile 150 (+ 300 MISSILE-SPEED))
                    (make-missile 170 (+ 280 MISSILE-SPEED))
                    (make-missile 65 (+ 138 MISSILE-SPEED))))

; (define (tock-lom lom) lom)

#;
(define (tock-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-lom (first lom))
              (tock-lom (rest lom)))]))

(define (tock-lom lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (tock-lom (rest lom)))]))


;; Missile -> Missile
;; updates Missile's y position by MISSILE-SPEED
(check-expect (move-missile M1) (make-missile 150 (+ 300 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 170 280)) (make-missile 170 (+ 280 MISSILE-SPEED)))

; (define (move-missile m) m)

; <Missile template>
(define (move-missile m)
  (make-missile (missile-x m) (+ (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; moves Tank x by TANK-SPEED multiplied by its direction if within screen, stops otherwise
(check-expect (tock-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (tock-tank (make-tank (/ WIDTH 2) -1)) (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (tock-tank (make-tank (/ (image-width TANK) 2) -1)) (make-tank (/ (image-width TANK) 2) -1)) ; tank at  left screen
(check-expect (tock-tank (make-tank (- WIDTH (/ (image-width TANK) 2)) 1)) (make-tank (- WIDTH (/ (image-width TANK) 2)) 1)) ; tank at right screen

; (define (tock-tank t) t)

; <Tank template>
(define (tock-tank t)
  (cond [(and (<= (tank-x t) (/ (image-width TANK) 2))
              (= (tank-dir t) -1))
         (make-tank (/ (image-width TANK) 2) -1)]
        [(and (>= (tank-x t) (- WIDTH (/ (image-width TANK) 2)))
              (= (tank-dir t) 1))
         (make-tank (- WIDTH (/ (image-width TANK) 2)) 1)]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))


;; Game -> Image
;; renders Game as a scene
; (make-game (list I1 I2) (list M1 M2) T1)
(check-expect (render G3)
              (overlay (put-image INVADER (invader-x I1) (invader-y I1)
                                  (put-image INVADER (invader-x I2) (invader-y I2) TRANSPARENT))
                       (put-image TANK (tank-x T1) TANK-HEIGHT/2 TRANSPARENT)
                       (put-image MISSILE (missile-x M1) (missile-y M1)
                                  (put-image MISSILE (missile-x M2) (missile-y M2) TRANSPARENT))
                       BACKGROUND))

; (define (render s) empty-image)

; <Game template>


(define (render s)
  (overlay (render-loinvader (game-invaders s))
           (render-tank (game-tank s))
           (render-lom (game-missiles s))
           BACKGROUND))



;; listof Missile -> Image
;; renders each missile as MISSILE at (x,y) position
(check-expect (render-lom empty) TRANSPARENT)
(check-expect (render-lom (list M1 M2)) (put-image MISSILE (missile-x M1) (missile-y M1)
                                                   (put-image MISSILE (missile-x M2) (missile-y M2) TRANSPARENT)))

; (define (render-lom lom) empty-image)

#;
(define (render-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-lom (first lom))
              (render-lom (rest lom)))]))

(define (render-lom lom)
  (cond [(empty? lom) TRANSPARENT]
        [else
         (put-image MISSILE
                    (missile-x (first lom))
                    (missile-y (first lom))
                    (render-lom (rest lom)))]))

;; listof Invader -> Image
;; renders each invader as INVADER at (x,y) position
(check-expect (render-loinvader empty) TRANSPARENT)
(check-expect (render-loinvader (list I1 I2)) (put-image INVADER (invader-x I1) (invader-y I1)
                                                         (put-image INVADER (invader-x I2) (invader-y I2) TRANSPARENT)))

; (define (render-loinvader loi) BACKGROUND)

#;
(define (render-loinvader loi s)
  (cond [(empty? loi) (... s)]
        [else
         (... (fn-for-loinvader (first loi) s)
              (render-loinvader (rest loi) s))]))

(define (render-loinvader loi)
  (cond [(empty? loi) TRANSPARENT]
        [else
         (put-image INVADER
                    (invader-x (first loi))
                    (invader-y (first loi))
                    (render-loinvader (rest loi)))]))


;; Tank -> Image
;; renders Tank as TANK at (x,y) position
(check-expect (render-tank T0) (put-image TANK (tank-x T0) TANK-HEIGHT/2 TRANSPARENT))

; (define (render-tank t) empty-image)

;<Tank template>
(define (render-tank t)
  (put-image TANK (tank-x t) TANK-HEIGHT/2 TRANSPARENT))


;; listof Invader listof Missile -> listof Invader
;; replaces Invader in list if collided to at least one of Missile in listof Missile
(check-expect (invader-collided empty           empty) empty)
(check-expect (invader-collided (list I1)       empty) (list I1))
; (check-expect (invader-collided (list I1)       (list M2)) (list IR))
; (check-expect (invader-collided (list I1 I2 I3) (list M1 M2)) (list IR I2 I3))
(check-expect (invader-collided (list I1)       (list M1)) (list I1))
(check-expect (invader-collided (list I1 I2 I3) (list M1)) (list I1 I2 I3))

#;
(define (invader-collided loi lom)
  (cond [(empty? loi) (...)]
        [(empty? lom) (...)]
        [else
         (... (fn-for-loinvader (first loi) (first lom))
              (invader-collided (rest loi) (rest lom)))]))

(define (invader-collided loi lom)
  (cond [(empty? loi) loi]
        [(empty? lom) loi]
        [else
         (invader-collided (loi-on-missile loi (first lom)) (rest lom))]))


;; listof Invader Missile -> listof Invader
;; compares if Missile has collided with an invader and replaces invader, otherwise nothing
(check-expect (loi-on-missile empty           M1) empty)
;(check-expect (loi-on-missile (list I1)       M2) (list (make-invader (random WIDTH) HEIGHT INVADER-X-SPEED)))
;(check-expect (loi-on-missile (list I1 I2 I3) M2) (list IR I2 I3)) guys how the hell do you check randoms
(check-expect (loi-on-missile (list I1)       M1) (list I1))
(check-expect (loi-on-missile (list I1 I2 I3) M1) (list I1 I2 I3))

; (define (loi-on-missile? loi m) loi) ; stub

(define (loi-on-missile loi m)
  (cond [(empty? loi) empty]
        [else
         (if (collided? (first loi) m)
             (cons (make-invader (random WIDTH) HEIGHT (random-sign INVADER-X-SPEED)) (rest loi))
             (cons (first loi) (loi-on-missile (rest loi) m)))]))


;; listof Missile listof Invader -> listof Missile
;; removes Invader in list if collided to at least one of Missile in listof Missile
(check-expect (missile-collided empty        empty) empty)
(check-expect (missile-collided (list M1)    empty) (list M1))
(check-expect (missile-collided (list M2)    (list I1)) empty)
(check-expect (missile-collided (list M1 M2) (list I1 I2 I3)) (list M1))
(check-expect (missile-collided (list M1)    (list I1)) (list M1))
(check-expect (missile-collided (list M1)    (list I1 I2 I3)) (list M1))

#;
(define (missile-collided lom loi)
  (cond [(empty? loi) (...)]
        [(empty? lom) (...)]
        [else
         (... (fn-for-lom (first lom) (first loi))
              (missile-collided (rest lom) (rest loi)))]))

(define (missile-collided lom loi)
  (cond [(empty? lom) lom]
        [(empty? loi) lom]
        [else
         (missile-collided (lom-on-invader lom (first loi)) (rest loi))]))


;; listof Missile Invader -> listof Missile
;; compares if Missile has collided with an invader and removes invader, otherwise nothing
(check-expect (lom-on-invader empty        empty) empty)
(check-expect (lom-on-invader (list M2)    I1)    empty)
(check-expect (lom-on-invader (list M1 M2) I1)    (list M1))
(check-expect (lom-on-invader (list M1)    I2)    (list M1))
(check-expect (lom-on-invader (list M1 M2) I2)    (list M1 M2))

; (define (loi-on-missile? loi m) loi) ; stub

(define (lom-on-invader lom i)
  (cond [(empty? lom) empty]
        [else
         (if (collided? i (first lom))
             (rest lom)
             (cons (first lom) (lom-on-invader (rest lom) i)))]))


;; Missile Invader -> Boolean
;; checks if missile and invader collided with circle collision
;; just trust the math for this one i think lol
(check-expect (collided? I1 M2) true)
(check-expect (collided? I1 M1) false)

; (define (collided? i m) false) ; stub

#;
(define (collided? i m)
  (... (missile-x m) (missile-y m)
       (invader-x invader) (invader-y invader) (invader-dx invader)))

(define (collided? i m)
  (if (<= (sqrt (+ (expt (- (missile-x m) (invader-x i)) 2)
                   (expt (- (missile-y m) (invader-y i)) 2)))
          RADIUSES)
      true
      false))


;; Number -> Number
;; randomly turns number to positive or negative
;; i dont think check expect is necessary here

; (define (random-sign n) 0) ; stub

#;
(define (random-sign n)
  (...))

(define (random-sign n)
  (if (= (random 2) 0) 
      n              
      (- n)))


;; listof Missile -> listof Missile
;; removes missile if out of screen
(check-expect (missile-out empty) empty)

(check-expect (missile-out (list (make-missile (/ WIDTH 2) HEIGHT)))   ; half of missile is still on screen
              (missile-out (list (make-missile (/ WIDTH 2) HEIGHT))))

(check-expect (missile-out (list (make-missile (/ WIDTH 2)
                                               (+ HEIGHT (/ (image-height MISSILE) 2))))) ; missile should be completely unseen
              empty)

; (define (missile-out lom) lom) ; stub

#;
(define (missile-out lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-lom (first lom))
              (missile-out (rest lom)))]))

(define (missile-out lom)
  (cond [(empty? lom) empty]
        [else
         (if (>= (missile-y (first lom)) (+ HEIGHT (/ (image-height MISSILE) 2)))
             (rest lom)
             (cons  (first lom) (missile-out (rest lom))))
         ]))


;; Game -> Boolean
;; kills game if invader lands on bottom of screen
(check-expect (invader-lands (make-game empty empty T1)) false)

(check-expect (invader-lands (make-game (list (make-invader (/ WIDTH 2) (/ (image-height INVADER) 2) 5)) empty T1)) ; invader sitting on screen
              true)

(check-expect (invader-lands (make-game (list (make-invader (/ WIDTH 2) (/ HEIGHT 2) -5)) empty T1)) ; invader half way
              false)

; (define (invader-lands loi) false) ; stub

#;
(define (invader-lands loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-loinvader (first loi))
              (invader-lands (rest loi)))]))

(define (invader-lands loi)
  (cond [(empty? (game-invaders loi)) false]
        [else
         (if (<= (invader-y (first (game-invaders loi))) (/ (image-height INVADER) 2))
             true
             (invader-lands (make-game (rest (game-invaders loi)) (game-missiles loi) (game-tank loi))))]))

;; Game KeyEvent -> Game
;; handles the KeyEvent functions for gameplay
(check-expect (handle-key G3 "left") (make-game (list I1 I2) (list M1 M2) (make-tank 50 (* 1 -1))))
(check-expect (handle-key G3 "right") G3)
(check-expect (handle-key (make-game (list I1) (list M2) T2) "left")
              (make-game (list I1) (list M2) T2))
(check-expect (handle-key (make-game (list I1) (list M2) T2) "right")
              (make-game (list I1) (list M2) (make-tank 50 (* -1 -1))))
(check-expect (handle-key G3 " ") (make-game (list I1 I2) (list (make-missile (tank-x T1) (* TANK-HEIGHT/2 2)) M1 M2) T1))


; (define (handle-key g ke) g)

; <template for KeyEvent>
(define (handle-key g ke)
  (cond [(key=? ke "left") (change-direction g ke)]
        [(key=? ke "right") (change-direction g ke)]
        [(key=? ke " ") (shoot-missile g ke)]
        [else 
         g]))


;; Game KeyEvent -> Game
;; changes the direction of tank in a game
(check-expect (change-direction G3 "left") (make-game (list I1 I2) (list M1 M2) (make-tank 50 (* 1 -1))))
(check-expect (change-direction G3 "right") G3)
(check-expect (change-direction (make-game (list I1) (list M1) (make-tank 68 -1)) "right")
              (make-game (list I1) (list M1) (make-tank 68 1)))
(check-expect (change-direction (make-game (list I1) (list M1) (make-tank 68 -1)) "left")
              (make-game (list I1) (list M1) (make-tank 68 -1)))

; (define (change-direction g ke) g) ; stub

; <Game template>
(define (change-direction s ke)
  (make-game (game-invaders s)
             (game-missiles s)
             (change-direction--tank (game-tank s) ke)))


;; Tank KeyEvent -> Tank
;; multiplies tank's direction by -1 to switch direction
(check-expect (change-direction--tank T1 "left") (make-tank 50 -1))
(check-expect (change-direction--tank T1 "right") T1)
(check-expect (change-direction--tank (make-tank 68 -1) "right") (make-tank 68 1))
(check-expect (change-direction--tank (make-tank 68 -1) "left") (make-tank 68 -1))

; (define (change-direction--tank t ke) t) ; stub

; <Tank template>
(define (change-direction--tank t ke)
  (if (or (and (string=? ke "left")
               (= -1 (tank-dir t)))
          (and (string=? ke "right")
               (= 1 (tank-dir t))))
      t
      (make-tank (tank-x t) (* (tank-dir t) -1))))


;; Game KeyEvent -> Game
;; shoots a missile where tank is currently at
(check-expect (shoot-missile (make-game empty empty T0) " ") (make-game empty (list (make-missile (tank-x T0) (* TANK-HEIGHT/2 2))) T0))
(check-expect (shoot-missile G3 " ")
              (make-game (list I1 I2) (list (make-missile (tank-x T1) (* TANK-HEIGHT/2 2)) M1 M2) T1))

; (define (shoot-missile g ke) g) ;stub

; <Game template with additional atomic paramter ke>
(define (shoot-missile s ke)
  (make-game  (game-invaders s)
              (add-missile (game-missiles s) ke (game-tank s))
              (game-tank s)))


;; listof Missile KeyEvent Tank -> listof Missile
;; prepends a missile from where tank currently is
(check-expect (add-missile empty " " T1) (list (make-missile (tank-x T1) (* TANK-HEIGHT/2 2))))
(check-expect (add-missile empty "a" T1) empty)
(check-expect (add-missile (list M1 M2) " " T0)
              (list (make-missile (tank-x T0) (* TANK-HEIGHT/2 2)) M1 M2))

; (define (add-missile m ke t) m) ;stub

#;
(define (add-missile lom ke t)
  (... lom ke t))

(define (add-missile lom ke t)
  (if (string=? " " ke)
      (append (list (make-missile (tank-x t) (* TANK-HEIGHT/2 2))) lom)
      lom))


(main start)
