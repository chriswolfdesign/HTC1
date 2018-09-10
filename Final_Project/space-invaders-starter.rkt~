;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-Y (- TANK-Y 20))


;; Data Definitions:

(define-struct game (invaders missiles t))
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
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. each invader is an invader object trying to reach the bottom of the screen
(define LOI1 empty)
(define LON2 (list (make-invader 100 200 INVADER-X-SPEED) (make-invader 200 300 (- INVADER-X-SPEED))))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvader)
;;  - self-reference: (rest loi) is ListOfInvader


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. each missile is a missile object trying to shoot down invaders
(define LOM1 empty)
(define LOM2 (list (make-missile 100 200) (make-missile 200 300)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile ListOfMissile)
;;  - self-reference: (rest lom) is ListOfMissile


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

(define STARTING-INVADERS (list (make-invader 100 -80 INVADER-X-SPEED)
                                (make-invader 85 -65 INVADER-X-SPEED)
                                (make-invader 70 -40 INVADER-X-SPEED)
                                (make-invader 55 -25 INVADER-X-SPEED)
                                (make-invader 40 -10 INVADER-X-SPEED)
                                (make-invader 250 -200 INVADER-X-SPEED)
                                (make-invader 235 -185 INVADER-X-SPEED)
                                (make-invader 220 -170 INVADER-X-SPEED)
                                (make-invader 205 -155 INVADER-X-SPEED)
                                (make-invader 190 -140 INVADER-X-SPEED)
                                (make-invader 175 -125 INVADER-X-SPEED)))

(define GAME-START-STATE (make-game STARTING-INVADERS empty T0))


;; =================
;; Functions:

;; Game -> Game
;; start the world with (main GAME-START-STATE)
;; 
(define (main g)
  (big-bang g                                ; Game
            (on-tick   update-game)          ; Game -> Game
            (to-draw   render-game)          ; Game -> Image
            (stop-when game-over)            ; Game -> Boolean
            (on-key    handle-key)))         ; Game KeyEvent -> Game


;; Game -> Game
;; produce the next frame in our gameplay, updating the invaders, missiles, and tank as necessary
(check-expect (update-game (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (update-game (make-game empty empty (make-tank (/ WIDTH 2) -1)))
              (make-game empty empty (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))) 

; (define (update-game g) (make-game empty empty (make-tank (/ WIDTH 2) 1))) ; Stub

; <use template from Game>

(define (update-game g)
  (make-game (update-invaders (game-invaders g) (game-missiles g))
             (update-missiles (game-missiles g))
             (update-tank (game-t g))))


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Consumes a list of invader characters, returns the list being moved by one frame.  Also removes any invaders that have been hit by missiles
;; Collision mechanics have been tested elsewhere!
(check-expect (update-invaders empty empty) empty)           ; Empty List
(check-expect (update-invaders (list (make-invader 100 200 INVADER-X-SPEED)) empty)
              (list (make-invader (+ 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) INVADER-X-SPEED)))
(check-expect (update-invaders (list (make-invader 100 200 INVADER-X-SPEED)
                                     (make-invader 200 300 (- INVADER-X-SPEED))) empty)
              (list (make-invader (+ 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) INVADER-X-SPEED)
                    (make-invader (- 200 INVADER-X-SPEED) (+ 300 INVADER-Y-SPEED) (- INVADER-X-SPEED))))
(check-expect (update-invaders (list (make-invader (add1 WIDTH) 100 INVADER-X-SPEED)
                                     (make-invader -5 200 (- INVADER-X-SPEED))) empty)
              (list (make-invader WIDTH 100 (- INVADER-X-SPEED))
                    (make-invader 0 200 INVADER-X-SPEED)))

; (define (update-invaders invaders missiles) empty) ; Stub

; <use template from ListOfInvaders>

(define (update-invaders invaders missiles)
  (cond [(empty? invaders) empty]
        [(got-hit? (first invaders) missiles) (rest invaders)] 
        [else
         (cons (update-invader (first invaders)) (update-invaders (rest invaders) missiles))]))


;; Invader ListOfMissiles -> Boolean
;; Returns true if the invader has collided with one of the missiles in the List
(check-expect (got-hit? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 100 200))) true)
(check-expect (got-hit? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 100 400))) false)
(check-expect (got-hit? (make-invader 100 200 INVADER-X-SPEED) (list (make-missile 100 400) (make-missile 100 200))) true)

; (define (got-hit? i missiles) true) ; Stub

; <use template from ListOfMissile

(define (got-hit? i missiles)
  (cond [(empty? missiles) false]
        [else
         (or (collision? i (first missiles)) (got-hit? i (rest missiles)))]))


;; Invader Missile -> Boolean
;; Returns true if the invader and the missile are touching, false otherwise
(check-expect (collision? (make-invader 100 200 INVADER-X-SPEED) (make-missile 100 200)) true)
(check-expect (collision? (make-invader 100 200 INVADER-X-SPEED) (make-missile 100 400)) false)

; (define (collision? i m) true) ; Stub

; <use template from Invader>

(define (collision? i m)
  (and (< (- (missile-x m) (invader-x i)) HIT-RANGE) (< (- (missile-y m) (invader-y i)) HIT-RANGE)))


;; Invader -> Invaders
;; Moves the invader by one frame
(check-expect (update-invader (make-invader 100 200 INVADER-X-SPEED))                                  ; Invader is moving right
              (make-invader (+ 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) INVADER-X-SPEED))
(check-expect (update-invader (make-invader 100 200 (- INVADER-X-SPEED)))                              ; Invader is moving left
              (make-invader (- 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
(check-expect (update-invader (make-invader 0 200 INVADER-X-SPEED))                                    ; Invader is on left edge, moving right
              (make-invader (+ 0 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) INVADER-X-SPEED))
(check-expect (update-invader (make-invader WIDTH 200 (- INVADER-X-SPEED)))                            ; Invader is on right edge, moving left
              (make-invader (- WIDTH INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) (- INVADER-X-SPEED)))
(check-expect (update-invader (make-invader -5 200 (- INVADER-X-SPEED)))                               ; Invader is on left edge, moving left
              (make-invader 0 200 INVADER-X-SPEED))
(check-expect (update-invader (make-invader (add1 WIDTH) 200 INVADER-X-SPEED))                         ; Invader is on right edge, moving right
              (make-invader WIDTH 200 (- INVADER-X-SPEED)))

; (define (update-invader i) i)

; <use template from Invader>

(define (update-invader i)
  (cond [(invader-touch-left-border? i)
         (make-invader 0 (invader-y i) INVADER-X-SPEED)]
        [(invader-touch-right-border? i)
         (make-invader WIDTH (invader-y i) (- INVADER-X-SPEED))]
        [else
         (move-invader i)]))


;; Invader -> Boolean
;; Return true if the invader is going to the left and has hit the left border
(check-expect (invader-touch-left-border? (make-invader 100 200 INVADER-X-SPEED)) false)      ; Invader in middile, moving right
(check-expect (invader-touch-left-border? (make-invader 0 200 INVADER-X-SPEED)) false)        ; Invader on left border, moving right
(check-expect (invader-touch-left-border? (make-invader -5 200 (- INVADER-X-SPEED))) true)    ; Invader on left border, moving left

; (define (invader-touch-left-border? i) false) ; Stub

; <use template from Invader>

(define (invader-touch-left-border? i)
  (and (< (invader-x i) 0) (< (invader-dx i) 0)))


;; Invader -> Boolean
;; Return ture if the invader is going to the right and has hit the right border
(check-expect (invader-touch-right-border? (make-invader 100 200 INVADER-X-SPEED)) false)            ; Invader in middile, moving right
(check-expect (invader-touch-right-border? (make-invader (add1 WIDTH) 200 INVADER-X-SPEED)) true)    ; Invader on right border, moving right
(check-expect (invader-touch-right-border? (make-invader WIDTH 200 (- INVADER-X-SPEED))) false)      ; Invader on right border, moving left

; (define (invader-touch-right-border? i) false) ; Stub

; <use template from Invader>

(define (invader-touch-right-border? i)
  (and (> (invader-x i) WIDTH) (> (invader-dx i) 0)))


;; Invader -> Invader
;; Returns the original invader, moved by one frame
(check-expect (move-invader (make-invader 100 200 INVADER-X-SPEED)) (make-invader (+ 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) INVADER-X-SPEED))            ; Invader is moving right
(check-expect (move-invader (make-invader 100 200 (- INVADER-X-SPEED))) (make-invader (- 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) (- INVADER-X-SPEED)))    ; Invader is moving left

; (define (move-invader i) i) ; Stub

; <use template from Invader>

(define (move-invader i)
  (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i)))


;; ListOfMissiles -> ListOfMissiles
;; Consumes a list of missile projectiles, returns the list being moved by one frame
(check-expect (update-missiles empty) empty)                                                                  ; Empty list
(check-expect (update-missiles (list (make-missile 45 MISSILE-Y)))                                            ; List with one missile
              (list (make-missile 45 (- MISSILE-Y MISSILE-SPEED))))
(check-expect (update-missiles (list (make-missile 45 MISSILE-Y) (make-missile 50 200)))                      ; List with two missiles
              (list (make-missile 45 (- MISSILE-Y MISSILE-SPEED)) (make-missile 50 (- 200 MISSILE-SPEED))))
(check-expect (update-missiles (list (make-missile 45 -3) (make-missile 50 200)))                             ; List with two missiles but one has gone off screen
              (list (make-missile 50 (- 200 MISSILE-SPEED))))

; (define (update-missiles missiles) empty) ; Stub

; <use template from ListOfMissile>

(define (update-missiles missiles)
  (cond [(empty? missiles) empty]
        [(missile-out-of-bounds? (first missiles))
         (update-missiles (rest missiles))]
        [else
         (cons (move-missile (first missiles)) (update-missiles (rest missiles)))]))


;; Missile -> Boolean
;; returns true if the missile has left the viewing area in the background, false otherwise
(check-expect (missile-out-of-bounds? (make-missile 50 50)) false)
(check-expect (missile-out-of-bounds? (make-missile 45 0)) false)
(check-expect (missile-out-of-bounds? (make-missile 30 -5)) true)

; (define (missile-out-of-bounds? m) true) ; Stub

; <use template from Missile>

(define (missile-out-of-bounds? m)
  ( < (missile-y m) 0))


;; Missile -> Missile
;; produce the missile that has been moved by one frame
(check-expect (move-missile (make-missile 50 200)) (make-missile 50 (- 200 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 30 400)) (make-missile 30 (- 400 MISSILE-SPEED)))

; (define (move-missile m) m) ; Stub

; <use template from Missile>

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; Consumes a tank, returns teh tank being moved by one frame
(check-expect (update-tank (make-tank (/ WIDTH 2) 1))            ; Tank starts at center and moves right
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)) 
(check-expect (update-tank (make-tank (/ WIDTH 2) -1))           ; Tank starts at center and moves left
              (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (update-tank (make-tank WIDTH 1))                  ; Tank is stuck on right edge
              (make-tank WIDTH 1))
(check-expect (update-tank (make-tank 0 -1))                     ; Tank is stuck on left edge
              (make-tank 0 -1))

; (define (update-tank t) (make-tank (/ WIDTH 2) 1)) ; Stub

; <use template from Tank>

(define (update-tank t)
  (cond [(tank-hit-left-border? t) (make-tank 0 -1)]
        [(tank-hit-right-border? t) (make-tank WIDTH 1)]
        [(= 1 (tank-dir t)) (move-tank-right t)]
        [(= -1 (tank-dir t)) (move-tank-left t)]))


;; Tank -> Boolean
;; Returns true if the tank is moving to the left and hitting the left border
(check-expect (tank-hit-left-border? (make-tank (/ WIDTH 2) -1)) false)        ; Tank is in center moving left
(check-expect (tank-hit-left-border? (make-tank (/ WIDTH 2) 1)) false)         ; Tank is in center moving right
(check-expect (tank-hit-left-border? (make-tank 0 -1)) true)                   ; Tank is at left border, moving left
(check-expect (tank-hit-left-border? (make-tank 0 1)) false)                   ; Tank is at left border, moving right

; (define (tank-hit-left-border? t) true) ; Stub

; <use template from Tank>

(define (tank-hit-left-border? t)
  (and (< (tank-x t) TANK-SPEED) (= -1 (tank-dir t))))


;; Tank -> Boolean
;; Returns true if the tank is moving right and hitting the right border
(check-expect (tank-hit-right-border? (make-tank (/ WIDTH 2) 1)) false)     ; Tank is in center, moving right
(check-expect (tank-hit-right-border? (make-tank (/ WIDTH 2) -1)) false)    ; Tank is in center, moving left
(check-expect (tank-hit-right-border? (make-tank WIDTH 1)) true)            ; Tank is at right border, moving right
(check-expect (tank-hit-right-border? (make-tank WIDTH -1)) false)          ; Tank is at right border, moving left

; (define (tank-hit-right-border? t) true) ; Stub

; <use template from Tank>

(define (tank-hit-right-border? t)
  (and (> (tank-x t) (- WIDTH TANK-SPEED)) (= 1 (tank-dir t))))


;; Tank -> Tank
;; Moves the given tank one frame to the right
(check-expect (move-tank-right (make-tank (/ WIDTH 2) 1))
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (move-tank-right (make-tank 50 1))
              (make-tank (+ 50 TANK-SPEED) 1))

; (define (move-tank-right t) t) ; Stub

; <use template from Tank>

(define (move-tank-right t)
  (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t)))


;; Tank -> Tank
;; Moves the given tank one frame to the left
(check-expect (move-tank-left (make-tank (/ WIDTH 2) -1))
              (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (move-tank-left (make-tank 50 -1))
              (make-tank (- 50 TANK-SPEED) -1))

; (define (move-tank-left t) t) ; Stub

; <use template from Tank>

(define (move-tank-left t)
  (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))


;; Game -> Image
;; Creates a display of the current state of our game 
(check-expect (render-game (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))
(check-expect (render-game (make-game empty empty (make-tank 50 1)))
              (place-image TANK 50 TANK-Y BACKGROUND))

; (define (render-game g) BACKGROUND) ; Stub

; <use template from Game>

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-t g) BACKGROUND))))


;; ListOfInvader Image -> Image
;; Renders all of the invaders in our list onto our image
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list (make-invader 100 200 INVADER-X-SPEED)) BACKGROUND)
              (place-image INVADER 100 200 BACKGROUND))
(check-expect (render-invaders (list (make-invader 100 200 INVADER-X-SPEED)
                                     (make-invader 200 300 (- INVADER-X-SPEED))) BACKGROUND)
              (place-image INVADER 200 300 (place-image INVADER 100 200 BACKGROUND)))

; (define (render-invaders invaders img) img) ; Stub

; <use Stub from ListOfInvader>

(define (render-invaders invaders img)
  (cond [(empty? invaders) img]
        [else
         (render-invader (first invaders) (render-invaders (rest invaders) img))]))


;; Invader Image -> Image
;; Places the given invader onto the correct position of the given image
(check-expect (render-invader (make-invader 100 200 INVADER-X-SPEED) BACKGROUND)
              (place-image INVADER 100 200 BACKGROUND))
(check-expect (render-invader (make-invader 200 300 (- INVADER-X-SPEED)) BACKGROUND)
              (place-image INVADER 200 300 BACKGROUND))

; (define (render-invader i img) img) ; Stub

; <use template from Invader>

(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;; ListOfMissiles Image -> Image
;; Renders all of the missiles in our list onto our image
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list (make-missile 50 200)) BACKGROUND)
              (place-image MISSILE 50 200 BACKGROUND))
(check-expect (render-missiles (list (make-missile 50 200) (make-missile 100 300)) BACKGROUND)
              (place-image MISSILE 50 200 (place-image MISSILE 100 300 BACKGROUND)))

; (define (render-missiles missiles img) img) ; Stub

; <use template from ListOfMissile>

(define (render-missiles missiles img)
  (cond [(empty? missiles) img]
        [else
         (render-missile (first missiles) (render-missiles (rest missiles) img))]))


;; Missile Image -> Image
;; Places the given missile onto the correct position of the given image
(check-expect (render-missile (make-missile 50 100) BACKGROUND)
              (place-image MISSILE 50 100 BACKGROUND))
(check-expect (render-missile (make-missile 200 300) BACKGROUND)
              (place-image MISSILE 200 300 BACKGROUND))

; (define (render-missile m img) img)

; <use template from Missile>

(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;; Tank Image -> Image
;; Renders our tank onto the correct position in the image
(check-expect (render-tank (make-tank (/ WIDTH 2) 1) BACKGROUND)
              (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))
(check-expect (render-tank (make-tank 50 1) BACKGROUND)
              (place-image TANK 50 TANK-Y BACKGROUND))

; (define (render-tank t img) img)

; <use template from Tank>

(define (render-tank t img)
  (place-image TANK (tank-x t) TANK-Y BACKGROUND))


;; Game MouseEvent -> Game
;; Moves the tank the appropriate direction if the left or right keys are pressed
;; If space bar is pressed, we add a new missile into the list of missiles
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) -1)) "left")                      ; Going left, left arrow was pressed
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) 1)) "left")                       ; Going right, left arrow was pressed
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) -1)) "right")                     ; Going left, right arrow was pressed
              (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) 1)) "right")                      ; Going right, right arrow was pressed
              (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) -1)) "x")                         ; Going left, unsupported key was pressed
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) 1)) "x")                          ; Going right, unsupported key was pressed
              (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) 1)) " ")
              (make-game empty (list (make-missile (/ WIDTH 2) MISSILE-Y)) (make-tank (/ WIDTH 2) 1)))    ; Space Bar was pressed

; (define (handle-key g ke) g) ; Stub

; <use template from HtdW Formula page>

(define (handle-key g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-t g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-t g)) 1))]
        [(key=? ke " ") (make-game (game-invaders g) (shoot-missile g) (game-t g))]
        [else g]))


;; Game -> ListOfMissiles
;; Takes in a game and returns the ListOfMissiles in the game, with a new missile added at the tank's x-coordinate
(check-expect (shoot-missile (make-game empty empty (make-tank 50 1))) (cons (make-missile 50 MISSILE-Y) empty))
(check-expect (shoot-missile (make-game empty (list (make-missile 50 MISSILE-Y)) (make-tank 45 1)))
              (list (make-missile 45 MISSILE-Y) (make-missile 50 MISSILE-Y)))

; (define (shoot-missile g) g) ; Stub

; <use template from Game>

(define (shoot-missile g)
  (cons (make-missile (tank-x (game-t g)) MISSILE-Y) (game-missiles g)))


;; Game -> Boolean
;; Returns true if the game has either been lost or won
(check-expect (game-over (make-game (list (make-invader 100 200 INVADER-X-SPEED)) empty T0)) false)             ; The game is not over
(check-expect (game-over (make-game empty empty T0)) true)                                                      ; The player has won the game
(check-expect (game-over (make-game (list (make-invader 100 (add1 HEIGHT) INVADER-X-SPEED)) empty T0)) true)    ; The player has lost the game

; (define (game-over g) false)

; <use template from Game>

(define (game-over g)
  (or (game-won (game-invaders g)) (game-lost (game-invaders g))))


;; ListOfInvaders -> Boolean
;; Returns true if the list is empty
(check-expect (game-won empty) true)
(check-expect (game-won (list (make-invader 100 200 INVADER-X-SPEED))) false)

; (define (game-won invaders) false) ; Stub

; <use template from ListOfInvader>

(define (game-won invaders)
  (empty? invaders))


;; ListOfInvader -> Boolean
;; Returns true if one of the invaders has touched the bottom of the screen
(check-expect (game-lost (list (make-invader 100 200 INVADER-X-SPEED))) false)
(check-expect (game-lost (list (make-invader 100 (add1 HEIGHT) INVADER-X-SPEED))) true)
(check-expect (game-lost (list (make-invader 100 200 INVADER-X-SPEED) (make-invader 200 300 INVADER-X-SPEED))) false)
(check-expect (game-lost (list (make-invader 100 200 INVADER-X-SPEED) (make-invader 200 (add1 HEIGHT) INVADER-X-SPEED))) true)

; (define (game-lost invaders) false)

; <use template from ListOfInvader>

(define (game-lost invaders)
  (cond [(empty? invaders) false]
        [else
         (or (touched-bottom? (first invaders)) (game-lost (rest invaders)))]))


;; Invader -> Boolean
;; Returns true if the invader has reached the bottom of the screen
(check-expect (touched-bottom? (make-invader 100 200 INVADER-X-SPEED)) false)
(check-expect (touched-bottom? (make-invader 100 (add1 HEIGHT) INVADER-X-SPEED)) true)

; (define (touched-bottom? i) false) ; Stub

; <use template from Invader>

(define (touched-bottom? i)
  (> (invader-y i) HEIGHT))