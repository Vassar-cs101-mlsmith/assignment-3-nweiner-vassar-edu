;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101 
; Fall 2018
; Assign 3
; Nicholas Weiner
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300)

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (im x y dx dy))
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. '(), or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
;   These will help you test bounce conditions.

; here's one of my ball CONSTANTS, which you may use or modify
; if you like to define the rest.
(define BALL-AT-LEFT 
  (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) -4 4))
(define BALL-AT-RIGHT 
  (make-ball (circle (+ RADIUS 5) "solid" "red")
             (- WIDTH (+ RADIUS 5)) (/ HEIGHT 2) 3 3))
(define BALL-AT-TOP 
  (make-ball (circle (+ RADIUS 6) "solid" "blue")
             (/ WIDTH 2) (+ RADIUS 6) 5 -5))
(define BALL-AT-BOTTOM 
  (make-ball (circle (+ RADIUS 7) "solid" "yellow")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 7)) 2 2)) 


; Define INIT-LOB to be a list-of-balls:
; You will use this to be the initial state of the world.
; I've defined it to be the empty list, but you should define it
; to contain the four example ball CONSTANTS you just defined. 
(define INIT-LOB (list BALL-AT-LEFT BALL-AT-RIGHT BALL-AT-TOP BALL-AT-BOTTOM))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
; Use these to help you get started with the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ???
; Template for a function that consumes a ball
(define (fun-for-ball b) 
  (...(ball-im b)...
   ...(ball-x b)...(ball-y b)...
   ...(ball-dx b)...(ball-dy b)...))

; list-of-balls -> ???
; Template for a function that consumes a list-of-balls
(define (fun-for-list-of-balls lob) 
  (cond
    [(empty? lob)...] 
    [else (...(fun-for-ball (first lob))...
           ...(fun-for-lob (rest lob))...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the functions below, in order. I've supplied the
; signature, purpose statement, and header for each function.
;
; You provide the check-expect examples, and using the appropriate
; template, complete the function bodies.
;
; I recommend you proceed in order, and complete each function,
; with passing tests, before going on to the next.
;
; The reason for completing the functions in the order they appear
; is earlier functions can be used as helper functions for the
; later functions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> number
; computes the radius of given ball
(define (ball-radius b) (/ (image-height (ball-im b)) 2))

(check-expect (ball-radius BALL-AT-LEFT) 29)
(check-expect (ball-radius BALL-AT-RIGHT) 30)
(check-expect (ball-radius BALL-AT-TOP) 31)
(check-expect (ball-radius BALL-AT-BOTTOM) 32)

; ball -> boolean
; determines whether the ball reached the top edge of scene
(define (top-edge? b) (<= (ball-y b) (ball-radius b)))

(check-expect (top-edge? BALL-AT-TOP) #t)
(check-expect (top-edge? BALL-AT-BOTTOM) #f)

; ball -> boolean
; determines whether the ball reached the bottom edge of scene
(define (bottom-edge? b) (>= (ball-y b) (- HEIGHT (ball-radius b))))

(check-expect (bottom-edge? BALL-AT-TOP) #f)
(check-expect (bottom-edge? BALL-AT-BOTTOM) #t)

; ball -> boolean
; determines whether the ball reached the left edge of scene
(define (left-edge? b) (<= (ball-x b) (ball-radius b)))

(check-expect (left-edge? BALL-AT-LEFT) #t)
(check-expect (left-edge? BALL-AT-RIGHT) #f)

; ball -> boolean
; determines whether the ball reached the right edge of scene
(define (right-edge? b) (>= (ball-x b) (- WIDTH (ball-radius b))))

(check-expect (right-edge? BALL-AT-LEFT) #f)
(check-expect (right-edge? BALL-AT-RIGHT) #t)

; ball -> ball
; reverse ball's up-down direction   
(define (reverse-up-down b) (make-ball (ball-im b) (ball-x b) (ball-y b)
                            (ball-dx b) (* -1 (ball-dy b))))

(check-expect (reverse-up-down BALL-AT-TOP)
              (make-ball (circle (+ RADIUS 6) "solid" "blue")
              (/ WIDTH 2) (+ RADIUS 6) 5 5))

; ball -> ball
; reverse ball's left-right direction   
(define (reverse-left-right b) (make-ball (ball-im b) (ball-x b) (ball-y b)
                               (* -1 (ball-dx b)) (ball-dy b)))

(check-expect (reverse-left-right BALL-AT-TOP)
              (make-ball (circle (+ RADIUS 6) "solid" "blue")
              (/ WIDTH 2) (+ RADIUS 6) -5 -5))

; ball -> ball
; changes direction of given ball if it hit the top or bottom edge
(define (bounce-up-down b)
  (cond
    [(or (top-edge? b) (bottom-edge? b)) (reverse-up-down b)]
    [else b]))

(check-expect (bounce-up-down BALL-AT-TOP) (reverse-up-down BALL-AT-TOP))
(check-expect (bounce-up-down BALL-AT-BOTTOM) (reverse-up-down BALL-AT-BOTTOM))
(check-expect (bounce-up-down BALL-AT-RIGHT) BALL-AT-RIGHT) 

; ball -> ball
; changes direction of given ball if it hit the left or right edge
(define (bounce-left-right b)
  (cond
    [(or (left-edge? b) (right-edge? b)) (reverse-left-right b)]
    [else b]))

(check-expect (bounce-left-right BALL-AT-RIGHT) (reverse-left-right BALL-AT-RIGHT))
(check-expect (bounce-left-right BALL-AT-LEFT) (reverse-left-right BALL-AT-LEFT))
(check-expect (bounce-left-right BALL-AT-TOP) BALL-AT-TOP) 

; ball -> ball
; moves the given ball by its dx and dy amounts
(define (move-ball b)
  (make-ball (ball-im b)(+ (ball-dx b) (ball-x b))(+ (ball-dy b)(ball-y b))
             (ball-dx b)(ball-dy b)))

(check-expect (move-ball BALL-AT-LEFT)
              (make-ball (circle (+ RADIUS 4) "solid" "teal")
              (- (+ 4 RADIUS) 4)(+ 4 (/ HEIGHT 2)) -4 4))
(check-expect (move-ball BALL-AT-RIGHT)
              (make-ball (circle (+ RADIUS 5) "solid" "red")
              (- WIDTH (+ RADIUS 5) -3) (+ 3 (/ HEIGHT 2)) 3 3))
(check-expect (move-ball BALL-AT-TOP)
              (make-ball (circle (+ RADIUS 6) "solid" "blue")
              (+ 5 (/ WIDTH 2)) (- (+ RADIUS 6) 5) 5 -5))
(check-expect (move-ball BALL-AT-BOTTOM)
              (make-ball (circle (+ RADIUS 7) "solid" "yellow")
              (+ 2 (/ WIDTH 2)) (+ 2 (- HEIGHT (+ RADIUS 7))) 2 2))
 
; list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
(define (move-list-of-balls lob) 
  (cond
    [(empty? lob) '()] 
    [(cons? lob) (all-pos-collides(cons (move-ball (bounce-up-down( bounce-left-right(first lob))))
           (move-list-of-balls(rest lob))))]))

(check-expect (move-list-of-balls '()) '())
(check-expect (cons? (move-list-of-balls INIT-LOB)) #t)

; ball image -> image
; renders given ball b on given background bg
(define (render-ball b bg) (place-image (ball-im b) (ball-x b) (ball-y b) bg))

(check-expect (render-ball BALL-AT-TOP BACKGROUND)
              (place-image (circle (+ RADIUS 6) "solid" "blue")
              (/ WIDTH 2) (+ RADIUS 6) BACKGROUND))
(check-expect (render-ball BALL-AT-BOTTOM BACKGROUND)
              (place-image (circle (+ RADIUS 7) "solid" "yellow")
              (/ WIDTH 2) (- HEIGHT (+ RADIUS 7)) BACKGROUND))

;ball ball -> ball
;changes sign of both velocities of a ball depending on the direction the ball 1 collides with ball 2
(define (reverse-dir b1 b2)
  (cond
    [(and (negative? (* (ball-dx b1) (ball-dx b2))) (negative? (* (ball-dy b1) (ball-dy b2))))
     (move-ball(reverse-left-right (reverse-up-down b1)))]
    [(negative? (* (ball-dx b1) (ball-dx b2))) (reverse-if-greater-dy(reverse-left-right b1) b2)]
    [(negative? (* (ball-dy b1) (ball-dy b2))) (reverse-if-greater-dx(reverse-up-down b1) b2)]
    [else (reverse-if-greater-dx(reverse-if-greater-dy b1 b2) b2)]))
  ;(move-ball(reverse-left-right (reverse-up-down b1))))

(check-expect (reverse-dir BALL-AT-LEFT BALL-AT-RIGHT)
              (reverse-if-greater-dy(reverse-left-right BALL-AT-LEFT) BALL-AT-RIGHT))

;ball ball -> ball
;helper function that reverses dx if b1's dx is greater
(define (reverse-if-greater-dx b1 b2)
  (cond
    [(>= (abs (ball-dx b1))(abs(ball-dx b2))) (reverse-left-right b1)]
    [else b1]))

(check-expect (reverse-if-greater-dx BALL-AT-TOP BALL-AT-BOTTOM) (reverse-left-right BALL-AT-TOP))
(check-expect (reverse-if-greater-dx BALL-AT-BOTTOM BALL-AT-TOP) BALL-AT-BOTTOM)


;helper function that reverses dy if b1's dy is greater
(define (reverse-if-greater-dy b1 b2)
  (cond
    [(>= (abs(ball-dy b1))(abs(ball-dy b2))) (reverse-up-down b1)]
    [else b1]))
(check-expect (reverse-if-greater-dy BALL-AT-TOP BALL-AT-BOTTOM) (reverse-up-down BALL-AT-TOP))
(check-expect (reverse-if-greater-dy BALL-AT-BOTTOM BALL-AT-TOP) BALL-AT-BOTTOM)
  
;ball ball -> boolean
;checks if the balls collide
(define (collide? b1 b2)
  (<= (sqrt (+ (expt(- (ball-x b1) (ball-x b2)) 2)
               (expt(- (ball-y b1) (ball-y b2)) 2)))
      (+ (ball-radius b1) (ball-radius b2))))

(check-expect (collide?
               (make-ball (circle (+ RADIUS 4) "solid" "teal")
             51 (/ HEIGHT 2) -4 4)
               (make-ball (circle (+ RADIUS 4) "solid" "teal")
             49 (/ HEIGHT 2) -4 4)) #t)
(check-expect (collide?
               (make-ball (circle 1 "solid" "teal")
             200 (/ HEIGHT 2) -4 4)
               (make-ball (circle 1 "solid" "teal")
             1 (/ HEIGHT 2) -4 4)) #f)

;ball lob -> lob
;checks if b collides with any other ball, and reverses b and other if true
(define (collide-reverse b lob)
  (cond
    [(empty? lob) (cons b lob)]
    [(cons? lob)
                (cond
                  [(collide? b (first lob))  (cons (reverse-dir (first lob) b)
                                                   (collide-reverse (reverse-dir b (first lob)) (rest lob)))]
                  [else (cons (first lob) (collide-reverse b (rest lob)))])]))

(check-expect (cons?(collide-reverse BALL-AT-LEFT INIT-LOB)) #t)
(check-expect (collide-reverse BALL-AT-TOP '())
              (cons BALL-AT-TOP '()))

;lob -> lob
;checks all balls against each other to see if they collide
(define (all-pos-collides lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (cons (first (collide-reverse (first lob) (rest lob)))
                       (all-pos-collides (rest (collide-reverse (first lob) (rest lob)))))]))

(check-expect (cons? (all-pos-collides INIT-LOB)) #t)
(check-expect (all-pos-collides '()) '())
  
; list-of-balls -> image 
; produces image of each ball at each given current position on
; background.
; (Yes, I provided this function for you! You shouldn't have to
;  touch it if you've correctly implemented the functions above.)
(define (render-balls lob) 
  (cond [(empty? lob) BACKGROUND]
        [else (render-ball (first lob)
                           (render-balls (rest lob)))]))

; Here's the main function with the big-bang expression!
; Once you've implemented move-list-of-balls, uncomment on-tick below.
(define (main w)
  (big-bang w
            (on-tick move-list-of-balls 1/28) 
            (to-draw render-balls)))

; Run program automatically, or type this in Interactions Pane:
; Use INIT-LOB as the initial state of the world...
(main INIT-LOB)