;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname falling-extensions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
The goal of this assignment is to extend your falling game.
In addition to the extensions listed below, find all
opportunities to abstract the functions that you have already written using map, filter,
and other such higher-order functions.

  1) make objects that would be touching the paddle, if
they were at the bottom of the screen, look different.
That is, help the player to understand the extent of the
paddle with a subtly different faller image. (This
applies to the new types of fallers you add for the
second part of the homework too.)

  2) make a new type of faller, such that, when it touches
the paddle, the paddle gets wider and another such that,
when it touches the paddle, the paddle gets narrower.
These fallers should appear at regular intervals (not
randomly) in your game. For example, every 10th faller
could be a shrinking faller,say. 
|#


;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          
;     ;;;;;;                                                               ;;;;            
;    ;;;;;;;;                                                              ;;;;            
;   ;;;;;;;;;;                                                                             
;   ;;;;  ;;;;  ;;;;   ;;;; ;;;; ;;;;      ;;;;     ;;;; ;;;      ;;;;;    ;;;;    ;;;;;   
;   ;;;;         ;;;   ;;;  ;;;;;;;;;;   ;;;;;;;;   ;;;;;;;;;    ;;;;;;;   ;;;;   ;;;;;;;  
;    ;;;;;;;     ;;;; ;;;;  ;;;;;;;;;;   ;;;  ;;;   ;;;;  ;;;;  ;;;;  ;;;  ;;;;  ;;;;  ;;; 
;     ;;;;;;;    ;;;; ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;       ;;;;  ;;;;      
;       ;;;;;;    ;;; ;;;   ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;   ;;;;;;;   ;;;;   ;;;;;;;  
;         ;;;;    ;;; ;;;   ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;    ;;;;;;;  ;;;;    ;;;;;;; 
;   ;;;;  ;;;;    ;;; ;;;   ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;       ;;;;  ;;;;       ;;;; 
;   ;;;;;;;;;;     ;;;;;    ;;;;  ;;;;   ;;;  ;;;   ;;;;  ;;;;  ;;;  ;;;;  ;;;;  ;;;  ;;;; 
;    ;;;;;;;;      ;;;;;    ;;;;  ;;;;   ;;;;;;;;   ;;;;;;;;;    ;;;;;;;   ;;;;   ;;;;;;;  
;     ;;;;;;       ;;;;;    ;;;;  ;;;;     ;;;;     ;;;; ;;;      ;;;;;    ;;;;    ;;;;;   
;                   ;;;                             ;;;;                                   
;                  ;;;;                             ;;;;                                   
;               ;;;;;;                              ;;;;                                   
;               ;;;;;                               ;;;;                                   
;                                                                                          
;                                                                                          


#|

For this exercise you will design and implement a
minimalistic, one finger input game. The player
controls a paddle that moves back and forth at the
bottom of the screen. Falling from the heavens are
some items that you're trying to capture on your
paddle. The paddle never stays still; it
continuously moves left and right along the bottom
of the screen.

There is only a single kind of input accepted
(think like a thumb tap on a phone); the tap
reverses the direction of the paddle. That is, if
there is no input, then the paddle moves from the
left edge to the right edge and then back to the
left edge, over and over. When the user taps, then
the paddle reverses direction even when it isn’t
at one of the edges. So, if the user wishes to
keep the paddle in one spot, they can tap
repeatedly.

The player gets 10 points for each falling item
that the paddle catches and loses one point each
time they tap to reverse direction, but the score
never goes below zero.

Use the world data definition given below; note
that there is some ambiguity in this definition.
For example, do the `Posn`s of the fallers
represent their centers or upper-left corners? You
will need to figure out issues like this one and
make sure your code is consistent.

Either way, you should use the center of the
faller to determine if it has fallen off of the
bottom or if it has hit the paddle.

|#

(require 2htdp/image)
(require 2htdp/universe)
(require math/number-theory)

;                                                
;                                                
;                                                
;                                                
;                                                
;                                                
;                                                
;   ;;;;;;;;                     ;               
;   ;;;;;;;;;                 ;;;;               
;   ;;;;;;;;;;                ;;;;               
;   ;;;;  ;;;;;     ;;;;;;   ;;;;;;;    ;;;;;;   
;   ;;;;   ;;;;    ;;;;;;;;  ;;;;;;;   ;;;;;;;;  
;   ;;;;   ;;;;   ;;;;  ;;;; ;;;;;;;  ;;;;  ;;;; 
;   ;;;;   ;;;;        ;;;;;  ;;;;         ;;;;; 
;   ;;;;   ;;;;    ;;;;;;;;;  ;;;;     ;;;;;;;;; 
;   ;;;;   ;;;;   ;;;;; ;;;;  ;;;;    ;;;;; ;;;; 
;   ;;;;  ;;;;;   ;;;;  ;;;;  ;;;;    ;;;;  ;;;; 
;   ;;;;;;;;;;    ;;;;  ;;;;  ;;;;    ;;;;  ;;;; 
;   ;;;;;;;;;     ;;;;;;;;;;  ;;;;;;  ;;;;;;;;;; 
;   ;;;;;;;;       ;;;; ;;;;   ;;;;;   ;;;; ;;;; 
;                                                
;                                                
;                                                
;                                                
;                                                
;                                                


; A Faller-world is
;   (make-fw Number Direction [List-of Fallers] Natural Natural Natural)
; interp.: if `a-fw` is a Faller-world then all of:
; - (fw-paddle a-fw) is the x coordinate of the paddle,
; - (fw-direction a-fw) gives which direction the paddle is moving,
; - (fw-fallers a-fw) is a list of the fallers, and
; - (fw-score a-fw) is the score.
; - (fw-width a-fw) is the width of paddle.
; - (fw-tick a-fw) is the number of tick since the game started.
(define-struct fw (paddle direction fallers score width ticks))

; A Direction is one of:
; - "left"
; - "right"

;; A faller is:
;; - (make-faller Posn FallerType)
(define-struct faller (p t))

;;a FallerType is one of:
;; - "shrinking"
;; - "widening"
;; - "normal"

; A Posn is (make-posn Real Real)
; (Note: `Real` means a real number, which excludes complex numbers.)

;; define template for faller-worl
#;
(define (faller-world fw ...)
  ... (fw-paddle fw) ...          ;x coordinate of the paddle (number)
  ... (fw-direction fw) ...       ;give direction of paddle (string)
  ... (fw-fallers fw) ...         ;list of fallers (list)
  ... (fw-score fw) ...           ;score (natural)
  ... (fw-width fw) ...           ;paddle's width (natural) 
  ... (fw-ticks fw) ...)          ;tick number (natural)


;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;      ;;;;;                                            ;                               ;              
;     ;;;;;;;;                                       ;;;;                            ;;;;              
;    ;;;;;;;;;                                       ;;;;                            ;;;;              
;   ;;;;; ;;;;;      ;;;;     ;;;; ;;;;     ;;;;;   ;;;;;;;    ;;;;;;    ;;;; ;;;;  ;;;;;;;    ;;;;;   
;   ;;;;   ;;      ;;;;;;;;   ;;;;;;;;;;   ;;;;;;;  ;;;;;;;   ;;;;;;;;   ;;;;;;;;;; ;;;;;;;   ;;;;;;;  
;   ;;;;           ;;;  ;;;   ;;;;;;;;;;  ;;;;  ;;; ;;;;;;;  ;;;;  ;;;;  ;;;;;;;;;; ;;;;;;;  ;;;;  ;;; 
;   ;;;;          ;;;;  ;;;;  ;;;;  ;;;;  ;;;;       ;;;;         ;;;;;  ;;;;  ;;;;  ;;;;    ;;;;      
;   ;;;;   ;;     ;;;;  ;;;;  ;;;;  ;;;;   ;;;;;;;   ;;;;     ;;;;;;;;;  ;;;;  ;;;;  ;;;;     ;;;;;;;  
;   ;;;;   ;;;;   ;;;;  ;;;;  ;;;;  ;;;;    ;;;;;;;  ;;;;    ;;;;; ;;;;  ;;;;  ;;;;  ;;;;      ;;;;;;; 
;   ;;;;; ;;;;;   ;;;;  ;;;;  ;;;;  ;;;;       ;;;;  ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;         ;;;; 
;    ;;;;;;;;;     ;;;  ;;;   ;;;;  ;;;;  ;;;  ;;;;  ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;    ;;;  ;;;; 
;     ;;;;;;;;     ;;;;;;;;   ;;;;  ;;;;   ;;;;;;;   ;;;;;;  ;;;;;;;;;;  ;;;;  ;;;;  ;;;;;;   ;;;;;;;  
;      ;;;;;         ;;;;     ;;;;  ;;;;    ;;;;;     ;;;;;   ;;;; ;;;;  ;;;;  ;;;;   ;;;;;    ;;;;;   
;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      


;; You will use these named constants in the
;; definitions of your functions to determine the
;; world’s dimensions and when fallers are created.
;; Your program should still work—with no other
;; changes—when these constants are adjusted (within
;; a reasonable range).
(define WORLD-WIDTH     200)   ; window width
(define WORLD-HEIGHT    300)  ; window height
(define MAX-FALLERS      20)    ; maximum faller count
(define INV-P-FALLER     25)   ; inverse of per-tick probability of new faller
(define PADDLE-WIDTH     40)
(define PADDLE-HEIGHT    10)
(define FALLER-WIDTH     10)
(define FALLER-HEIGHT    20)
(define FALLER-START-Y    -5)
(define BACKGROUND-COLOR   "Black")
(define PADDLE-COLOR       "White")
(define FALLER-COLOR       "Light Blue")
(define FALLER-COLOR2      "Red")
(define FALLER-COLOR3      "Yellow")
(define FALLER-COLOR4      "green")
(define PADDLE-X       100)
(define PADDLE-Y       290)
(define PADDLE-DIR     "right")
(define LIST-OF-POSN (list (make-posn 130 15) 
                           (make-posn 30 10)
                           (make-posn 70 25)
                           (make-posn 180 30)))
(define SCORE-X           180)
(define SCORE-Y            20)
(define SCORE-SIZE         10)
(define SCORE-COLOR  "orange")
(define SCORE-FALLER       10)
(define SCORE               0)
(define PADDLE-SPEED        1)
(define FALLER-SPEED        1)

(define F1  (make-faller (make-posn 130 15) "normal"))
(define F2  (make-faller (make-posn 30 10) "normal"))
(define F3  (make-faller (make-posn 70 25) "normal"))
(define F4  (make-faller (make-posn 180 30) "normal"))
(define LIST-OF-FALLER (list F1 F2 F3 F4))
(define TICK1               1)
(define THREE               3)
(define NINETY              90)
(define FIFTY-SEVEN         57)

; FALLER0 : faller-world
; The initial state of the faller world
(define FALLER0          (make-fw PADDLE-X PADDLE-DIR LIST-OF-FALLER SCORE PADDLE-WIDTH 1))



#|

For the first step, give your game some flavor.
Find or design an image to show as the falling
items and design an image to use as the paddle.
For the paddle, use `2htdp/image` to make an
image, but for the fallers you may find an image
online to include in your program (or you may
compose your own one using `2htdp/image`).
|#

#|
Make your falling image about 20 pixels tall and
20 pixels wide and make your paddle about 12
pixels tall and 50 pixels wide. Use `image-width`
and `image-height` to confirm the sizes.

Please DO NOT paste the image that you find
directly into your code because that makes version
control (Git) not work very well on the resulting
file. Instead, you should save the image as a file
in this directory and load it in your program
using the `bitmap/file` function. For example, if
you save your faller image as `faller.jpg` (in the
same directory as this file), then you can load it
like this:

  (define FALLER-IMAGE (bitmap/file "faller.jpg"))

In order to a new file like `faller.jpg` to be
committed to Git and uploaded to GitHub (so that
we can see it when grading), you need to use the
`git add` command, like so:

  $ git add faller.jpg

When you commit after `git add`, the file that you
added will be included in the commit.

|#

(define FALLER-IMAGE (add-solid-curve
                      (add-solid-curve
                       (rectangle FALLER-WIDTH FALLER-HEIGHT "solid" BACKGROUND-COLOR)
                       (/ FALLER-WIDTH 2) 0 180 1/10
                       (/ FALLER-WIDTH 2) FALLER-HEIGHT 0 1/2
                       FALLER-COLOR)
                      (/ FALLER-WIDTH 2) 0 0 1/10
                      (/ FALLER-WIDTH 2) FALLER-HEIGHT 180 1/2
                      FALLER-COLOR))

(define FALLER-IMAGE2 (add-solid-curve
                       (add-solid-curve
                        (rectangle FALLER-WIDTH (+ FALLER-HEIGHT 5) "solid" BACKGROUND-COLOR)
                        (/ FALLER-WIDTH 2) 0 180 1/10 
                        (/ FALLER-WIDTH 2) (+ FALLER-HEIGHT 10) 0 1/2
                        FALLER-COLOR2)
                       (/ FALLER-WIDTH 2) 0 0 1/10
                       (/ FALLER-WIDTH 2) (+ FALLER-HEIGHT 10) 180 1/2
                       FALLER-COLOR2))

(define FALLER-IMAGE3 (add-solid-curve
                       (add-solid-curve
                        (rectangle FALLER-WIDTH (+ FALLER-HEIGHT 5) "solid" BACKGROUND-COLOR)
                        (/ FALLER-WIDTH 2) 0 180 1/10 
                        (/ FALLER-WIDTH 2) FALLER-HEIGHT 0 1/2
                        FALLER-COLOR3)
                       (/ FALLER-WIDTH 2) 0 0 1/10
                       (/ FALLER-WIDTH 2) FALLER-HEIGHT 180 1/2
                       FALLER-COLOR3))
(define FALLER-IMAGE4 (add-solid-curve
                       (add-solid-curve
                        (rectangle FALLER-WIDTH (+ FALLER-HEIGHT 5) "solid" BACKGROUND-COLOR)
                        (/ FALLER-WIDTH 2) 0 180 1/10 
                        (/ FALLER-WIDTH 2) FALLER-HEIGHT 0 1/2
                        FALLER-COLOR4)
                       (/ FALLER-WIDTH 2) 0 0 1/10
                       (/ FALLER-WIDTH 2) FALLER-HEIGHT 180 1/2
                       FALLER-COLOR4))



(define BACKGROUND
  (rectangle WORLD-WIDTH WORLD-HEIGHT "solid" BACKGROUND-COLOR))

;                                                                                                   
;                                                                                                   
;                                                                                                   
;                                                                                                   
;                                                                                                   
;                                                                                                   
;                                                                                                   
;   ;;;;;;;;;                                          ;    ;;;;                                    
;   ;;;;;;;;;                                       ;;;;    ;;;;                                    
;   ;;;;;;;;;                                       ;;;;                                            
;   ;;;;        ;;;;  ;;;;  ;;;; ;;;;     ;;;;;;   ;;;;;;;  ;;;;     ;;;;     ;;;; ;;;;     ;;;;;   
;   ;;;;        ;;;;  ;;;;  ;;;;;;;;;;   ;;;;;;;;  ;;;;;;;  ;;;;   ;;;;;;;;   ;;;;;;;;;;   ;;;;;;;  
;   ;;;;;;;;    ;;;;  ;;;;  ;;;;;;;;;;   ;;;  ;;;; ;;;;;;;  ;;;;   ;;;  ;;;   ;;;;;;;;;;  ;;;;  ;;; 
;   ;;;;;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;    ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;      
;   ;;;;;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;        ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;   ;;;;;;;  
;   ;;;;        ;;;;  ;;;;  ;;;;  ;;;;  ;;;;        ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;    ;;;;;;; 
;   ;;;;        ;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;    ;;;;    ;;;;  ;;;;  ;;;;  ;;;;  ;;;;       ;;;; 
;   ;;;;        ;;;;;;;;;;  ;;;;  ;;;;  ;;;;  ;;;;  ;;;;    ;;;;   ;;;  ;;;   ;;;;  ;;;;  ;;;  ;;;; 
;   ;;;;        ;;;;;;;;;;  ;;;;  ;;;;   ;;;;;;;;   ;;;;;;  ;;;;   ;;;;;;;;   ;;;;  ;;;;   ;;;;;;;  
;   ;;;;         ;;;; ;;;;  ;;;;  ;;;;    ;;;;;;     ;;;;;  ;;;;     ;;;;     ;;;;  ;;;;    ;;;;;   
;                                                                                                   
;                                                                                                   
;                                                                                                   
;                                                                                                   
;                                                                                                   
;                                                                                                   

#|

There are three separate pieces of the game to be
implemented: rendering the world to an image to be
shown on the screen, handling user inputs, and
adjusting the world as time passes.

Here are the sigantures of the three functions:

  draw : Faller-world -> Scene

  key : Faller-world Key-event -> Faller-world

  tick : Faller-world -> Faller-world

`draw` and `key` are fairly well-described by the
description above, but `tick` needs a little more
explanation. Conceptually, it performs several
different tasks:

 1. Move all the fallers down the screen by one
pixel.

 2. If a faller touches the bottom of the screen,
remove it from the world; if it overlaps with the
paddle, also increment the score.

 3. Possibly add a new faller to the list,
starting from the top of the screen.

 4. Move the paddle.

Be sure to compose several different functions to
accomplish these tasks, and not just one
monolithic function that does it all! (And think
carefully about how you might break up the other
two main functions, too.)

Don't forget: the coordinate system has the
origin in the upper-left, with `x` coordinates
increasing rightward and `y` coordinates
increasing *downward*.

|#

; draw : Faller-world -> Scene
; Render the faller world in the scene
;
; Strategy: Struct Decomp

(define (draw fw)
  (place-images    
   (list (draw-paddle (fw-width fw) (fw-fallers fw) (fw-paddle fw))
         (draw-score (fw-score fw)))
   (list (make-posn (fw-paddle fw) PADDLE-Y)
         (make-posn SCORE-X SCORE-Y))  
   (draw-fallers (fw-fallers fw) (fw-paddle fw) BACKGROUND (fw-width fw)))
  )
 
; Examples:
(check-expect (draw (make-fw 30 "left" (list
                                        (make-faller (make-posn 150 150) "normal")
                                        (make-faller (make-posn 30 -30) "normal")
                                        (make-faller (make-posn 130 20) "normal")
                                        (make-faller (make-posn 70 -30) "normal")) 
                             2 40 0))
              (draw (make-fw 30 "left" (list
                                        (make-faller (make-posn 150 150) "normal")
                                        (make-faller (make-posn 30 -30) "normal")
                                        (make-faller (make-posn 130 20) "normal")
                                        (make-faller (make-posn 70 -30) "normal"))
                             2 40 0)))


(check-expect (draw (make-fw 50 "left" (list
                                        (make-faller (make-posn 150 -30) "normal")
                                        (make-faller (make-posn 70 10) "normal")
                                        (make-faller (make-posn 40 50) "normal")
                                        (make-faller (make-posn 80 130) "normal"))
                             2 40 0))
              (draw (make-fw 50 "left" (list
                                        (make-faller (make-posn 150 -30) "normal")
                                        (make-faller (make-posn 70 10) "normal")
                                        (make-faller (make-posn 40 50) "normal")
                                        (make-faller (make-posn 80 130) "normal"))
                             2 40 0)))



;; draw-paddle : Number LIST-OF-FALLER Number  -> Image
(define (draw-paddle width lof paddlex)
  (add-solid-curve
   (rectangle width PADDLE-HEIGHT "solid" PADDLE-COLOR)
   0 0 (- (- 40 (/ width 4))) 1/4
   width 0 (- 40 (/ width 4)) 1/4
   BACKGROUND-COLOR))

(check-expect (draw-paddle 40 (list
                               (make-faller (make-posn 150 -30) "normal")
                               (make-faller (make-posn 70 10) "normal")
                               (make-faller (make-posn 40 50) "normal")
                               (make-faller (make-posn 80 130) "normal")) 100)
              (add-solid-curve
               (rectangle 40 PADDLE-HEIGHT "solid" PADDLE-COLOR)
               0 0 (- (- 40 (/ 40 4))) 1/4
               40 0 (- 40 (/ 40 4)) 1/4
               BACKGROUND-COLOR))

;; helper function to change to width, maybe useful for the tick function 
#|

|#
; draw-fallers : List-of-Faller Number Image Number-> Image
; Render the fallers one at a time upon the background
;
; Strategy: Struct Decomp

(define (draw-fallers lof paddlex img width)
  (cond
    [(empty? lof) img]
    [else 
     (place-image
      (change-faller-image (faller-p (first lof)) paddlex width (faller-t (first lof)))  
      (posn-x (faller-p (first lof)))
      (posn-y (faller-p (first lof)))
      (draw-fallers (rest lof) paddlex img width))
     ]))

;; change-faller-image : POSN Number Number String-> Image
;; Check if the faller hit the paddle, if yes, change the faller image; otherwise,
;; remain the same
;;
;; Exmaples:
(check-expect (change-faller-image (make-posn 20 250) 20 40 "normal")
              FALLER-IMAGE2)
(check-expect (change-faller-image (make-posn 10 20) 20 40 "normal")
              FALLER-IMAGE2)
(check-expect (change-faller-image (make-posn 100 20) 20 40 "normal")
              FALLER-IMAGE)
(check-expect (change-faller-image (make-posn 20 20) 20 40 "shrinking")
              FALLER-IMAGE3)
(check-expect (change-faller-image (make-posn 20 20) 20 40 "widening")
              FALLER-IMAGE4)

;; Strategy: functional compsition
(define (change-faller-image ps paddlex width type) 
  (cond
    [(and
      (above-paddle? ps paddlex width)
      (string=? type "normal"))
     FALLER-IMAGE2]
    [(and
      (above-paddle? ps paddlex width)
      (string=? type "shrinking"))
     FALLER-IMAGE3]
    [(and
      (above-paddle? ps paddlex width)
      (string=? type "widening"))
     FALLER-IMAGE4]
    [else                     FALLER-IMAGE]))







;; draw-score : Number -> Image
;; To print the score in the screen
;;
;; Strategy: Struct Decomp
(define(draw-score score)
  (text (number->string score) SCORE-SIZE SCORE-COLOR))


; key : Faller-world Key-event -> world
; Respond to space key by changing directions of the paddle. Subtract 1 point each time
; press the key
;
; Strategy: Struct Decomp

(define (key fw input-key)
  (cond
    [(string=? input-key " ")
     (reverse-paddle-direction fw)]
    [else                   fw]))

; Examples:
(check-expect (key (make-fw 100
                            "right"
                            (list
                             (make-faller (make-posn 70 30) "normal")
                             (make-faller (make-posn 60 130) "normal"))
                            9
                            40
                            0)
                   " ")
              (make-fw 100
                       "left"
                       (list
                        (make-faller (make-posn 70 30) "normal")
                        (make-faller (make-posn 60 130) "normal"))
                       8
                       40
                       0))


;; reverse-paddle-direction : Faller-world -> Faller-world
;; To reverse the paddle direction
;;
;; Examples:
(check-expect (reverse-paddle-direction (make-fw 20
                                                 "left"
                                                 (list
                                                  (make-faller (make-posn 20 20) "normal")
                                                  (make-faller (make-posn 30 20) "normal"))
                                                 20
                                                 40
                                                 0))
              
              (make-fw 20
                       "right"
                       (list
                        (make-faller (make-posn 20 20) "normal")
                        (make-faller (make-posn 30 20) "normal"))
                       19
                       40
                       0))
(check-expect (reverse-paddle-direction (make-fw 20
                                                 "right"
                                                 (list
                                                  (make-faller (make-posn 20 20) "normal")
                                                  (make-faller (make-posn 30 20) "normal"))
                                                 0
                                                 40
                                                 0))
              (make-fw 20
                       "left"
                       (list
                        (make-faller (make-posn 20 20) "normal")
                        (make-faller (make-posn 30 20) "normal"))
                       0
                       40
                       0))

;; Strategy: Struct Decomp
(define (reverse-paddle-direction fw)
  (cond
    [(string=? (fw-direction fw) "right")
     (make-fw (fw-paddle fw)         
              "left"  
              (fw-fallers fw)         
              (decrement-score (fw-score fw))
              (fw-width fw)
              (fw-ticks fw))]
    [else
     (make-fw (fw-paddle fw)         
              "right"  
              (fw-fallers fw)         
              (decrement-score (fw-score fw))
              (fw-width fw)
              (fw-ticks fw))]))


;; decrement-score : Natural -> Natural
;; to decrement score
;;
;; Examples:
(check-expect (decrement-score 0) 0)
(check-expect (decrement-score 10) 9)

(define (decrement-score sc)
  (cond
    [(zero? sc) 0]
    [else
     (- sc 1)]))


;; tick : Faller-world -> Faller-world
;; Advances the game world state every tick
;;
;; Strategy: Functional Composition
(define (tick fw)
  (make-fw (get-paddle-x (fw-paddle fw) (fw-direction fw))
           (get-dir (fw-paddle fw) (fw-direction fw) (fw-width fw))
           (move-and-remove-fallers (maybe-add-faller (fw-fallers fw)) (fw-paddle fw) (fw-width fw) (fw-ticks fw))
           (get-score (fw-fallers fw) (fw-paddle fw) (fw-score fw) (fw-width fw))
           (change-width (fw-paddle fw) (fw-width fw) (fw-fallers fw)) 
           (+ TICK1 (fw-ticks fw))))  

; Examples
(check-expect (tick (make-fw
                     100
                     "left"
                     (list
                      (make-faller (make-posn 39 1) "normal")
                      (make-faller (make-posn 71 16) "normal")
                      (make-faller (make-posn 32 37) "normal")
                      (make-faller (make-posn 38 122) "normal")
                      (make-faller (make-posn 52 150) "normal")
                      (make-faller (make-posn 130 177) "normal")
                      (make-faller (make-posn 30 172) "normal")
                      (make-faller (make-posn 70 187) "normal")
                      (make-faller (make-posn 180 192) "normal"))
                   
                        
                     0
                     40
                     90))
              (make-fw
               99
               "left"
               (list
                (make-faller (make-posn 39 2) "shrinking")
                (make-faller (make-posn 71 17) "normal")
                (make-faller (make-posn 32 38) "normal")
                (make-faller (make-posn 38 123) "normal")
                (make-faller (make-posn 52 151) "normal")
                (make-faller (make-posn 130 178) "normal")
                (make-faller (make-posn 30 173) "normal")
                (make-faller (make-posn 70 188) "normal")
                (make-faller (make-posn 180 193) "normal"))
               0
               40
               91))

(check-expect (tick (make-fw
                     100
                     "left"
                     (list
                      (make-faller (make-posn 100 290) "shrinking")
                      (make-faller (make-posn 71 16) "normal")
                      (make-faller (make-posn 32 37) "normal")
                      (make-faller (make-posn 38 122) "normal")
                      (make-faller (make-posn 52 150) "normal")
                      (make-faller (make-posn 130 177) "normal")
                      (make-faller (make-posn 30 172) "normal")
                      (make-faller (make-posn 70 187) "normal")
                      (make-faller (make-posn 180 192) "normal"))
                   
                        
                     0
                     40
                     90)) 
              (make-fw
               99
               "left"
               (list
                (make-faller (make-posn 71 17) "shrinking")
                (make-faller (make-posn 32 38) "normal")
                (make-faller (make-posn 38 123) "normal")
                (make-faller (make-posn 52 151) "normal")
                (make-faller (make-posn 130 178) "normal")
                (make-faller (make-posn 30 173) "normal")
                (make-faller (make-posn 70 188) "normal")
                (make-faller (make-posn 180 193) "normal"))
               10
               37
               91))

(check-expect (tick (make-fw
                     100
                     "left"
                     (list
                      (make-faller (make-posn 100 290) "widening")
                      (make-faller (make-posn 71 16) "normal")
                      (make-faller (make-posn 32 37) "widening")
                      (make-faller (make-posn 38 122) "normal")
                      (make-faller (make-posn 52 150) "normal")
                      (make-faller (make-posn 130 177) "normal")
                      (make-faller (make-posn 30 172) "shrinking")
                      (make-faller (make-posn 70 187) "normal")
                      (make-faller (make-posn 180 192) "normal"))
                   
                        
                     0
                     40
                     57)) 
              (make-fw
               99
               "left"
               (list
                (make-faller (make-posn 71 17) "widening")
                (make-faller (make-posn 32 38) "widening")
                (make-faller (make-posn 38 123) "normal")
                (make-faller (make-posn 52 151) "normal")
                (make-faller (make-posn 130 178) "normal")
                (make-faller (make-posn 30 173) "shrinking")
                (make-faller (make-posn 70 188) "normal")
                (make-faller (make-posn 180 193) "normal"))
               10
               43
               58))



;; get-paddle-x : Number String -> Number
;; Adjusts the paddle's x coordinates based on direction string
;;
;; Strategy: 
(define (get-paddle-x px pd)
  (cond
    [(string=? pd "right") (+ PADDLE-SPEED px)]
    [else (- px PADDLE-SPEED)]))

; Examples
(check-expect (get-paddle-x 100 "right") (+ 100 PADDLE-SPEED))
(check-expect (get-paddle-x 100 "left") (- 100 PADDLE-SPEED)) 

;; get-dir : Number String Number-> String
;; Adjusts the paddle's direction if the paddle makes contact with world walls
;;
;; Strategy: 
(define (get-dir px pd pw)
  (cond
    [(>= px (- WORLD-WIDTH (/ pw 2))) "left"]
    [(<= px (/ pw 2))  "right"]
    [else pd])
  )

; Examples
(check-expect (get-dir 100 "right" 40) "right")
(check-expect (get-dir 180 "right" 40) "left")
(check-expect (get-dir 20 "left" 40) "right")

;; change-width : Number Number LIST-OF-FALLER -> width
;; change the width of paddle
(define (change-width paddlex width lof)
  (cond
    [(and (hit-paddle? (faller-p (first lof)) paddlex width)
          (string=? (faller-t (first lof)) "shrinking")
          (> width THREE))
    (- width THREE)] ;;narrower
    [(and (hit-paddle? (faller-p (first lof)) paddlex width)
          (string=? (faller-t (first lof)) "widening")
          (< width WORLD-WIDTH))
     (+ width THREE)] ;;widening
    [else 
     width])) 

(check-expect (change-width 100 40 (list (make-faller (make-posn 100 290) "shrinking")))
              37)
(check-expect (change-width 100 40 (list (make-faller (make-posn 100 290) "widening")))
              43)



;; get-lop : LIST-OF-FALLER Number Number Number -> LIST-OF-FALLER
;; given the LIST-OF-FALLER, and move falller one by one. If it touch the ground, remove the
;; item from the list and add new fallers
;; Strategy: Struct Decomp


(define (move-and-remove-fallers lof paddlex width ticks)
  (cond
    [(empty? lof) lof]
    [(hit-paddle? (faller-p (first lof)) paddlex width)
     (move-and-remove-fallers (filter-list lof paddlex width) paddlex width ticks)] ;; hit paddle
    [(>= (posn-y (faller-p (first lof))) (+ WORLD-HEIGHT 10))
     (move-and-remove-fallers (hit-ground-filter lof) paddlex width ticks)] ;; hit ground
    [(= (modulo ticks NINETY) 0)
     (cons (make-faller (make-posn (posn-x (faller-p (first lof)))
                                   (+ FALLER-SPEED (posn-y (faller-p (first lof))))) "shrinking")
           (move-and-remove-fallers (rest lof) paddlex width (+ 1 ticks)))] ;; add shrinking
    [(= (modulo ticks FIFTY-SEVEN) 0)
     (cons (make-faller (make-posn (posn-x (faller-p (first lof)))
                                   (+ FALLER-SPEED (posn-y (faller-p (first lof))))) "widening")
           (move-and-remove-fallers (rest lof) paddlex width (+ 1 ticks)))] ;;add widening
    [else
     (cons (make-faller (make-posn (posn-x (faller-p (first lof)))
                                   (+ FALLER-SPEED (posn-y (faller-p (first lof))))) (faller-t (first lof)))
           (move-and-remove-fallers (rest lof) paddlex width ticks))]))  ;; moving the fallers


;; Example:
(check-expect (move-and-remove-fallers '()
                                       120
                                       40
                                       3)
              '())
(check-expect (move-and-remove-fallers (list
                                        (make-faller (make-posn 100 290) "normal")
                                        (make-faller (make-posn 70 50) "normal"))
                                       100
                                       40
                                       3)
              (list (make-faller (make-posn 70 51) "normal")))
(check-expect (move-and-remove-fallers (list
                                        (make-faller (make-posn 100 310) "normal")
                                        (make-faller (make-posn 70 50) "normal"))
                                       100
                                       40
                                       3)
              (list (make-faller (make-posn 70 51) "normal")))
(check-expect (move-and-remove-fallers (list
                                        (make-faller (make-posn 100 200) "normal")
                                        (make-faller (make-posn 70 50) "normal"))
                                       100
                                       40
                                       90)
              (list (make-faller (make-posn 100 201) "shrinking")
                    (make-faller (make-posn 70 51) "normal")))

(check-expect (move-and-remove-fallers (list
                                        (make-faller (make-posn 100 200) "normal")
                                        (make-faller (make-posn 70 50) "normal"))
                                       100
                                       40
                                       57)
              (list (make-faller (make-posn 100 201) "widening")
                    (make-faller (make-posn 70 51) "normal")))




;; hit-ground-filter : LIST-OF-FALLER Number -> LIST-OF-FALLER
;; Removes fallers that make contact with the world ground, using the coordinates of the fallers
;;
;; Strategy: Struct Decomp

(define (hit-ground-filter lof)
  (cond
    [(>= (posn-y (faller-p (first lof))) (+ WORLD-HEIGHT 10))
     (rest lof)]
    [else
     lof]))

; Examples
(check-expect (hit-ground-filter (list
                                  (make-faller (make-posn 120 320) "normal")
                                  (make-faller (make-posn -20 130) "normal")))
              (list (make-faller (make-posn -20 130) "normal")))

(check-expect (hit-ground-filter (list
                                  (make-faller (make-posn 120 200) "normal")
                                  (make-faller (make-posn -20 130) "normal"))) 
              (list (make-faller (make-posn 120 200) "normal")
                    (make-faller (make-posn -20 130) "normal"))) 
;; hit-paddle? : POSN Number Number -> Boolean
;; Checks if the faller makes contact with the paddle using the faller's coordinates
;; and the paddle's x coordinate.
;;
;; Strategy: Function Composition

(define (hit-paddle? pn paddlex width)
  (and (<= (posn-x pn) (+ (/ width 2) paddlex))
       (>= (posn-x pn) (- paddlex (/ width 2)))
       (<= (posn-y pn) (+ (/ PADDLE-HEIGHT 2) PADDLE-Y))
       (>= (posn-y pn) (- PADDLE-Y (/ PADDLE-HEIGHT 2)))))


; Examples
(check-expect (hit-paddle? (make-posn 110 290) 10 40) #f)
(check-expect (hit-paddle? (make-posn 10 290) 10 40) #t)
(check-expect (hit-paddle? (make-posn 10 250) 10 40) #f)

;; above-paddle? : POSN Number Number  -> boolean
;; to determine if the line segment between x1 and x2 intersects with the line
;; segment between x3 and x4
(define (above-paddle? pn paddlex width)
  (and (<= (posn-x pn) (+ (/ width 2) paddlex))
       (>= (posn-x pn) (- paddlex (/ width 2)))))

(check-expect (above-paddle? (make-posn 100 30) 100 40) #t)
(check-expect (above-paddle? (make-posn 30 30) 100 40) #f)

;; filter-list : LIST-OF-FALLER Number Number -> LIST-OF-FALLER
;; Removes fallers that make contact with the paddle, using the coordinates of the fallers
;;
;; Strategy: Struct Decomp
(define (filter-list lof paddlex width)
  (cond
    [(hit-paddle? (faller-p (first lof)) paddlex width)
     (rest lof)]
    [else
     lof]))

; Examples
(check-expect (filter-list (list
                            (make-faller (make-posn 110 290) "normal")
                            (make-faller (make-posn -20 130) "normal"))
                           110
                           40)
              (list (make-faller (make-posn -20 130) "normal")))
(check-expect (filter-list (list
                            (make-faller (make-posn 110 200) "normal")
                            (make-faller (make-posn -20 130) "normal"))
                           110
                           40)
              (list (make-faller (make-posn 110 200) "normal")
                    (make-faller (make-posn -20 130) "normal")))
;; get-score :  LIST-OF-FALLER Number Number Number -> Number
;; If the faller touch the paddle, add 10 pts; otherwise, 0 pts
;;
;; Strategy: Struct Decomp
(define (get-score lof paddlex pts width)
  (cond
    [(empty? lof) pts]
    [(hit-paddle? (faller-p (first lof)) paddlex width) 
     (+ pts SCORE-FALLER)] 
    [else (get-score (rest lof) paddlex pts width)]))  

; Examples
(check-expect (get-score '() 100 10 40) 10)
(check-expect (get-score (list (make-faller (make-posn 110 290) "normal")
                               (make-faller (make-posn -20 130) "normal"))
                         100
                         0
                         40) 10)
(check-expect (get-score (list (make-faller (make-posn 110 200) "normal")
                               (make-faller (make-posn -20 130) "normal"))
                         100
                         0
                         40) 0)

;                                               
;                                               
;                                               
;                                               
;                                               
;                                               
;                                               
;   ;;;;    ;;;;               ;;;;             
;   ;;;;    ;;;;               ;;;;             
;   ;;;;    ;;;;               ;;;;             
;   ;;;;    ;;;;      ;;;;;    ;;;;  ;;;; ;;;   
;   ;;;;    ;;;;    ;;;;;;;;   ;;;;  ;;;;;;;;;  
;   ;;;;;;;;;;;;    ;;;  ;;;   ;;;;  ;;;;  ;;;; 
;   ;;;;;;;;;;;;   ;;;;  ;;;;  ;;;;  ;;;;  ;;;; 
;   ;;;;;;;;;;;;   ;;;;;;;;;;  ;;;;  ;;;;  ;;;; 
;   ;;;;    ;;;;   ;;;;;;;;;;  ;;;;  ;;;;  ;;;; 
;   ;;;;    ;;;;   ;;;;        ;;;;  ;;;;  ;;;; 
;   ;;;;    ;;;;    ;;;  ;;;;  ;;;;  ;;;;  ;;;; 
;   ;;;;    ;;;;    ;;;;;;;;   ;;;;  ;;;;;;;;;  
;   ;;;;    ;;;;     ;;;;;;    ;;;;  ;;;; ;;;   
;                                    ;;;;       
;                                    ;;;;       
;                                    ;;;;       
;                                    ;;;;       
;                                               
;                                               


;; In your `tick` function, you need to
;; *sometimes* add a faller to the list of
;; fallers. Use a function like `maybe-add-faller`
;; (below) to (randomly) add a faller to the
;; current list. You may wish to adjust it based
;; on gameplay factors or the way you interpret
;; `Posn`s as fallers. Note that because of the
;; randomness, this function is difficult to test
;; using `check-expect`, so the test example given
;; below just checks the length of the resulting
;; list.

; maybe-add-faller : List-of-FALLER -> List-of-FALLER
; Adds a random faller with probabilty
; `1/INV-P-FALLERS`, but only if there are fewer than `MAX-FALLERS`
; fallers aleady.
;
; Strategy: Decision Tree
(define (maybe-add-faller fallers)
  (cond
    [(< (length fallers) MAX-FALLERS)
     (cond
       [(> 1 (random INV-P-FALLER))
        (cons (make-faller (make-posn (+(random (* WORLD-WIDTH .8)) 20) FALLER-START-Y) "normal")
              fallers)]
       [else fallers])]
    [else fallers]))

; Examples:
(check-expect
 (<= 4
     (length
      (maybe-add-faller
       (list (make-faller (make-posn 0 0) "normal")
             (make-faller (make-posn 1 1) "normal")
             (make-faller (make-posn 2 2) "normal")
             (make-faller (make-posn 3 3) "normal"))))
     )
 #true)

(check-expect
 (<= 1
     (length
      (maybe-add-faller
       (list '())))
     )
 #true)

(check-expect
 (<= 2
     (length
      (maybe-add-faller
       (list (make-faller (make-posn 0 0) "normal")
             (make-faller (make-posn 1 1) "normal"))))
     )
 #true)




;; You'll use this `start` function to start your
;; faller game once you’ve completed designing the
;; three main handler functions that it depends
;; on.

; start : Any -> Faller-world
; Starts the faller game. (Argument is ignored.)
;
; Example:
;  - (start 0)
#; ;; [remove this line to uncomment the function]

#|
(define (start _dummy)
  (big-bang (make-fw (/ WORLD-WIDTH 2) "right" '() 0)
    [on-tick tick 1/200]
    [on-key  key]
    [to-draw draw]))
|#

(big-bang FALLER0
  [on-tick tick 1/200]
  [on-key  key]
  [to-draw draw])
