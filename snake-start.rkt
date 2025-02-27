#lang racket
(require 2htdp/universe 2htdp/image)


(define SIZE 40) ; size * seg-size is used to place items on screen, so SIZE*SEG-SIZE should == WIDTH == HEIGHT
(define SEG-SIZE 10)  ; size of images
(define WIDTH 400)
(define HEIGHT 400)

(define TICK-RATE 0.1)
(define EXPIRATION-TIME 30)


(define ENDGAME-TEXT-SIZE 10)
(define GOO-IMG (bitmap/file "./assets/goo.png"))
(define SEG-IMG (bitmap/file "./assets/segment.png"))
(define HEAD-UP-IMG (bitmap/file "./assets/head-up.png"))
(define HEAD-DOWN-IMG (bitmap/file "./assets/head-down.png"))
(define HEAD-LEFT-IMG (bitmap/file "./assets/head-left.png"))
(define HEAD-RIGHT-IMG (bitmap/file "./assets/head-right.png"))

(struct pit (snake goos))
(struct snake (dir segs))
(struct goo (loc expire))
(struct posn (x y))

(define (posn=? p1 p2)
      (and (= (posn-x p1) (posn-x p2))
           (= (posn-y p1) (posn-y p2))))


;; goo
(define (age-goo goos)
  (rot (renew goos)))

(define (decay g)
  (goo (goo-loc g)(sub1 (goo-expire g))))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (renew goos)
  (cond [(empty? goos) empty]   ;; why check for empty here?
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))
(define (rotten? g)
  (zero? (goo-expire g)))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))


;; snake
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))
(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir  (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (snake-head sn)
  (first (snake-segs sn)))
(define (snake-body sn)
  (rest (snake-segs sn)))
(define (snake-tail sn)
  (last (snake-segs sn)))
(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

;; other
(define (next-pit w)
      (define snake (pit-snake w))
      (define goos (pit-goos w))
      (define goo-to-eat (can-eat snake goos))
      (if goo-to-eat
          (pit (grow snake) (age-goo (eat goos goo-to-eat)))
          (pit (slither snake) (age-goo goos))))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))
(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))
(define (snake+scene snake scene)
      (define snake-body-scene
        (img-list+scene  (snake-body snake) SEG-IMG scene))
      (define dir (snake-dir snake))
      (img+scene (snake-head snake)
                 (cond [(string=? "up" dir) HEAD-UP-IMG]
                       [(string=? "down" dir) HEAD-DOWN-IMG]
                       [(string=? "left" dir) HEAD-LEFT-IMG]
                       [(string=? "right" dir) HEAD-RIGHT-IMG])
                 snake-body-scene))
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))
(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

(define MT-SCENE
  (empty-scene WIDTH HEIGHT))

;; controls


(define (direct-snake w ke)
      (cond [(dir? ke) (world-change-dir w ke)]
            [else w]))

(define (dir? x)
      (or (key=? x "up")
          (key=? x "down")
          (key=? x "left")
          (key=? x "right")))

(define (world-change-dir w d)
      (define the-snake (pit-snake w))
      (cond [(and (opposite-dir? (snake-dir the-snake) d)
                  ;; consists of the head and at least one segment
                  (cons? (rest (snake-segs the-snake))))
             (stop-with w)]
            [else
             (pit (snake-change-dir the-snake d) (pit-goos w))]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))



;; end game

(define (dead? w)
  (define snake (pit-snake w))
    (or (self-colliding? snake) (wall-colliding? snake)))

(define (snake-length-message s)
  (string-append "Your snake was this long: " (number->string (length (snake-segs s)))))

(define (dead-reason w)
  (define snake (pit-snake w))
  (cond
    [(wall-colliding? snake) (string-append "Duh you hit a wall! " (snake-length-message snake))]
    [else (string-append "You collided with yourself! " (snake-length-message snake))]
    ))
  
(define (render-end w)
      (overlay (text (dead-reason w) ENDGAME-TEXT-SIZE "red")
               (render-snake-world w)))

(define (render-snake-world w)
  (render-pit w))

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))






;; BIG BANG
(define (start-snake)
      (big-bang (pit (snake "right" (list (posn 1 1)))
                     (list (fresh-goo)
                           (fresh-goo)
                           (fresh-goo)
                           (fresh-goo)
                           (fresh-goo)
                           (fresh-goo)))
                (on-tick next-pit TICK-RATE)
                (on-key direct-snake)
                (to-draw render-pit)
                (stop-when dead? render-end)))