#lang racket
(require 2htdp/universe 2htdp/image)
(struct interval (small big))

(define TEXT-SIZE 20)
(define TEXT-X 20)
(define TEXT-UPPER-Y 20)
(define TEXT-LOWER-Y 280)
(define WIDTH 600)
(define HEIGHT 300)
(define SIZE 50)
(define HELP-TEXT
(text "UP for larger numbers, DOWN for smaller ones"
      TEXT-SIZE
      "blue"))
(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))
(define COLOR "red")

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
(interval-big w)))

(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC))

(define (render-last-scene w)
  (overlay (text (string-append "I got it: " (number->string (guess w))) SIZE COLOR) MT-SC))

(define (single? w)
  (= (interval-small w) (interval-big w)))

(define (start lower upper)
  (big-bang (interval lower upper)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))