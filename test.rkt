#lang racket (require 2htdp/image)
; Z każdym uruchomieniem programu kolor nieba jest inny.

; funkcja tworząca kolor:
(define (odcien R G B)
  (make-color R G B))

; funkcja tworząca psudogradient:
(define (f_odcienie wysokosc_odcienia R G B)
  (rectangle 800 wysokosc_odcienia "solid" (odcien R G B)))
                               
(define (gradient ilosc_odcieni R G B wysokosc_odcienia)
  (cond
   [(< ilosc_odcieni 1) (rectangle 800 1 "solid" (make-color 103 97 71) )]                             
   [else
    (above
      (f_odcienie wysokosc_odcienia R G B)
        (gradient ( - ilosc_odcieni 1) (+ R 20) (+ G 8) (+ B 7) (- wysokosc_odcienia 4)))]))

; generowanie randomowych wartosci R, G, B w definicjach
(define random_red_value (random 55))
(define random_green_value (random 75))
(define random_blue_value (random 85))

; definicje funkcji rysującej, nieba, trawy i ziemii
(define (rect x y) (rectangle 800 x "solid" y))
(define sky (gradient 9 random_red_value random_green_value random_blue_value 40))
(define grass (rect 10 (make-color 103 97 71)))
(define ground (rect 12 (make-color 96 79 71)))

; definicja pojedynczej gory, nie moze byc wyzsza od 150 i niższa od 20
(define (tri a kolor) (cond
   [(> a 150) (triangle (random 150) "solid" kolor)]
   [(< a 20) (triangle (random 35) "solid" kolor)]
   [else (triangle (random a) "solid" kolor)]))

; definiowanie funkcji generujacej rozne randomowe góry w czymś na kształt pętli
(define (gory n kolor)
  (cond
   [(< n 1) (triangle 20 "solid" (make-color 130 88 103))]
   [else (beside/align "bottom" (tri (expt 2 n) kolor) (gory (- n 1) kolor))]))

; wyswietlanie obrazkow
(above (above
        (overlay/align "center" "bottom"
          (gory 10 (make-color 84 63 80))
        (overlay/align "center" "bottom"
          (gory 11 (make-color 113 93 96))             
        (overlay/align "center" "bottom"
              (gory 12 (make-color 137 106 120))
              sky))) grass) ground)
