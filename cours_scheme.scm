
; booléens
(boolean? #t)

(and #f #t)
(or (= 2 2) (= 3 (/ 5 0)))
(and (> 4 12) (= 3 (/ 5 0)))
(or (> 4 12) (= 3 (/ 5 0)))

; nombres
(number? 42) 
(number? #t) 
(complex? 2+3i) 
(real? 2+3i)
(real? 3.1416) 
(real? 22/7)
(real? 42)
(rational? 2+3i)
(rational? 3.1416) 
(rational? 22/7)
(integer? 22/7)
(integer? 42)

(+ 1 2 3)
(- 5.3 2) 
(- 5 2 1) 
(* 1 2 3)
(/ 6 3) 
(/ 22 7) 
(expt 2 3) 
(expt 4 1/2)
(add1 5)
(sub1 5)
(abs -10)
(modulo 10 3)

(- 4) 
(/ 4) 

(= 42 42) 
(= 42 42.0) 
(< 3 2) 
(>= 4.5 3) 

(max 1 3 4 2 3) 
(min 1 3 4 2 3)

; caractères
(char? #\c) 
(char? 1) 
(char? #\;)

(char=? #\a #\a)
(char<? #\a #\b) 
(char>=? #\a #\b)

; chaines de caractères
(string? "Apple")

; variables
(define taille 1.80)

; fonctions
(define (surface_cercle r) 
    (* pi (* r r))
)

(define (factorielle n)
    (if (= n 1)
        1
        (* n (factorielle (- n 1)))
    )
)

(surface_cercle 12)

((lambda (x) (* x x)) 5)

(symbol? 'xyz) 
(symbol? 42)

(define (aire-disque r)
    (let ((pi 3.1416))
        (* pi (carre r)) 
    ) 
)

; listes
(pierre jean marie) ; qui est une liste à trois éléments
(a (b c) (d)) ; une liste constituée d’un atome et de deux listes
(
    (roi-blanc (g5))
    (cavalier-blanc (g1))
    (pion-blanc (c6))
    (roi-noir (a7))
    (cavalier-noir (a8))
)

(list? '(a b c d))

(cons 'a '(b c))
(cons '+ '(1 2)) 
(cons '(New-York) '(Chicago Boston))

(append '(a b) '(c d)) 
(append '((a) (b)) '((c) (d))) 

(list 'lundi 'mardi 'mercredi) 
(list '(a b) 'c '(d e)) 

(car '(tea for two))
(car '((lundi mardi mercredi jeudi vendredi)(samedi dimanche)))

(cdr '(tea for two)) 
(cdr '((lundi mardi mercredi jeudi vendredi)(samedi dimanche))) 

(cddr '(1 2 c d e))
(caddr '(1 2 c d e))

(define y (list 1 2 3 4))
(list-ref y 0)
(list-ref y 3) 
(list-tail y 1) 
(list-tail y 3) 

(length y)
(member 2 y)
(reverse y)

; structures de controle
(define (absolue x)
    (if (>= x 0)
    x
    (- x ))  
)

(define (truc x)
    (cond 
        ((< x 0) (- x ))
        ((<= x 10) (* 2 x ))
        ((<= x 20) (/ x 3))
        (else (sqrt x)) 
    ) 
)

; fonctionnelles
(define (add2 x)
    (+ 2 x)
)
(map add2 '(1 2 3))
(map + '(1 2 3) '(10 20 30))
(apply + '(10 20 30))


(define (carre x) 
    (* x x) 
)
(define (plus1 x) 
    (+ 1 x) 
)
(define (compose f g)
    (lambda (x) (f (g x)))
)