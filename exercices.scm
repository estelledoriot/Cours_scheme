;;; Définir la fonction squash qui prend une s-expression comme argument et retourne une liste _sans parenthèses internes_ de tous les atomes rencontrés dans la s-expression.
(define (squash liste)
    (cond 
        ((null? liste) liste)
        ((atom? liste) (list liste))
        (else (append (squash (car liste)) (squash (cdr liste))))
    )
)

;;; Définir la fonction union. L’union de deux ensembles étant l’ensemble de tous les éléments qui appartiennent soit à l’un, soit à l’autre, des deux ensembles.
(define (union set1 set2)
    (cond 
        ((null? set2) set1)
        ((not (member (car set2) set1)) 
            (union (cons (car set2) set1) (cdr set2))
        )
        (else (union set1 (cdr set2)))
    )
)

;;; Définir la fonction intersection. L’intersection de deux ensembles étant l’ensemble de tous les éléments qui appartiennent aux deux ensembles.
(define (intersection set1 set2)
    (define (aux set1 set2 res)
        (cond 
            ((null? set2) res)
            ((member (car set2) set1) 
                (aux set1 (cdr set2) (cons (car set2) res)))
            (else (aux set1 (cdr set2) res))
        )
    )
    (aux set1 set2 '())
)

;;; Après avoir spécifié le problème, écrire un jeu de tests et une définition Scheme de la fonction, nommée polynomiale, telle que (polynomiale a b c d x) rend la valeur de la fonction qui à x associe ax3 + bx2 + cx + d.
(define (polynomiale a b c d x)
    (+ (* a x x x) (* b x x) (* c x) d)
)

;;; Après avoir spécifié le problème, écrire un jeu de tests et une définition de la fonction polynomiale-carre qui rend la valeur de la fonction ax4 + bx2 + c.
(define (carre x)
    (* x x)
)

(define (polynomiale-carre a b c x)
    (+ (* a (carre (carre x))) (* b (carre x)) c)
)

;;; Écrire une définition Scheme du prédicat positif? qui, étant donné un nombre n, retourne vrai si, et seulement si, n est strictement positif.
(define (positif? n)
    (> n 0)
)

;;; Écrire une définition Scheme du prédicat nombre-positif? qui, étant donnée une valeur quelconque x, retourne vrai si, et seulement si, x est un nombre et s'il est strictement positif.
(define (nombre-positif? n)
    (and (number? n) (positif? n))
)

;;; En n'utilisant pas les prédicats prédéfinis even? et odd?, écrire une définition Scheme du prédicat nombre-pair? qui, étant donné un nombre entier, rend vrai si, et seulement si, ce nombre est pair.
(define (nombre-pair? n)
    (= (modulo n 2) 0)
)

;;; Écrire une définition Scheme du prédicat divise? qui, étant donnés un entier strictement positif m et un entier n rend vrai si, et seulement si, m divise n.
(define (divise? m n)
    (= (modulo n m) 0)
)

;;; Écrire une définition Scheme du semi-prédicat quotient-si-divise qui, étant donnés un entier strictement positif m et un entier n rend la division de n par m si m divise n et le booléen faux sinon.
(define (quotient-si-divise m n)
    (if (divise? m n)
        (/ n m)
        #f
    )
)

;;; Écrire une définition Scheme de la fonction mention qui calcule la mention correspondant à une note (sur 20) donnée. Par exemple :
;;; (mention 8.5) -+ "Eliminé"
;;; (mention 10) -+ "Passable"
;;; (mention 12.5) -+ "AB"
;;; (mention 15) -+ "B •
;;; (mention 16.5) -+ "TB"
(define (mention note)
    (cond 
        ((< note 10) "Eliminé")
        ((< note 12) "Passable")
        ((< note 14) "AB")
        ((< note 16) "B")
        (else "TB")
    )
)

;;; Écrire une définition de la fonction somme-des-chiffres qui rend la somme des chiffres d'un entier naturel n.
(define (somme-des-chiffres n)
    (if (< n 10)
        n
        (+ (somme-des-chiffres (floor (/ n 10))) (modulo n 10))
    )
)

;;; Écrire une définition du prédicat existe-chiffre? qui, étant donné un chiffre centre 0 et 9, et un entier naturel n, rend vrai si et seulement si le chiffre c apparaît dans l' écriture en base 10 de n. On supposera que l'écriture des nombres autres que zéro ne commence pas par le chiffre O.
(define (existe-chiffre? c n)
    (cond 
        ((= n 0) #f)
        ((= c (modulo n 10)) #t)
        (else (existe-chiffre? c (floor (/ n 10))))
    )
)

;;; Écrire une définition de la fonction somme-des-sommes qui, étant donné un entier naturel n, renvoie le nombre à un seul chiffre obtenu en calculant la somme des chiffres de n et en réitérant le procédé jusqu'à ce que le résultat ne comporte qu'un seul chiffre.
(define (somme-des-sommes n)
    (let ((somme (somme-des-chiffres n)))
        (if (< somme 10)
        somme
        (somme-des-sommes somme)
        )
    )
)

;;; Cet exercice utilise le schéma de récursion dichotomique, il montre l'intérêt du nommage et revient sur la notion de semi-prédicat. Écrire une définition du semi-prédicat puissance-de-2 qui, étant donné un entier positif n, rend #f si n n'est pas une puissance de 2 et sinon, rend l'entier k tel que n = 2^k.
(define (puissance-de-2 n)
    (define (aux n r)
        (cond 
            ((= n 1) r)
            ((not (= 0 (modulo n 2))) #f)
            (else (aux (/ n 2) (+ r 1)))
        )
    )
    (aux n 0)
)