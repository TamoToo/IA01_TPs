; EXERCICE 1

(defun nombres3 (L)
              (if (and (numberp (car L)) (numberp (cadr L)) (numberp (caddr L)))
                  'BRAVO
                'PERDU))

(defun grouper (L1 L2)
              (if (and (car L1) (car L2))
                  (cons (cons (car L1) (list (car L2))) (grouper (cdr L1) (cdr L2)))
                NIL))

(defun grouper (L1 L2)
  (mapcar #'list L1 L2))


(defun monReverse (L)
               (if (car L)
                   (append (monReverse (cdr L)) (list (car L)))))

(defun palindrome (L)
  (if L
      (equal L (monReverse L))
    T)
  )



; EXERCICE 2

(defun list-triple-couple (L)
               (mapcar #'(lambda (x) (list x (* x 3))) L))

; EXERCICE 3

(defun my-assoc (cle a-list)
               (if (car a-list)
                   (if (eq cle (caar a-list))
                       (car a-list)
                     (my-assoc cle (cdr a-list)))
                 NIL))

(defun cles (a-list)
    (mapcar #'car a-list))


(defun creation (listeCles listeValeurs)
  (mapcar #'list listeCles listeValeurs))


; EXERCICE 4

(defvar BaseTest
'(("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles"))
  ("Conquête de la Thuringe" 531 531 (("Royaume Franc") ("Thuringes")) ("Thuringe"))
  ("Guere des Goths" 535 553 (("Royaume ostrogoth") ("Empire byzantin")) ("Péninsule italienne"))
  ("Conquête de l'Alémanie" 536 536 (("Royaume Franc") ("Alemans")) ("Alémanie"))
  ("Conquête de la Bavière" 555 555 (("Royaume Franc") ("Bavarii")) ("Bavière"))
  ("Campagnes de Bretagne" 560 578 (("Royaume Franc") ("Royaume du Vannetais")) ("Vannetais"))
  ("Guère franco-frisonne" 600 793 (("Royaume Franc") ("Royaume de Frise")) ("Pays-bas" "Allemagne"))
  ("Guerre civile des Francs" 715 719 (("Neustrie") ("Austrasie")) ("Royaume Franc"))
  ("Invasion omeyyade en France" 719 759 (("Royaume Franc") ("Califat omeyyade")) ("Royaume d'Aquitaine" "Septimanie"))
  ("Guerre des Lombards" 755 758 (("Royaume Franc") ("Lombards")) ("Lombardie"))
  ("Guerre d'Aquitaine" 761 768 (("Royaume Franc") ("Aquitains")) ("Vasconie Aquitaine"))
  ("Guerre des Saxons" 772 804 (("Royaume Franc") ("Saxons")) ("Germanie"))
  ("Guerre des Lombards" 773 774 (("Royaume Franc") ("Lombards")) ("Lombardie"))
  ("Guerre des Avars" 791 805 (("Royaume de France") ("Avars")) ("Pannonie"))
  ("Invasion sarrasines en Provence" 798 990 (("Royaume de France" "Comté de Provence") ("Sarrasins")) ("Provence"))
  ("Guerre civile entre les fils de Louis le Pieux" 830 842 (("Francie occidentale" "Francie orientale") ("Francie médiane")) ("Fontenoy"))
  ("Guerre franco-bretonne" 843 851 (("Royaume de France") ("Royaume de Bretagne" "Vikings")) ("Royaume de Bretagne"))
  ("Luttes inter-dynastiques carolingiennes" 876 946 (("Francie occidentale" "Francie orientale") ("Royaume de Bourgogne" "Francie orientale")) ("Ardennes" "Saône-et-Loire" "Rhénanie-Palatinat" "Aisne"))
  ("Invasions vikings en France" 799 1014 (("Royaume de France") ("Vikings")) ("Normandie" "Bretagne"))
  ("Première croisade" 1096 1099 (("Comté de Blois" "Comté de Toulouse" "Comté de Boulogne" "Marquisat de Provence" "Comté de Flandre" "Duché de Normandie" "Diocèse du Puy-en-Velay" "Comté de Vermandois" "République de Gênes" "Duché de Basse-Lotharingie" "Principauté de Tarente" "Empire byzantin" "Royaume de Petite-Arménie" "Croisés") ("Sultanat de Roum" "Danichmendides" "Califat fatimide")) ("Terre sainte"))
  ))



(defun dateDebut (conflit)
  (cadr conflit))

(defun nomConflit (conflit)
  (car conflit))

(defun allies (conflit)
  (car (cadddr conflit)))

(defun ennemis (conflit)
  (cadr (cadddr conflit)))

(defun lieu (conflit)
  (car (cddddr conflit)))


;;; RECURSIF
(defun FB1 (Liste)
              (if (car Liste)
                  (progn
                    (print (car Liste))
                    (FB1 (cdr Liste))
                    )))

(defun FB2 (Liste)
                  (if (car Liste)
                    (progn
                      (let ((rf "Royaume Franc"))
                        (if (or (member rf (allies (car Liste)) :test #'equal) (member rf (ennemis (car Liste)) :test #'equal)) ;;; On vérifie le cas où Royaume Franc se trouverait dans la liste en tant qu'ennemis même si ce n'est pas le cas ici.
                            (print (car Liste)))
                        (FB2 (cdr Liste))
                        ))))

(defun FB2 (Liste)
                  (if (car Liste)
                    (progn
                      (let ((rf "Royaume Franc"))
                        (if (member rf (allies (car Liste)) :test #'equal)
                            (print (car Liste)))
                        (FB2 (cdr Liste))
                        ))))




(defun FB3 (Liste ally)
              (if (car Liste)
                    (if (member ally (allies (car Liste)) :test #'equal)
                        (cons (car Liste) (FB3 (cdr Liste) ally))
                      (FB3 (cdr Liste) ally))
                    ))

(defun FB4 (Liste)
              (if (car Liste)
                  (let ((date 523))
                    (if (= date (dateDebut (car Liste)))
                        (car Liste)
                      (FB4 (cdr Liste)))
                      )))

(defun FB5 (Liste)
                (if (car Liste)
                      (let ((actual_date (dateDebut (car Liste))))
                        (if (and (>= actual_date 523) (<= actual_date 715))
                            (cons (car Liste) (FB5 (cdr Liste)))
                          (FB5 (cdr Liste)))
                        )))

(defun FB6 (Liste)
                (if (car Liste)
                    (progn
                      (let ((ennemy "Lombards"))
                        (if (member ennemy (ennemis (car Liste)) :test #'equal)
                            (+ 1 (FB6 (cdr Liste)))
                          (FB6 (cdr Liste)))
                        ))
                  0))



;;; ITERATIF

(defun FB1 (Liste)
  (dolist (l Liste)
    (print l)))

(defun FB2 (Liste)
               (let ((rf "Royaume Franc"))
                 (dolist (l Liste)
                   (if (or (member rf (allies l) :test #'equal) (member rf (ennemis l) :test #'equal))
                       (print l))
                    )))

(defun FB3 (Liste ally)
               (loop for l in Liste
                     unless (NULL (member ally (allies l) :test #'equal))
                   collect l
                     ))

(defun FB4 (Liste)
                (let ((date 523))
                  (loop for l in Liste
                     unless (/= date (dateDebut l))
                   collect l
                     )))

(defun FB5 (Liste)
                  (loop for l in Liste
                     unless (or (< (dateDebut l) 523) (> (dateDebut l) 715))
                   collect l
                     ))

(defun FB6 (Liste)
                (let ((ennemy "Lombards") (x 0))
                  (dolist (l Liste)
                    (if (member ennemy (ennemis l) :test #'equal)
                        (incf x))
                     )
                  x))


; JEUX DE DONNEES

(nombres3 '( 1 2 3 R S 4))

(grouper '(1 2 3) '(4 5 6))

(monReverse '(1 2 3 4 5))

(palindrome '(x a m a x) )

(list-triple-couple '(0 2 3 11))

(my-assoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45)))

(cles '((Yolande 25) (Pierre 22) (Julie 45)))

(creation '(Yolande Pierre Julie) '(25 22 45))

(dateDebut '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles")))

(nomConflit '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles")))

(allies '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles")))

(ennemis '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles")))

(lieu '("Guerre de Burgondie" 523 533 (("Royaume Franc") ("Royaume des Burgondes")) ("Vezeronce" "Arles")))

(FB1 BaseTest)

(FB2 BaseTest)

(FB3 BaseTest "Royaume de France")

(FB4 BaseTest)

(FB5 BaseTest)

(FB6 BaseTest)
