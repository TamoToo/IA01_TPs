;TP02

(setq actions '((16 3 2 1) (15 3 2 1) (14 3 2 1) (13 3 2 1) (12 3 2 1) (11 3 2 1) (10 3 2 1) (9 3 2 1) (8 3 2 1) (7 3 2 1) (6 3 2 1) (5 3 2 1) (4 3 2 1) (3 3 2 1) (2 2 1) (1 1)))


;; Résolution 1

;;; fonction qui donne tous les successeurs d'un etat :
(defun successeurs (allumettes actions)
  (cdr (assoc allumettes actions)))

;;; EXPLORE 
(defun explore (allumettes actions joueur i)
  (cond 
   ((and (eq joueur 'humain) (eq allumettes 0)) nil) 
   ((and (eq joueur 'IA) (eq allumettes 0)) t)    
    (t (progn                             
         (let ((sol nil) (coups (successeurs allumettes actions)))
           (cond
              ((and (eq joueur 'IA) (eq allumettes 3)) (pop coups))
              ((and (eq joueur 'IA) (eq allumettes 2)) (pop coups)))
             (while (and coups (not sol)) 
               (progn
                 (format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur (car coups) (- allumettes (car coups)))    
                 (setq sol (explore (- allumettes (car coups)) actions (if (eq joueur 'IA) 'humain 'IA) (+ i 3)))  
                 (if sol           
                     (setq sol (car coups))) 
                 (format t "~%~V@t sol = ~s~%" i sol)
                 (pop coups)
                 )  
               )
             sol)))))
           
(defvar nbCoupsAJouer nil)
(setq nbCoupsAJouer (explore 16 actions 'IA 0))
(setq nbCoupsAJouer (explore 8 actions 'IA 0))
(setq nbCoupsAJouer (explore 1 actions 'IA 0))



;; Résolution 2

(defun Randomsuccesseurs (actions) 
  (let ((r (random (length actions))))
    ;;(format t "~&~2t Res du random ~s~&" r)
    (nth r actions)))

;;; Question 2:
(defun JeuJoueur (allumettes actions)
  (format t "~%Il reste ~s allumettes. Combien voulez-vous en retirez ? " allumettes)
  (let ((stdin (read)) (succ (successeurs allumettes actions)))
    (while (not (member stdin succ))
      (format t "Impossible, veuillez rentrer un nouveau nombre : ")
      (setq stdin (read))
      )
    ;(format t "Vous avez décidé de retirer ~s allumetes" stdin)
    stdin
    )
  )

;;; Question 3:
(defun explore-renf (allumettes actions joueur)
  (cond 
   ((and (eq joueur 'humain) (eq allumettes 0)) 
    (format t "~%L'IA a perdu")
    nil) 
   ((and (eq joueur 'IA) (eq allumettes 0)) 
    (format t "~%L'IA a gagné")
    actions)    
   (t (progn
        (let ((coup NIL))
          (if (eq joueur 'humain)
              (setq coup (JeuJoueur allumettes actions))
            (setq coup (Randomsuccesseurs (successeurs allumettes actions))))
          (format t "~%Il y a ~s allumette(s), Joueur ~s tire ~s allumette(s) => il reste ~s allumette(s) " allumettes joueur coup (- allumettes coup))
          (explore-renf (- allumettes coup) actions (if (eq joueur 'IA) 'humain 'IA))
          )))))
        

(explore-renf 16 actions 'IA)


;;; Question 5:
(defun renforcement (allumettes coup actions)
  (push coup (cdr (assoc allumettes actions)))
  actions)


;;; Question 6 (Fonction finale):
(defun explore-renf-rec (allumettes actions joueur chemin)
  (cond 
   ((and (eq joueur 'humain) (eq allumettes 0)) 
    (format t "~%L'IA a perdu")
    nil) 
   ((and (eq joueur 'IA) (eq allumettes 0))
    (progn
      (format t "~%L'IA a gagné")
      (dolist (x chemin) ; Pour chaque coup joué par l'IA, on l'ajoute à la liste d'actions
        (renforcement (car x) (cadr x) actions))) ; (car x) = nb d'allumettes au moment du coup, (cadr x) = coup joué
      actions)    
   (t (progn
        (let ((coup NIL))
          (if (eq joueur 'IA)
              (progn
                (setq coup (Randomsuccesseurs (successeurs allumettes actions))) ; successeur aléatoire dans la liste d'actions
                (push (list allumettes coup) chemin)) ; ajout du coup joué par l'IA
            (setq coup (JeuJoueur allumettes actions))) ; lecture du coup de l'utilisateur
          (format t "~%Il y a ~s allumette(s), Joueur ~s tire ~s allumette(s) => il reste ~s allumette(s) " allumettes joueur coup (- allumettes coup))
          (explore-renf-rec (- allumettes coup) actions (if (eq joueur 'IA) 'humain 'IA) chemin)
          )))))

(explore-renf-rec 16 actions 'IA ())







