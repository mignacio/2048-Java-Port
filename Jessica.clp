;Brian Moex, Ignacio Moya. FCEIA-2014
(import nc.player.*)

(deftemplate Grilla (declare (from-class Grilla)))
(deftemplate Baldosa (slot i) (slot j) (slot valor))
(deftemplate Mov (slot dir)(slot puntaje))
(deftemplate Estrategia (slot valor))

(assert (Estrategia (valor 0)))
;valor=0, el programa trata de crear una baldoza con el mayor puntaje posible
;valor=1, el programa intenta liberar baldozas, suma puntaje en base a que dirección tiene la mayor cantidad de fusiones posibles

(defglobal ?*Arr* = (assert (Mov (dir 0)(puntaje 0))));arriba
(defglobal ?*Der* = (assert (Mov (dir 1)(puntaje 0))));derecha
(defglobal ?*Abj* = (assert (Mov (dir 2)(puntaje 0))));abajo
(defglobal ?*Izq* = (assert (Mov (dir 3)(puntaje 0))));izquierda
(defglobal ?*Dir* = 0)

(defrule ReglaMayor
    (declare (salience 1))
    ?m <- (Grilla (vector ?g) (accion ?mov))
    =>
    (bind ?j 0)
    (while (< ?j 4)
        (bind ?i 0)
        (while (< ?i 4)
            (bind ?v (call ?g elementAt (+ ?i (* 4 ?j))))
            (assert (Baldosa (j ?j)(i ?i)(valor ?v)))
            (printout t ?v)
            (++ ?i))
        (printout t crlf)
        (++ ?j))
    
    )


;estas reglas chequean si una baldoza tiene otra del mismo valor adyacente a ella
;en las 4 direcciones y suman puntaje a cada mov acorde
(defrule AdyDer
    
    ?b <- (Baldosa (i ?i1&:(< ?i1  3))(j ?jj)(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i =(+ ?i1 1))(j ?jj)(valor ?v2&:(eq ?v1 ?v2)))
    ;?dir <- (Mov (dir Derecha))
    =>
    ;suma puntaje a Der
    (modify ?*Der* (puntaje (+ ?*Der*.puntaje (* ?v1 2))))

    )

(defrule AdyAbj

    ?b <- (Baldosa (i ?i1)(j ?j1&:(< ?j1 3))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i ?i1)(j =(+ ?j1 1))(valor ?v2&:(eq ?v2 ?v1)))
    =>
    ;suma puntaje a Abj
    (modify ?*Abj* (puntaje (+ ?*Abj*.puntaje (* ?v1 2))))

    )

(defrule AdyArr

    ?b <- (Baldosa (i ?i1)(j ?j1&:(> ?j1 0))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i ?i1)(j =(- ?j1 1))(valor ?v2&:(eq ?v2 ?v1)))
    =>
    ;suma puntaje a Arr
    (modify ?*Arr* (puntaje (+ ?*Arr*.puntaje (* ?v1 2))))

    )

(defrule AdyIzq
    
    ?b <- (Baldosa (i ?i1&:(> ?i1 0))(j ?j1)(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i =(- ?i1 1))(j ?j1)(valor ?v2&:(eq ?v2 ?v1)))
    =>
    ;suma puntaje a Izq
    (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje (* ?v1 2))))
    
    )


(defrule AbjVsArr
    (declare (salience -1))
    (Mov (dir 2)(puntaje ?p1));abajo
    (Mov (dir 0)(puntaje ?p2));arriba
    =>
    (if (>= ?p1 ?p2)then 
        (modify ?*Abj* (puntaje 0))
        else
        (modify ?*Arr* (puntaje 0)))
    )

(defrule DerVsIzq
    (declare (salience -1))
    (Mov (dir 1)(puntaje ?p1));derecha
    (Mov (dir 3)(puntaje ?p2));izquierda
    =>
    (if (>= ?p1 ?p2)then
        (modify ?*Izq* (puntaje 0))
        else
        (modify ?*Der* (puntaje 0))
        )
    )
(defrule Accion
    (declare (salience -2))
    ?m1<-(Mov (puntaje ?p1&:(neq ?p1 0)))
    ?m2<-(Mov (puntaje ?p2&:(neq ?p2 0)))
    =>
    (if (>= ?p1 ?p2)then
        (bind ?*Dir* ?m1.dir)
        else
        (bind ?*Dir* ?m2.dir)
        )
    )

(defrule ReglaMenor
    (declare (salience -3))
    ?m <- (Grilla (vector ?g) (accion ?mov))
    =>
    
    (printout t "Puntaje Der  :" ?*Der*.puntaje crlf)
    (printout t "Puntaje Abajo:" ?*Abj*.puntaje crlf)
    (printout t "Puntaje Izq. :" ?*Izq*.puntaje crlf)
    (printout t "Puntaje Arr. :" ?*Arr*.puntaje crlf)
    
    (modify ?m (accion ?*Dir*))
    (printout t crlf ?*Dir* " Presione una tecla para continuar")
    (readline t)
    
    )
