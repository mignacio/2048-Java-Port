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
(defglobal ?*baldLibre* = 0)

(defrule ReglaMayor
    (declare (salience 2))
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
;esta regla cuenta cuantas baldosas libres hay en la grilla
(defrule BaldLibres
    (declare (salience 1))
    (Baldosa (valor 0))
    =>
    (bind ?*baldLibre* (+ ?*baldLibre* 1))
    )


;estas reglas chequean si una baldoza tiene otra del mismo valor adyacente a ella
;en las 4 direcciones y suman puntaje a cada mov acorde


(defrule AdyDer
    
    (Baldosa (i ?i1&:(< ?i1  3))(j ?j1)(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i =(+ ?i1 1))(j ?j2&:(eq ?j1 ?j2))(valor ?v2&:(eq ?v1 ?v2)))
    ;?dir <- (Mov (dir Derecha))
    =>
    ;suma puntaje a Der
    (if(> ?*baldLibre* 3)then
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje (* ?v1 2))))
        else
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje 1)))
        
        )
    )

(defrule AdyIzq
    
    (Baldosa (i ?i1&:(> ?i1 0))(j ?j1)(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i =(- ?i1 1))(j ?j2&:(eq ?j1 ?j2))(valor ?v2&:(eq ?v2 ?v1)))
    =>
    ;suma puntaje a Izq
    (if(> ?*baldLibre* 3)then
        (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje (* ?v1 2))))
        else
        (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje 1)))
        )
    )

(defrule AdyAbj
    
    (Baldosa (i ?i1)(j ?j1&:(< ?j1 3))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i ?i2&:(eq ?i1 ?i2))(j =(+ ?j1 1))(valor ?v2&:(eq ?v2 ?v1)))
    =>
    ;suma puntaje a Abj
    (if(> ?*baldLibre* 3)then
        (modify ?*Abj* (puntaje (+ ?*Abj*.puntaje (* ?v1 2))))
        else
        (modify ?*Abj* (puntaje (+ ?*Abj*.puntaje 1)))
        )
    )

(defrule AdyArr
    
    (Baldosa (i ?i1)(j ?j1&:(> ?j1 0))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i ?i2&:(eq ?i1 ?i2))(j =(- ?j1 1))(valor ?v2&:(eq ?v2 ?v1)))
    =>
    ;suma puntaje a Arr
    (if(> ?*baldLibre* 3)then
        (modify ?*Arr* (puntaje (+ ?*Arr*.puntaje (* ?v1 2))))
        else
        (modify ?*Arr* (puntaje (+ ?*Arr*.puntaje 1)))
        )
    
    )
;las siguientes reglas chequean si existe alguna baldosa de mismo valor en la misma fila/columna
;y que tienen un cero entre ellas

(defrule AdyDer0
    
    (Baldosa (i ?i1&:(< ?i1  2))(j ?jj)(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i =(+ ?i1 1))(j ?jj)(valor ?v2&:(eq ?v2 0)))
    (Baldosa (i =(+ ?i1 2))(j ?jj)(valor ?v3&:(eq ?v1 ?v3)))
    ;?dir <- (Mov (dir Derecha))
    =>
    ;suma puntaje a Der
    (if(> ?*baldLibre* 3)then
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje (* ?v1 2))))
        else
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje 1)))
        )
    
    )

(defrule AdyIzq0
    
    (Baldosa (i ?i1&:(> ?i1 1))(j ?j1)(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i =(- ?i1 1))(j ?j1)(valor ?v2&:(eq ?v2 0)))
    (Baldosa (i =(- ?i1 2))(j ?j1)(valor ?v3&:(eq ?v3 ?v1)))
    =>
    ;suma puntaje a Izq
    (if(> ?*baldLibre* 3)then
        (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje (* ?v1 2))))
        else
        (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje 1)))
        )
    )

(defrule AdyAbj0
    
    (Baldosa (i ?i1)(j ?j1&:(< ?j1 3))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i ?i1)(j =(+ ?j1 1))(valor ?v2&:(eq ?v2 0)))
    (Baldosa (i ?i1)(j =(+ ?j1 2))(valor ?v3&:(eq ?v3 ?v1)))
    =>
    ;suma puntaje a Abj
    (if(> ?*baldLibre* 3)then
        (modify ?*Abj* (puntaje (+ ?*Abj*.puntaje (* ?v1 2))))
        else
        (modify ?*Abj* (puntaje (+ ?*Abj*.puntaje 1)))
        )
    )

(defrule AdyArr0
    
    (Baldosa (i ?i1)(j ?j1&:(> ?j1 1))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i ?i1)(j =(- ?j1 1))(valor ?v2&:(eq ?v2 0)))
    (Baldosa (i ?i1)(j =(- ?j1 2))(valor ?v3&:(eq ?v3 ?v1)))
    =>
    ;suma puntaje a Arr
    (if(> ?*baldLibre* 3)then
        (modify ?*Arr* (puntaje (+ ?*Arr*.puntaje (* ?v1 2))))
        else
        (modify ?*Arr* (puntaje (+ ?*Arr*.puntaje 1)))
        )
    )

;esta regla se dispara dos veces por jugada, por eso el puntaje sumado es sólo el valor de la baldosa
(defrule AdyDerArr0
    (Baldosa (i 0)(j ?j1&:(> ?j1 0))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i 1)(j =(- ?j1 1))(valor ?v2&:(eq ?v1 ?v2)))
    (Baldosa (i ?i1&:(> ?i1 0))(j ?j3&:(eq ?j3 ?j1))(valor ?v3&:(eq ?v3 0)))
    (Baldosa (i ?i2&:(> ?i2 0))(j ?j4&:(eq ?j4 ?j1))(valor ?v4&:(neq ?v4 0)))
    (Baldosa (i ?i3&:(> ?i3 0))(j ?j5&:(eq ?j5 ?j1))(valor ?v5&:(neq ?v5 0)&:(neq ?v5 ?v4)))
    
    ;match cuando la primer fila esta totalmente ocupada
    ;(Baldosa (i 0)(j ?j1)(valor ?b0&:(neq ?b0 0)))
    (Baldosa (i 1)(j =(- ?j1 1))(valor ?b1&:(neq ?b1 0)))
    (Baldosa (i 2)(j =(- ?j1 1))(valor ?b2&:(neq ?b2 0)))
    (Baldosa (i 3)(j =(- ?j1 1))(valor ?b3&:(neq ?b3 0)))
    
    =>
    ;suma puntaje a Der
    (if(> ?*baldLibre* 3)then
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje ?v1)))
        else
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje 1)))
        )
    )

(defrule AdyDerArr1
    (Baldosa (i 1)(j ?j1&:(> ?j1 0))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i 2)(j =(- ?j1 1))(valor ?v2&:(eq ?v1 ?v2)))
    (Baldosa (i ?i1&:(> ?i1 1))(j ?j3&:(eq ?j3 ?j1))(valor ?v3&:(eq ?v3 0)))
    (Baldosa (i ?i2&:(> ?i2 1))(j ?j4&:(eq ?j4 ?j1))(valor ?v4&:(neq ?v4 0)))
    
    ;match cuando la primer fila esta totalmente ocupada
    (Baldosa (i 0)(j =(- ?j1 1))(valor ?b0&:(neq ?b0 0)))
    ;(Baldosa (i 1)(j ?j1)(valor ?b1&:(neq ?b1 0)))
    (Baldosa (i 2)(j =(- ?j1 1))(valor ?b2&:(neq ?b2 0)))
    (Baldosa (i 3)(j =(- ?j1 1))(valor ?b3&:(neq ?b3 0)))
    
    =>
    ;suma puntaje a Der
    (if(> ?*baldLibre* 3)then
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje (* ?v1 2))))
        else
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje 1)))
        )
    )
(defrule AdyDerArr2
    (Baldosa (i 2)(j ?j1&:(> ?j1 0))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i 3)(j =(- ?j1 1))(valor ?v2&:(eq ?v1 ?v2)))
    (Baldosa (i 3)(j ?j3&:(eq ?j3 ?j1))(valor ?v3&:(eq ?v3 0)))
    
    ;match cuando la primer fila esta totalmente ocupada
    (Baldosa (i 0)(j =(- ?j1 1))(valor ?b0&:(neq ?b0 0)))
    (Baldosa (i 1)(j =(- ?j1 1))(valor ?b1&:(neq ?b1 0)))
    ;(Baldosa (i 2)(j ?j1)(valor ?b2&:(neq ?b2 0)))
    (Baldosa (i 3)(j =(- ?j1 1))(valor ?b3&:(neq ?b3 0)))
    
    =>
    ;suma puntaje a Der
    (if(> ?*baldLibre* 3)then
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje (* ?v1 2))))
        else
        (modify ?*Der* (puntaje (+ ?*Der*.puntaje 1)))
        )
    )



(defrule AdyIzqArr1
    (Baldosa (i 1)(j ?j1&:(> ?j1 0))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i 0)(j =(- ?j1 1))(valor ?v2&:(eq ?v1 ?v2)))
    (Baldosa (i 0)(j ?j3&:(eq ?j3 ?j1))(valor ?v4&:(eq ?v4 0)))
    
    ;match cuando la primer fila está totalmente ocupada
    
    (Baldosa (i 0)(j =(- ?j1 1))(valor ?b0&:(neq ?b0 0)))
    (Baldosa (i 1)(j =(- ?j1 1))(valor ?b1&:(neq ?b1 0)))
    (Baldosa (i 2)(j =(- ?j1 1))(valor ?b2&:(neq ?b2 0)))
    (Baldosa (i 3)(j =(- ?j1 1))(valor ?b3&:(neq ?b3 0)))
    
    =>
    ;suma puntaje a Der
    (if(> ?*baldLibre* 3)then
        (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje (* ?v1 2))))
        else
        (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje 1)))
        )
    )

(defrule AdyIzqArr2
    (Baldosa (i 2)(j ?j1&:(> ?j1 0))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i 1)(j =(- ?j1 1))(valor ?v2&:(eq ?v1 ?v2)))
    (Baldosa (i ?i1&:(< ?i1 2))(j ?j3&:(eq ?j3 ?j1))(valor ?v3&:(eq ?v3 0)))
    (Baldosa (i ?i2&:(< ?i2 2))(j ?j4&:(eq ?j4 ?j1))(valor ?v4&:(neq ?v4 0)))
    ;match cuando la primer fila está totalmente ocupada
    
    (Baldosa (i 0)(j =(- ?j1 1))(valor ?b0&:(neq ?b0 0)))
    (Baldosa (i 1)(j =(- ?j1 1))(valor ?b1&:(neq ?b1 0)))
    (Baldosa (i 2)(j =(- ?j1 1))(valor ?b2&:(neq ?b2 0)))
    (Baldosa (i 3)(j =(- ?j1 1))(valor ?b3&:(neq ?b3 0)))
    
    =>
    ;suma puntaje a Der
    (if(> ?*baldLibre* 3)then
        (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje (* ?v1 2))))
        else
        (modify ?*Izq* (puntaje (+ ?*Izq*.puntaje 1)))
        )
    )



(defrule Accion
    (declare (salience -1))
    (Grilla (vector ?g) (accion ?mov))
    =>
    ;lógica de desempate
    (if (>= ?*Izq*.puntaje ?*Der*.puntaje)then
        (
            if (>= ?*Arr*.puntaje ?*Abj*.puntaje)then
            (
                if (> ?*Izq*.puntaje ?*Arr*.puntaje)then
                (bind ?*Dir* 3)
                else
                (bind ?*Dir* 0)
                )
            else
            (
                if (> ?*Izq*.puntaje ?*Abj*.puntaje)then
                (bind ?*Dir* 3)
                else
                (bind ?*Dir* 2)
                )
            )
        else
        (
            if (>= ?*Arr*.puntaje ?*Abj*.puntaje)then
            (
                if (>= ?*Arr*.puntaje ?*Der*.puntaje)then
                (bind ?*Dir* 0)
                else
                (bind ?*Dir* 1)
                )
            else
            (
                if (> ?*Abj*.puntaje ?*Der*.puntaje)then
                (bind ?*Dir* 2)
                else
                (bind ?*Dir* 1)
                )
            )
        )
    )

(defrule ReglaMenor
    (declare (salience -2))
    ?m <- (Grilla (vector ?g) (accion ?mov))
    =>
    (printout t "Baldozas Libres:" ?*baldLibre* crlf)
    (printout t "Puntaje Der  :" ?*Der*.puntaje crlf)
    (printout t "Puntaje Izq. :" ?*Izq*.puntaje crlf)
    (printout t "Puntaje Abajo:" ?*Abj*.puntaje crlf)
    (printout t "Puntaje Arr. :" ?*Arr*.puntaje crlf)
    
    (modify ?m (accion ?*Dir*))
    (printout t ?*Dir* " Presione una tecla para continuar" crlf)
    ;(readline t)
    
    
    )
