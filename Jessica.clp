;Brian Moex, Ignacio Moya. FCEIA-2014
(import nc.player.*)

(deftemplate Grilla (declare (from-class Grilla)))
(deftemplate Baldosa (slot i) (slot j) (slot valor))
(deftemplate Mov (slot dir)(slot puntaje))
(deftemplate Estrategia (slot valor))

(assert (Estrategia (valor 0)))
;valor=0, el programa trata de crear una baldoza con el mayor puntaje posible
;valor=1, el programa intenta liberar baldozas, suma puntaje en base a que dirección tiene la mayor cantidad de fusiones posibles
(assert (Mov (dir Derecha)))
(assert (Mov (dir Abajo)))
(assert (Mov (dir Arriba)))
(assert (Mov (dir Izquierda)))


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
    
    ;    (assert (Baldosa (j 0)(i 0)  (valor (call ?g elementAt 0))))
    ;    (assert (Baldosa (j 0)(i 1)  (valor (call ?g elementAt 1))))
    ;    (assert (Baldosa (j 0)(i 2)  (valor (call ?g elementAt 2))))
    ;    (assert (Baldosa (j 0)(i 3)  (valor (call ?g elementAt 3))))
    ;    (assert (Baldosa (j 1)(i 0)  (valor (call ?g elementAt 4))))
    ;    (assert (Baldosa (j 1)(i 1)  (valor (call ?g elementAt 5))))
    ;    (assert (Baldosa (j 1)(i 2)  (valor (call ?g elementAt 6))))
    ;    (assert (Baldosa (j 1)(i 3)  (valor (call ?g elementAt 7))))
    ;    (assert (Baldosa (j 2)(i 0)  (valor (call ?g elementAt 8))))
    ;    (assert (Baldosa (j 2)(i 1)  (valor (call ?g elementAt 9))))
    ;    (assert (Baldosa (j 2)(i 2)  (valor (call ?g elementAt 10))))
    ;    (assert (Baldosa (j 2)(i 3)  (valor (call ?g elementAt 11))))
    ;    (assert (Baldosa (j 3)(i 0)  (valor (call ?g elementAt 12))))
    ;    (assert (Baldosa (j 3)(i 1)  (valor (call ?g elementAt 13))))
    ;    (assert (Baldosa (j 3)(i 2)  (valor (call ?g elementAt 14))))
    ;    (assert (Baldosa (j 3)(i 3)  (valor (call ?g elementAt 15))))
    
    )


;estas reglas chequean si una baldoza tiene otra del mismo valor adyacente a ella
;en las 4 direcciones y suman puntaje a cada mov acorde
(defrule AdyDer
    ;(Estrategia (valor 0))
    (Baldosa (i ?i1&:(< ?i1  3))(j ?jj)(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i =(+ ?i1 1))(j ?jj)(valor ?v2&:(eq ?v1 ?v2)))
    ;?dir <- (Mov (dir Derecha))
    =>
    ;suma puntaje a Der
    ;(modify ?dir (puntaje (+ ?dir.puntaje (* ?v1 2))))
    (printout t "Sumo a Der" crlf)
    )

(defrule AdyAbj
    ;(Estrategia (valor 0))
    (Baldosa (i ?i1)(j ?j1&:(< ?j1 3))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i ?i1)(j =(+ ?j1 1))(valor ?v2&:(eq ?v2 ?v1)))
    ;?dir <- (Mov (dir Abajo))
    =>
    ;suma puntaje a Abj
    ;(modify ?dir (puntaje (+ ?dir.puntaje (* ?v1 2))))
    (printout t "Sumo a Abj" crlf)
    
    )

(defrule AdyArr
    ;(Estrategia (valor 0))
    ?dir <- (Mov (dir Arriba))
    (Baldosa (i ?i1)(j ?j1&:(> ?j1 0))(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i ?i1)(j =(- ?j1 1))(valor ?v2&:(eq ?v2 ?v1)))
    
    =>
    ;suma puntaje a Izq
    ;(modify ?dir (puntaje (+ ?dir.puntaje (* ?v1 2))))
    (printout t "Sumo a Arr" crlf)
    
    )

(defrule AdyIzq
    ;(Estrategia (valor 0))
    (Baldosa (i ?i1&:(> ?i1 0))(j ?j1)(valor ?v1&:(neq ?v1 0)))
    (Baldosa (i =(- ?i1 1))(j ?j1)(valor ?v2&:(eq ?v2 ?v1)))
    ;?dir <- (Mov (dir Izquierda))
    =>
    ;suma puntaje a Arr
    ;(modify ?dir (puntaje (+ ?dir.puntaje (* ?v1 2))))
    (printout t "Sumo a Izq" crlf)
    
    )

(defrule ReglaMenor
    (declare (salience -1))
    ?m <- (Grilla (vector ?g) (accion ?mov))
    =>
    (printout t crlf "Presione una tecla para continuar")
    (readline t)
    
    )
