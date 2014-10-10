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
    
    (assert (Baldosa (i 0) (j 0) (valor (call ?g elementAt 0))))
    (assert (Baldosa (i 1) (j 0) (valor (call ?g elementAt 1))))
    (assert (Baldosa (i 2) (j 0) (valor (call ?g elementAt 2))))
    (assert (Baldosa (i 3) (j 0) (valor (call ?g elementAt 3))))
    (assert (Baldosa (i 0) (j 1) (valor (call ?g elementAt 4))))
    (assert (Baldosa (i 1) (j 1) (valor (call ?g elementAt 5))))
    (assert (Baldosa (i 2) (j 1) (valor (call ?g elementAt 6))))
    (assert (Baldosa (i 3) (j 1) (valor (call ?g elementAt 7))))
    (assert (Baldosa (i 0) (j 2) (valor (call ?g elementAt 8))))
    (assert (Baldosa (i 1) (j 2) (valor (call ?g elementAt 9))))
    (assert (Baldosa (i 2) (j 2) (valor (call ?g elementAt 10))))
    (assert (Baldosa (i 3) (j 2) (valor (call ?g elementAt 11))))
    (assert (Baldosa (i 0) (j 3) (valor (call ?g elementAt 12))))
    (assert (Baldosa (i 1) (j 3) (valor (call ?g elementAt 13))))
    (assert (Baldosa (i 2) (j 3) (valor (call ?g elementAt 14))))
    (assert (Baldosa (i 3) (j 3) (valor (call ?g elementAt 15))))
    
    )



;estas reglas chequean si una baldoza tiene otra del mismo valor adyacente a ella
;en las 4 direcciones y suman puntaje a cada mov acorde
(defrule AdyDer
    ;(Estrategia (valor 0))
    (Baldosa (i ?ii&:(< ?jj  3))(j ?jj)(valor ?v))
    (Baldosa (i =(+ ?ii  1))(j ?jj)(valor =(?v)))
    ?dir <- (Mov (dir Derecha))
    =>
    ;suma puntaje a Der
    (modify ?dir (puntaje (+ ?dir.puntaje (* ?v 2))))
    (printout t "Sumo a derecha" crlf)
    )

(defrule AdyAbj
    ;(Estrategia (valor 0))
    (Baldosa (i ?ii)(j ?jj&:(< ?jj 3))(valor ?v))
    (Baldosa (i ?ii)(j =(+ ?jj 1))(valor =(?v)))
    ?dir <- (Mov (dir Abajo))
    =>
    ;suma puntaje a Abj
    (modify ?dir (puntaje (+ ?dir.puntaje (* ?v 2))))
    (printout t "Sumo a abj" crlf)
    
    )

(defrule AdyArr
    ;(Estrategia (valor 0))
    (Baldosa (i ?ii)(j ?jj&:(> ?jj 0))(valor ?v))
    (Baldosa (i ?ii)(j =(- ?jj 1))(valor =(?v)))
    ?dir <- (Mov (dir Arriba))
    =>
    ;suma puntaje a Izq
    (modify ?dir (puntaje (+ ?dir.puntaje (* ?v 2))))
    (printout t "Sumo a izq" crlf)
    
    )

(defrule AdyIzq
    (Estrategia (valor 0))
    (Baldosa (i ?ii&:(> ?ii 0))(j ?jj)(valor ?v))
    (Baldosa (i =(- ?ii 1))(j ?jj)(valor ?v))
    ?dir <- (Mov (dir Izquierda))
    =>
    ;suma puntaje a Arr
    (modify ?dir (puntaje (+ ?dir.puntaje (* ?v 2))))
    (printout t "Sumo a arr" crlf)
    
    )

(defrule ReglaMenor
    (declare (salience -1))
    ?m <- (Grilla (vector ?g) (accion ?mov))
    =>
    (printout t "Presione una tecla para continuar")
    (readline t)
    
    )
