;Brian Moex, Ignacio Moya. FCEIA-2014

(deftemplate Grilla (declare (from-class Grilla)))
(deftemplate Baldosa (slot i) (slot j) (slot valor))
(deftemplate Mov (slot dir)(slot puntaje))
(assert (Mov (dir Derecha)))
(assert (Mov (dir Abajo)))


(defrule ReglaMayor
    ?m <- (Grilla (vectorE ?g) (accion ?mov))
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

; ?jj&:(< ?jj 3) la regla se dispara para una baldosa con la coordenada jj y si esta también es menor a 3

(defrule AdyDer
    (Baldosa (i ?ii)(j ?jj&:(< ?jj  3))(valor ?v))
    (Baldosa (i =(+ ?ii  1))(j ?jj)(valor =(?v)))
        =>
        ;suma puntaje a Der
        (printout t "Sumo a derecha" crlf)
        )
    
    (defrule AdyAbj
        (Baldosa (i ?ii)(j ?jj&:(< ?jj 3))(valor ?v))
        (Baldosa (i ?ii)(j =(+ ?jj 1))(valor =(?v)))
        =>
        ;suma puntaje a Abj
        (printout t "Sumo a abj" crlf)
        
        )
    
    (defrule AdyArr
        (Baldosa (i ?ii)(j ?jj&:(> ?jj 0))(valor ?v))
        (Baldosa (i ?ii)(j ?jj&:(- ?jj 1))(valor =(?v)))
        =>
        ;suma puntaje a Izq
        (printout t "Sumo a izq" crlf)
        
        )
    
    (defrule AdyIzq
        (Baldosa (i ?ii&:(> ?ii 0))(j ?jj)(valor ?v))
        (Baldosa (i ?ii&:(- ?ii 1))(j ?jj)(valor ?v))
        =>
        ;suma puntaje a Arr
        (printout t "Sumo a arr" crlf)
        
        )

(defrule ReglaMenor 
    (declare (salience -1))
    ?m <- (Grilla (vectorE ?g) (accion ?mov))
    =>
    (printout t "Presione una tecla para continuar")
    (readline t)
       
    )
    
    
