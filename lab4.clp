; Lab work 4
;   ANSI codepage!!!
;

(deftemplate State
    (slot id (type INTEGER) (default 0))
    (slot cf (type INTEGER)) ; Значение ЦФ для данной вершины
    (slot depth (type INTEGER))
    (slot status (type SYMBOL) (allowed-values rdy open ok) (default rdy)) ; В кайме, раскрыта или в ответе
    (slot parentid (type INTEGER))
(slot TL (type INTEGER) (range 0 8)) ; Всякие ячейки
(slot TT (type INTEGER) (range 0 8))
(slot TR (type INTEGER) (range 0 8))
(slot LL (type INTEGER) (range 0 8))
(slot CC (type INTEGER) (range 0 8))
(slot RR (type INTEGER) (range 0 8))
(slot BL (type INTEGER) (range 0 8))
(slot BB (type INTEGER) (range 0 8))
(slot BR (type INTEGER) (range 0 8))
)

(defglobal
?*runmode* = 1
?*lastTL* = 1
?*lastTT* = 2
?*lastTR* = 3
?*lastLL* = 4
?*lastCC* = 5
?*lastRR* =   6
?*lastBL* = 8
?*lastBB* =  7
?*lastBR* = 0
?*manhatX* = (create$ 1 2 3 4 5 6 7 8) ?*manhatY* = (create$ 1 2 3 4 5 6 7 8)

?*nextid* = 0
?*parentid* = -1
?*depth* = 0
?*opens* = 0
)

(deffunction get_id()
    (bind ?*nextid* (+ ?*nextid* 1))
    return ?*nextid*
)

(deffunction SetManhatX(?id ?value)
    (if (neq ?id 0) then (bind ?*manhatX* (replace$ ?*manhatX* ?id ?id ?value))))

(deffunction SetManhatY(?id ?value)
    (if (neq ?id 0) then (bind ?*manhatY* (replace$ ?*manhatY* ?id ?id ?value))))

; Функция метрики (по пропущенным ячейкам или по манхэт. расстоянию)
(deffunction W(?curdepth ?TLx ?TTx ?TRx ?RRx ?BRx ?BBx ?BLx ?LLx ?CCx)
    (bind ?a ?curdepth)     ; f(n) := g(n)
    ; Пропущенные ячейки
;    (if (not (= ?TLx ?*lastTL*)) then (bind ?a (+ 1 ?a)))
;    (if (not (= ?TTx ?*lastTT*)) then (bind ?a (+ 1 ?a)))
;    (if (not (= ?TRx ?*lastTR*)) then (bind ?a (+ 1 ?a)))
;    (if (not (= ?RRx ?*lastRR*)) then (bind ?a (+ 1 ?a)))
;    (if (not (= ?BRx ?*lastBR*)) then (bind ?a (+ 1 ?a)))
;    (if (not (= ?BBx ?*lastBB*)) then (bind ?a (+ 1 ?a)))
;    (if (not (= ?BLx ?*lastBL*)) then (bind ?a (+ 1 ?a)))
;    (if (not (= ?LLx ?*lastLL*)) then (bind ?a (+ 1 ?a)))

    ; Манхэттенское расстояние
   (if (neq ?TLx 0) then (bind ?a (+ ?a
       (abs (- 1 (nth$ ?TLx ?*manhatX*)))
       (abs (- 1 (nth$ ?TLx ?*manhatY*)))
   )))
   (if (neq ?TTx 0) then (bind ?a (+ ?a
       (abs (- 2 (nth$ ?TTx ?*manhatX*)))
       (abs (- 1 (nth$ ?TTx ?*manhatY*)))
   )))
   (if (neq ?TRx 0) then (bind ?a (+ ?a
       (abs (- 3 (nth$ ?TRx ?*manhatX*)))
       (abs (- 1 (nth$ ?TRx ?*manhatY*)))
   )))
   (if (neq ?RRx 0) then (bind ?a (+ ?a
       (abs (- 3 (nth$ ?RRx ?*manhatX*)))
       (abs (- 2 (nth$ ?RRx ?*manhatY*)))
   )))
   (if (neq ?CCx 0) then (bind ?a (+ ?a
       (abs (- 2 (nth$ ?CCx ?*manhatX*)))
       (abs (- 2 (nth$ ?CCx ?*manhatY*)))
   )))
   (if (neq ?BRx 0) then (bind ?a (+ ?a
       (abs (- 3 (nth$ ?BRx ?*manhatX*)))
       (abs (- 3 (nth$ ?BRx ?*manhatY*)))
   )))
   (if (neq ?BBx 0) then (bind ?a (+ ?a
       (abs (- 2 (nth$ ?BBx ?*manhatX*)))
       (abs (- 3 (nth$ ?BBx ?*manhatY*)))
   )))
   (if (neq ?BLx 0) then (bind ?a (+ ?a
       (abs (- 1 (nth$ ?BLx ?*manhatX*)))
       (abs (- 3 (nth$ ?BLx ?*manhatY*)))
   )))
   (if (neq ?LLx 0) then (bind ?a (+ ?a
       (abs (- 1 (nth$ ?LLx ?*manhatX*)))
       (abs (- 2 (nth$ ?LLx ?*manhatY*)))
   )))

    return ?a
)

(deffunction PrintField(?state $?indentstr)
    (if (= 1 (length ?indentstr)   
            ) then (bind
          ?prefix (nth$ 1 ?indentstr))
        else
      (bind ?prefix ""))

    (printout stdout
        ?prefix (fact-slot-value ?state TL) " " (fact-slot-value ?state TT) " " (fact-slot-value ?state TR)crlf
        ?prefix (fact-slot-value ?state LL) " " (fact-slot-value ?state CC) " " (fact-slot-value ?state RR)crlf
        ?prefix (fact-slot-value ?state BL) " " (fact-slot-value ?state BB) " " (fact-slot-value ?state BR)crlf)
)


(deffunction new_opening(?mincff ?node)
    (if (eq ?*runmode* 1) then
        (printout t "'r'=авторежим, 'q'=выход, другое=шаг > ")
        (bind ?inputstr (read))
        (if (eq ?inputstr q) then
            (halt))
        (if (or (eq ?inputstr run) (eq ?inputstr r)) then
            (bind ?*runmode* 0)))

    (if (eq ?*runmode* 1) then
        (printout t "Раскрываем, с ЦФ " (fact-slot-value ?node cf) crlf)
        (PrintField ?node "    "))

    (bind ?*parentid* (fact-slot-value ?node id))
    (bind ?*depth* (+ 1 (fact-slot-value ?node depth)))
    (retract ?mincff)
    (assert (mincf 9999))
    (bind ?*opens* (+ 1 ?*opens*))

    (modify ?node (status open))
)

(deffunction new_node(?TL ?TT ?TR ?LL ?CC ?RR ?BL ?BB ?BR)
    ; Новое число ЦФ
    (bind ?newcf (W ?*depth* ?TL ?TT ?TR ?RR ?BR ?BB ?BL ?LL ?CC))

    ; Пропуск уже существующих состояний
    (if (not (any-factp ((?f State)) (and
            (eq ?f:TL ?TL) (eq ?f:TT ?TT) (eq ?f:TR ?TR)
            (eq ?f:RR  ?RR) (eq ?f:CC ?CC) (eq ?f:LL ?LL)
            (eq ?f:BL ?BL) (eq ?f:BB ?BB) (eq ?f:BR ?BR)
        )))
    then
        (bind ?f (assert (State (id (get_id))
                    (TL ?TL) (TT    ?TT) (TR ?TR)
                    (LL   ?LL) (CC ?CC) (RR   ?RR)
                    (BL ?BL) (BB    ?BB) (BR ?BR)
                    (depth ?*depth*) (parentid ?*parentid*) (cf ?newcf)
                )
        ))

        (if (eq ?*runmode* 1) then
            (printout t "Новое состояние, ЦФ " ?newcf crlf)
            (PrintField ?f)))
)





; Init

(defrule LABSTART
    (declare (salience 1000))
    (initial-fact)

        =>

    (SetManhatX ?*lastTL* 1)(SetManhatX ?*lastTT* 2)(SetManhatX ?*lastTR* 3)
    (SetManhatX ?*lastLL* 1)(SetManhatX ?*lastCC* 2)(SetManhatX ?*lastRR* 3)
    (SetManhatX ?*lastBL* 1)(SetManhatX ?*lastBB* 2)(SetManhatX ?*lastBR* 3)

    (SetManhatY ?*lastTL* 1)(SetManhatY ?*lastTT* 1)(SetManhatY ?*lastTR* 1)
    (SetManhatY ?*lastLL* 2)(SetManhatY ?*lastCC* 2)(SetManhatY ?*lastRR* 2)
    (SetManhatY ?*lastBL* 3)(SetManhatY ?*lastBB* 3)(SetManhatY ?*lastBR* 3)

    (new_node 1 3 2
              4 8 5
              0 7 6)
    (bind ?firststate (nth$ 1
        (find-fact ((?st State)) (eq ?st:depth 0))))
    (assert (mincf (fact-slot-value ?firststate cf)))
)



; Если все цифры расположены на своих местах
(defrule goal_test (declare (salience 500))
    ?f <- (State
        (TL ?NW&:(= ?NW ?*lastTL*)) (TT ?N&:(= ?N ?*lastTT*)) (TR ?NE&:(= ?NE ?*lastTR*))
        (LL ?W& :(= ?W  ?*lastLL*))                           (RR  ?E&:(= ?E  ?*lastRR*))
        (BL ?SW&:(= ?SW ?*lastBL*)) (BB ?S&:(= ?S ?*lastBB*)) (BR ?SE&:(= ?SE ?*lastBR*))
        (status ~ok)
    )

        =>
    
    (modify ?f (status ok))
)

; Бэктрейсим до корня
(defrule backtrace_answer (declare (salience 499))
    (State (status ok) (parentid ?Id) (id ?curId))
    ?f <- (State (id ?Id) (status ~ok))
    
        =>
    
    (printout t crlf "Глубина " (+ 1 (fact-slot-value ?f depth)) crlf)
    (PrintField ?f)
    (modify ?f (status ok))
)
; И удаляем все лишнее
(defrule delete_excess_states (declare (salience 400))
    (State (status ok))
    ?f <- (State (status ~ok))

        =>

    (retract ?f)

    ;(do-for-all-facts ((?removee State)) (neq ?removee:status ok)
    ;   (retract ?removee)
    ;)
)


; Если все вершины раскрыты
(defrule FAIL (declare (salience 200))
    (not (State (status rdy|ok)))

        =>

    (printout t "РЕШЕНИЙ НЕТ" crlf)
    (halt)
)
(defrule SUCCESS (declare (salience 200))
    (State (status ok))

        =>

    (printout t "РЕШЕНИЕ НАЙДЕНО! " ?*opens* " раскрытий, " (- ?*nextid* 1) " состояний" crlf)
    (halt)
)


; Определение текущего минимума ЦФ:
(defrule find_min (declare (salience 150))
    ; Условие существования вершины, у которой значение целевой функции меньше текущего min:
    ?fmin <- (mincf ?min)
    (State (cf ?E&:(< ?E ?min)) (status rdy))

        =>
    
    (retract ?fmin)
    (assert (mincf ?E)) ; Обновляем минимум.
)



(defrule make_new_path_TL (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL  0 ) (TT  ?MT) (TR ?RT)
            (LL  ?LM) (CC ?MM) (RR  ?RM)
            (BL ?LB) (BB  ?MB) (BR ?RB))=>
    (new_opening ?fmin ?f)

    (new_node   ?MT  0  ?RT
                ?LM ?MM ?RM
                ?LB ?MB ?RB
    )
    (new_node   ?LM ?MT ?RT
                 0  ?MM ?RM
                ?LB ?MB ?RB
    )
)

(defrule make_new_path_TT (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL ?LT) (TT   0 ) (TR ?RT)
            (LL  ?LM) (CC ?MM) (RR  ?RM)
            (BL ?LB) (BB  ?MB) (BR ?RB))=>
    (new_opening ?fmin ?f)

    (new_node   ?LT ?RT  0 
                ?LM ?MM ?RM
                ?LB ?MB ?RB
    )
    (new_node    0  ?LT ?RT
                ?LM ?MM ?RM
                ?LB ?MB ?RB
    )
    (new_node   ?LT ?MM ?RT
                ?LM  0  ?RM
                ?LB ?MB ?RB
    )
)

(defrule make_new_path_TR (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL ?LT) (TT  ?MT) (TR  0 )
            (LL  ?LM) (CC ?MM) (RR  ?RM)
            (BL ?LB) (BB  ?MB) (BR ?RB))=>
    (new_opening ?fmin ?f)

    (new_node   ?LT  0  ?MT
                ?LM ?MM ?RM
                ?LB ?MB ?RB
    )
    (new_node   ?LT ?MT ?RM
                ?LM ?MM  0 
                ?LB ?MB ?RB
    )
)

(defrule make_new_path_WCell (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL ?LT) (TT  ?MT) (TR ?RT)
            (LL   0 ) (CC ?MM) (RR  ?RM)
            (BL ?LB) (BB  ?MB) (BR ?RB))=>
    (new_opening ?fmin ?f)

    (new_node   ?LT ?MT ?RT
                ?MM  0  ?RM
                ?LB ?MB ?RB
    )
    (new_node    0  ?MT ?RT
                ?LT ?MM ?RM
                ?LB ?MB ?RB
    )
    (new_node   ?LT ?MT ?RT
                ?LB ?MM ?RM
                 0  ?MB ?RB
    )
)

(defrule make_new_path_MMCell (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL ?LT) (TT  ?MT) (TR ?RT)
            (LL  ?LM) (CC   0) (RR  ?RM)
            (BL ?LB) (BB  ?MB) (BR ?RB))=>
    (new_opening ?fmin ?f)

    (new_node   ?LT ?MT ?RT
                ?LM ?MB ?RM
                ?LB  0  ?RB
    )
    (new_node   ?LT ?MT ?RT
                ?LM ?RM  0 
                ?LB ?MB ?RB
    )
    (new_node   ?LT  0  ?RT
                ?LM ?MT ?RM
                ?LB ?MB ?RB
    )
    (new_node   ?LT ?MT ?RT
                 0  ?LM ?RM
                ?LB ?MB ?RB
    )
)

(defrule make_new_path_ECell (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL ?LT) (TT  ?MT) (TR ?RT)
            (LL  ?LM) (CC ?MM) (RR   0 )
            (BL ?LB) (BB  ?MB) (BR ?RB))=>
    (new_opening ?fmin ?f)

    (new_node   ?LT ?MT ?RT
                ?LM  0  ?MM
                ?LB ?MB ?RB
    )
    (new_node   ?LT ?MT  0 
                ?LM ?MM ?RT
                ?LB ?MB ?RB
    )
    (new_node   ?LT ?MT ?RT
                ?LM ?MM ?RB
                ?LB ?MB  0 
    )
)

(defrule make_new_path_SWCell (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL ?LT) (TT  ?MT) (TR ?RT)
            (LL  ?LM) (CC ?MM) (RR  ?RM)
            (BL  0 ) (BB  ?MB) (BR ?RB)) =>
    (new_opening ?fmin ?f)

    (new_node   ?LT ?MT ?RT
                 0  ?MM ?RM
                ?LM ?MB ?RB
    )
    (new_node   ?LT ?MT ?RT
                ?LM ?MM ?RM
                ?MB  0  ?RB
    )
)

(defrule make_new_path_SCell (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL ?LT) (TT  ?MT) (TR ?RT)
            (LL  ?LM) (CC ?MM) (RR  ?RM)
            (BL ?LB) (BB   0 ) (BR ?RB)) =>
    (new_opening ?fmin ?f)

    (new_node   ?LT ?MT ?RT
                ?LM ?MM ?RM
                ?LB ?RB  0 
    )
    (new_node   ?LT ?MT ?RT
                ?LM  0  ?RM
                ?LB ?MM ?RB
    )
    (new_node   ?LT ?MT ?RT
                ?LM ?MM ?RM
                 0  ?LB ?RB
    )
)

(defrule make_new_path_SECell (declare (salience 100))
    ?fmin <- (mincf ?min)
    ?f <- (State (status rdy) (cf ?E& :(= ?E ?min))
            (TL ?LT) (TT  ?MT) (TR ?RT)
            (LL  ?LM) (CC ?MM) (RR  ?RM)
            (BL ?LB) (BB  ?MB) (BR  0 )
        )

                =>

    (new_opening ?fmin ?f)

    (new_node   ?LT ?MT ?RT
                ?LM ?MM  0 
                ?LB ?MB ?RM
    )
    (new_node   ?LT ?MT ?RT
                ?LM ?MM ?RM
                ?LB  0  ?MB
    )
)

