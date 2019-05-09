(deftemplate disease
    (slot name (type SYMBOL)(default ?NONE))
    (multislot symptoms)
    (slot probability (type FLOAT)(default 0.0))
    (slot checked (type INTEGER) (default 0))
)
(deffacts disease_list
    (disease 
        (name alergia) 
        (symptoms "lagrimeo" "estornudos"))
    (disease
        (name asma) 
        (symptoms "dificultad respiratoria" "inflamacion en el pecho" "tos"))
    (disease
        (name diabetes) 
        (symptoms "hambre" "fatiga"))
    (disease 
        (name herpes) 
        (symptoms "ardor area genital" "ulceras" "fiebre" "dolor de cabeza"))
    (disease 
        (name hepatitis) 
        (symptoms "fatiga" "fiebre" "dolor abdominal" "nauseas"))
    (disease
        (name gastroenteritis) 
        (symptoms "fatiga" "fiebre" "dolor de cabeza" "nauseas"))
    (disease
        (name otitis)
        (symptoms "fiebre" "dolor de oido" "irritabilidad"))
    (disease
        (name bronquitis)
        (symptoms "fiebre" "fatiga" "dificultad respiratoria" "molestia en el pecho"))
    (disease
        (name gripe)
        (symptoms "fatiga" "estornudos" "tos" "congestion nasal"))
    (disease 
        (name conjuntivitis)
        (symptoms "lagrimeo" "ardor en los ojos"))
    (disease
        (name resfriado)
        (symptoms "tos" "estornudos" "congestion nasal"))
    (disease
        (name sinusitis)
        (symptoms "fiebre" "congestion nasal" "dolor de cabeza"))
    (disease
        (name faringitis)
        (symptoms "dolor de oido" "fiebre" "dolor al deglutir"))
    (disease
        (name bronquiolitis)
        (symptoms "tos" "fatiga" "fiebre" "dificultad respiratoria"))
    (disease
        (name intoxicacion)
        (symptoms "nauseas" "fatiga" "fiebre"))
)
(deffacts symptom_list
    (symptoms "lagrimeo" "estornudos" "dificultad respiratoria" "dolor de cabeza" "dolor de oido"
    "dolor al deglutir" "ulceras" "fiebre" "fatiga" "nauseas" "tos" "congestion nasal" 
    "ardor area genital" "ardor en los ojos")
)

(deffacts initialFacts
    (mainMenu)
)

(defglobal ?*answerVariable* = nil)


(deffacts result
    (target)
    (highest 0.0 none)
    (second 0.0 none)
    (third 0.0 none)
)

(deffunction printMenu ()
    (printout t "   1. Add symptom" crlf)
    (printout t "   2. Delete last symptom added" crlf)
    (printout t "   3. Get result" crlf)
    (printout t "   0. Exit" crlf)
    (printout t crlf)
    (printout t "Select option: ")
)


(defrule mainMenu
    ?f<-(mainMenu)
=>
    (retract ?f)
    (printMenu)
    (assert (option (read)))
)

(defrule menuExit
    ?f<-(option 0)
=>
    (retract ?f)
    (exit)
)

(defrule menuNewSymptom
    ?f<-(option 1)  
    ?s<-(target $?symptoms)
    (symptoms $?valid)
=>
    (retract ?f)
    (printout t "Introduce symptom (lowercase and between double quotes):" crlf)
    (bind ?*answerVariable* (read))
    (if (not (member$ ?*answerVariable* ?valid)) then
        (printout t "That symptom is not valid" crlf)
    else
        (if (member$ ?*answerVariable* ?symptoms) then
            (printout t "That symptom has already been introduced" crlf)
        else
            (retract ?s)
            (assert (target ?*answerVariable* ?symptoms))
        )
    )
    (assert (mainMenu))
)

(defrule menuDeleteSymptom
    ?f<-(option 2)
    ?s<-(target $?symptoms)
=>
    (retract ?f)
    (retract ?s)
    (assert (target (rest$ ?symptoms)))
    (assert (mainMenu))
)

(defrule menuCheck
    ?f<-(option 3)
=>
    (retract ?f)
    (assert (goToCheck))
)


(defrule assignProbability
    ?f<-(goToCheck)
    ?d<-(disease (name ?n) (symptoms $?symptoms) (checked 0))
    (target $?list)
    ?h<-(highest ?p1 ?d1)
    ?s<-(second ?p2 ?d2)
    ?t<-(third ?p3 ?d3)
=>
    (bind ?count 0)
    (loop-for-count (?i (length$ ?list)) do
        (if (member$ (nth ?i ?list) ?symptoms) then
            (bind ?count (+ 1 ?count))
        )
    )
    (retract ?d)
    (bind ?prob (/ ?count (length$ ?symptoms)))
    (assert (disease (name ?n) (symptoms ?symptoms) (probability ?prob) (checked 1)))
    (if (> ?prob ?p1) then
        (retract ?h)
        (retract ?s)
        (retract ?t)
        (assert (highest ?prob ?n))
        (assert (second ?p1 ?d1))
        (assert (third ?p2 ?d2))
    else
        (if (> ?prob ?p2) then
            (retract ?s)
            (retract ?t)
            (assert (second ?prob ?n))
            (assert (third ?p2 ?d2))   
        else
            (if (> ?prob ?p3) then
                (retract ?t)
                (assert (third ?prob ?n))
            )
        )

    )
)

(defrule showResult
    (declare (salience -1000))
    ?f<-(goToCheck)
    (highest ?p1 ?d1)
    (second ?p2 ?d2)
    (third ?p3 ?d3)
=>
    (retract ?f)
    (printout t "The results are:" crlf)
    (printout t "1. " ?d1 " with a probability of " ?p1 crlf)
    (printout t "2. " ?d2 " with a probability of " ?p2 crlf)
    (printout t "3. " ?d3 " with a probability of " ?p3 crlf)
)