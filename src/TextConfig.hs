module TextConfig where

import Autolib.ToDoc (vcat, text)

ok =
    vcat [ text "Ja, Ihre Einreichung ist richtig."
         , text ""
         , text "Ignorieren Sie die unten angezeigte Bewertung."
         ]

noFeedbackDisclaimer =
    vcat [ text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
         , text "         Wenn eine Einreichung akzeptiert wird, heißt dies nicht, dass sie korrekt ist."
         ]

noFeedbackResult =
    vcat [ text "Verdeckt geprüft."
         , text ""
         , text "Die Bewertung Ihrer Einreichung erfahren Sie von Ihrem Tutor."
         , text ""
         , text "Ignorieren Sie die unten angezeigte Bewertung."
         ]
