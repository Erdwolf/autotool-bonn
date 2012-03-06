module TextConfig where

import Autolib.ToDoc (vcat, text)

ok =
    vcat [ text "Ja, Ihre Einsendung ist richtig."
         , text ""
         , text "Ignorieren Sie die unten angezeigte Bewertung."
         ]

noFeedbackDisclaimer =
    vcat [ text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
         , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt ist."
         ]

noFeedbackResult =
    vcat [ text "Nicht geprüft."
         , text ""
         , text "Die Einsendung wird von Ihrem Tutor bewertet."
         , text ""
         , text "Ignorieren Sie die unten angezeigte Bewertung."
         ]
