module Uni.SS04.Serie5 where

--  $Id$

import SAT.Quiz
import SAT.Types
import SAT.Param ( p )

import Inter.Types

generate :: [ IO Variant ]
generate = 
    [ return $ Variant $ SAT.Quiz.quiz "SAT" "QUIZ" $ p 10
    ]