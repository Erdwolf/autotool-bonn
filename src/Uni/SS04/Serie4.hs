module Uni.SS04.Serie4 where

--  $Id$

import PCProblem.Quiz 
import PCProblem.Type
import PCProblem.Param 

import SAT.Quiz
import SAT.Types
import SAT.Param ( p )

import Inter.Types

generate :: [ IO Variant ]
generate = 
    [ return $ Variant $ make "PCP" "QUIZ" $ PCProblem.Quiz.quiz 
         $ PCProblem.Param.Param 
                { alpha     = "abc"
                , paare     = 4
                , breite    = 3
                , nah   = 7
                , fern  = 20
                , viel  = 1000
                }

    , return $ Variant $ make "PCP" "EASY" $ 
          fixed $ PCP [("1","0"),("01","1"),("1","110")]

    , return $ Variant $ make "PCP" "EXTRA" $ PCProblem.Quiz.quiz 
            $ PCProblem.Param.Param 
             { alpha        = "abc"
             , paare        = 5
             , breite       = 3
             , nah          = 20
             , fern         = 35
             , viel         = 2000
             }
    ]


-- PCP schwer [ ("001","0"),("0","1"),("1","001") ]
--    , return $ Variant $ SAT.Quiz.quiz "SAT" "QUIZ"
--       $ p 10