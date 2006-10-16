module Axioms

( axioms
)

where

import Syntax
import Read

axioms :: [ Exp ]
axioms = parsed . unlines $ source

source :: [ String ]
source =
     [ "A -> (B -> A) "                             
     , "((A -> B) -> A) -> A "                       
     , "(A -> B) -> ((B -> C) -> (A -> C)) "        

{-
     , "A & B -> A "                                
     , "A & B -> B "                                
     , "(A -> B) -> ((A -> C) -> (A -> B & C)) "    
     , "A -> A | B "                                
     , "B -> A | B "                                
     , "(A -> C) -> ((B -> C) -> (A | B -> C)) "    
     , "(A <-> B) -> (A -> B) "                     
     , "(A <-> B) -> (B -> A) "                     
     , "(A -> B) -> ((B -> A) -> (A <-> B)) "       
-}


     , "(A -> B) -> (not B -> not A) "              
     , "A -> not (not A) "                          
     , "not (not A) -> A "                          


     ]                                                





