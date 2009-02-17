module Brainfuck.Syntax.Print where

import Brainfuck.Syntax.Data

import Autolib.ToDoc


instance ToDoc Statement where
   toDoc p = case p of
       Block sts -> parens $ hcat $ map toDoc sts
       Loop sts -> brackets $ hcat $ map toDoc sts
       Plus -> text "+"
       Minus -> text "-"
       MRight -> text ">"
       MLeft -> text "<"
       Input -> text ","
       Output -> text "."
       Null -> text " "
       
 