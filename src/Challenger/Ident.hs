module Challenger.Ident where

-- $Id$
    
data Ident = Ident {aufgabe :: Int } deriving (Show, Read)

showIdentForDatei :: Ident -> String
showIdentForDatei i = show (aufgabe i)
