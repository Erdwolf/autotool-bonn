module Flow.Action where

import Autolib.TES.Identifier
import Autolib.Symbol


import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Hash


-- | the alphabet for the automaton
data Action
    = Execute Identifier
    | Condition Bool Identifier 
  deriving ( Eq, Ord )

instance Size Action where size _ = 1

instance Hash Action where
    hash ( Execute this ) = hash ( 43 :: Int, this )
    hash ( Condition flag this ) = hash ( 22 :: Int, this, flag )

instance ToDoc Action where
    toDoc ( Execute this ) = 
        text "Execute" <+> toDoc this
    toDoc ( Condition flag this ) =  
        text ( take 1 $ show flag ) <> toDoc this

instance Reader Action where
    reader = do
        cs <- reader :: Parser Identifier
	case show cs of
	    'E' : rest -> return $ Execute $ mkunary rest
	    'T' : rest -> return $ Condition True  ( mkunary rest ) 
	    'F' : rest -> return $ Condition False ( mkunary rest ) 
	    _ -> fail "no parse"

instance Symbol Action
