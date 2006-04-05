module Turing.Check where

--   $Id$

import Turing.Type

import Autolib.Reporter
import Autolib.ToDoc

import Control.Monad ( guard )

check :: TuringC y z
      => Turing y z -> Reporter ()
check m = do
      when ( isEmptySet $ eingabealphabet m ) 
	   $ reject $ text "das Eingabe-Alphabet ist leer"
      when ( leerzeichen m `elementOf` eingabealphabet m )
           $ reject $ text "das Leerzeichen gehört zum Eingabealphabet"
      when ( not ( leerzeichen m `elementOf` arbeitsalphabet m) ) 
           $ reject $ text "das Leerzeichen gehört nicht zum Arbeitsalphabet" 
      let e = eingabealphabet m
          a = arbeitsalphabet m      
      when ( not (isEmptySet (e `minusSet` a)) )
	   $ reject $ fsep [ text "das Eingabealphabet", toDoc e 
	       , text "ist keine Teilmenge des Arbeitsalphabets", toDoc a
			     ]

      let check_regel (r, ((y,z), (y',z',b)) ) = do
           let ca t = do
	         when ( not ( t `elementOf` arbeitsalphabet m ) ) $ do
		      inform $ text "Fehler in Regel" <+> toDoc r
		      reject $ text "Zeichen" <+> toDoc t 
			    <+> text "gehört nicht zum Arbeitsalphabet" 
	   mapM_ ca [ y, y' ] 
	   let cz t = do
		 when ( not ( t `elementOf` zustandsmenge m ) ) $ do
		      inform $ text "Fehler in Regel" <+> toDoc r
		      reject $ text "Zustand" <+> toDoc t 
			    <+> text "gehört nicht zur Zustandsmenge" 
	   mapM_ cz [ z, z' ]
      mapM_ check_regel $ do
          r @ ((y,z),yzbs) <- fmToList $ tafel m
          (y',z',b) <- setToList yzbs
	  return (r, ((y,z), (y',z',b)) )

      let z = startzustand m
      when ( not (z `elementOf` zustandsmenge m) )
	   $ reject $ text "Startzustand " <+> toDoc z <+>
		 text "gehört nicht zur Zustandsmenge"

      let ce z = do
          when ( not (z `elementOf` zustandsmenge m))
	       $ reject $ text "Endzustand" <+> toDoc z
		    <+> text "gehört nicht zur Zustandsmenge"
      mapM_ ce $ setToList $ endzustandsmenge m
      inform $ text "Das ist wirklich eine Turingmaschine."

deterministisch :: TuringC y z
      => Turing y z 
      -> Reporter ()
deterministisch m = do
    let mehr = do
	    r @ ( yz, s ) <- fmToList $ tafel m
	    guard $ cardinality s > 1
	    return r
    case mehr of
	    [] -> inform $ text "diese Maschine ist deterministisch"
	    rs -> do
		  inform $ text "diese Regeln sind nicht deterministisch:"
		  reject $ toDoc rs


