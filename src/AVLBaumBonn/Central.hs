{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}
module AVLBaumBonn.Central where

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, Doc(..), text, vcat, hcat, ($$),  (<>), (<+>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>), empty)
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(Increasing), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless)
import Data.List (zip5, transpose, intersperse)

import qualified Baum.Such.Generate
import qualified Baum.AVL.Type


data AVLBaum = AVLBaum deriving Typeable

$(derives [makeReader, makeToDoc] [''AVLBaum])


type Config = Baum.Such.Generate.Instanz Baum.AVL.Type.AVLTree Int

make_fixed = direct AVLBaum example

instance OrderScore AVLBaum where
    scoringOrder h = Increasing

instance Verify AVLBaum Config where
    verify _ cfg = do
        return ()

make_fixed = direct AVLBaum $ error "no direct configuration possible"


newtype OpList = OpList [Baum.Such.Op.Op Int] deriving (Typeable)

{-
instance ToDoc (Op Int) where
    toDoc (Insert a) = text "Insert" <+> toDoc a
    toDoc (Delete a) = text "Delete" <+> toDoc a
    toDoc Any        = text "Any"
-}

instance Reader OpList where
    reader = do
        ops <- reader
        return (OpList ops)

instance ToDoc OpList where
    toDoc (OpList ops) = text (show ops)


instance Partial AVLBaum Config OpList where
    report _ ( start, plan, end ) = do
       inform $ text "Auf den Baum:"
       peng start
       inform $ vcat
          [ text "sollen diese Operationen angewendet werden"
              , text "(wobei Sie  Any  geeignet ersetzen sollen):"
              , nest 4 $ niceOps plan
              , text "so dass dieser Baum entsteht:"
              ]
       peng end
       inform $ text "Im Bild leer dargestellte Knoten dienen nur der Verdeutlichung, welcher der beiden Teilbäume leer ist."
       inform $ text "Beim Eingabeformat leitet \"--\" einen Kommentar ein. Sie können dies nutzen, um sich wie oben gezeigt Zeilen zu markieren."

    initial _ ( start, plan, end ) =
        OpList plan

 total   _ ( start, plan, end ) (OpList ops) = do
        --inform $ text "Beginne mit"
        --peng start
        c <- steps start plan ops
        --inform $ text "Stimmt überein mit Aufgabenstellung?"
        if c `equal` end
           then inform $ text "Ja."
           else rejectTree c $ text "Resultat stimmt nicht mit Aufgabenstellung überein."

      where
        rejectTree b reason = do
            inform $ text $ "<b>Tatsächlicher Baum  <->  Ziel-Baum</b>"
            peng b   -- Tatsächlicher Baum
            peng end -- Erwarteter Baum
            reject $ text "Nein." <+> reason

        step b op = do
            --inform $ text "Operation:" <+> toDoc op
            c <- case op of
        	 Insert a -> return $ insert b a
        	 Delete a -> return $ delete b a
        	 _        -> reject $ text "Operation ist unbekannt"
            --inform $ text "Resultat:"
            return c

        steps b [] [] = return b
        steps b [] send = rejectTree b $ vcat
                               [ text "Sie wollen noch diese Operationen ausführen:"
        	                   , nest 4 $ niceOps send
        	                   , text "es sind aber keine mehr zugelassen."
        	                   ]
        steps b plan [] = rejectTree b $ vcat
                               [ text "Es müssen noch diese Operationen ausgeführt werden:"
        	                   , nest 4 $ niceOps plan
        	                   ]
        steps b (p : plan) (s : send) = do
            conforms p s
            c <- step b s
            steps c plan send
          where
            conforms _ Any = do
                rejectTree b $ text "Sie sollen Any durch eine Operation ersetzen."
            conforms Any _ = return ()
            conforms x y | x == y = return ()
            conforms x y | x /= y = do
                rejectTree b $ text "Die Operation" <+> toDoc x <+> text "soll nicht geändert werden." 

niceOps [] = text "[]"
niceOps (x:xs) = vcat [ text "[" <+> toDoc x <> if x==Any then text "" else text " -- fixed"
                      , vcat [ text "," <+> toDoc x' <> if x'==Any then text "" else text " -- fixed" | x' <- xs ]
                      , text "]"
                      ]

