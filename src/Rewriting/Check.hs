{-# LINE 1 "Rewriting/Check.hs.drift" #-}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Rewriting.Check where

import Rewriting.TRS
import Rewriting.Overlap

import qualified Machine.Numerical.Config as C

import Autolib.Size
import Autolib.Set
import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import qualified Autolib.Reporter.Set

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Check = Left_Linear
           | Linear
           | Non_Overlapping
           | Constructor
           | Max_Rules Int
           | Max_Size Int
           | Max_Symbols Int
           | Max_Variables Int
    deriving ( Eq, Ord, Typeable )


instance(Symbol c, Symbol v) => C.Check Check ( TRS v c ) where

    check Non_Overlapping trs = do
        inform $ text "Das System soll nicht-überlappend sein."
        sequence_ $ do
            ol <- overlaps trs
            return $ reject $ toDoc ol

    check Left_Linear trs = do
        inform $ text "Das System soll links-linear sein."
        sequence_ $ do
            ( k, rule ) <- zip [0..] $ regeln trs
            return $ do
                inform $ text "prüfe Regel" <+> toDoc rule 
                linear ( text "linke Seite der Regel" ) ( lhs rule )

    check Linear trs = do
        inform $ text "Das System soll linear sein."
        sequence_ $ do
            ( k, rule ) <- zip [0..] $ regeln trs
            return $ do
                inform $ text "prüfe Regel" <+> toDoc rule 
                linear ( text "linke  Seite der Regel" ) ( lhs rule )
                linear ( text "rechte Seite der Regel" ) ( rhs rule )
                let lvs = vars ( lhs rule ) ; rvs = vars ( rhs rule )
                Autolib.Reporter.Set.subeq
                    ( text "Menge der Variablen in der rechten Seite:", rvs )
                    ( text "Menge der Variablen in der linken Seite:", lvs )

    check ( Max_Rules n ) trs = 
        bounder ( text "Anzahl der Regeln" ) n ( length $ regeln trs )
    check ( Max_Size n ) trs = 
        bounder ( text "Größe einer Regelseite" ) n 
            ( maximum $ 0 : map size ( terms trs ) )
    check ( Max_Symbols n ) trs = 
        bounder ( text "Anzahl verschiedener Symbole" ) n 
            ( cardinality $ unionManySets $ map syms $ terms trs )
    check ( Max_Variables n ) trs = 
        bounder ( text "Anzahl verschiedener Variablen" ) n 
            ( cardinality $ unionManySets $ map vars $ terms trs )
    check ch trs = do
        reject $ vcat
               [ text "Test für" <+> toDoc ch <+> text "nicht implementiert."
               , text "(please file a bug report)"
               ]

bounder :: Doc -> Int -> Int -> Reporter ()
bounder name bound value = do
    inform $ vcat
            [ name
            , text "erlaubt  :" <+> toDoc bound
            , text "vorhanden:" <+> toDoc value
            ]
    assert ( value <= bound ) $ text "OK?"

mehrfache_variablen t = do
    ( v, c ) <- fmToList $ addListToFM_C (+) emptyFM $ do
            p <- varpos t
            let Var v = peek t p
            return ( v, 1 )
    guard $ c > 1
    return v

linear name t = do
   let vs = mehrfache_variablen t
   when ( not $ null vs ) $ reject $ vcat 
        [ text "in diesem Term" <+> parens name
        , nest 4 $ toDoc t
        , text "kommen diese Variablen  mehrfach vor:"
        , nest 4 $ toDoc vs
        ]
 
terms trs = do rule <- regeln trs ; [ lhs rule, rhs rule ]

{-! for Check derive: Reader, ToDoc, Haskell2Xml !-}

-- local variables:
-- mode: haskell
-- end;
instance Reader Check where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
                      ((do my_reserved "Left_Linear"
                           return (Left_Linear))
                       <|>
                       (do my_reserved "Linear"
                           return (Linear))
                       <|>
                       (do my_reserved "Non_Overlapping"
                           return (Non_Overlapping))
                       <|>
                       (do my_reserved "Constructor"
                           return (Constructor))
                       <|>
                       (do guard (d < 9)
                           my_reserved "Max_Rules"
                           aa <- readerPrec 9
                           return (Max_Rules aa))
                       <|>
                       (do guard (d < 9)
                           my_reserved "Max_Size"
                           aa <- readerPrec 9
                           return (Max_Size aa))
                       <|>
                       (do guard (d < 9)
                           my_reserved "Max_Symbols"
                           aa <- readerPrec 9
                           return (Max_Symbols aa))
                       <|>
                       (do guard (d < 9)
                           my_reserved "Max_Variables"
                           aa <- readerPrec 9
                           return (Max_Variables aa)))

instance ToDoc Check where
    toDocPrec d (Left_Linear) = text "Left_Linear"
    toDocPrec d (Linear) = text "Linear"
    toDocPrec d (Non_Overlapping) = text "Non_Overlapping"
    toDocPrec d (Constructor) = text "Constructor"
    toDocPrec d (Max_Rules aa) = docParen (d >= 10)
              (text "Max_Rules" </> fsep [toDocPrec 10 aa])
    toDocPrec d (Max_Size aa) = docParen (d >= 10)
              (text "Max_Size" </> fsep [toDocPrec 10 aa])
    toDocPrec d (Max_Symbols aa) = docParen (d >= 10)
              (text "Max_Symbols" </> fsep [toDocPrec 10 aa])
    toDocPrec d (Max_Variables aa) = docParen (d >= 10)
              (text "Max_Variables" </> fsep [toDocPrec 10 aa])

{-
instance Haskell2Xml Check where
    toHType v =
        Defined "Check" []
                [Constr "Left_Linear" [] [],Constr "Linear" [] [],
                 Constr "Non_Overlapping" [] [],Constr "Constructor" [] [],
                 Constr "Max_Rules" [] [toHType aa],
                 Constr "Max_Size" [] [toHType ab],
                 Constr "Max_Symbols" [] [toHType ac],
                 Constr "Max_Variables" [] [toHType ad]]
      where
        (Max_Rules aa) = v
        (Max_Size ab) = v
        (Max_Symbols ac) = v
        (Max_Variables ad) = v
    fromContents (CElem (Elem constr [] cs):etc)
        | "Non_Overlapping" `isPrefixOf` constr =
            (Non_Overlapping,etc)
        | "Max_Variables" `isPrefixOf` constr =
            (\(ad,_)-> (Max_Variables ad, etc)) (fromContents cs)
        | "Max_Symbols" `isPrefixOf` constr =
            (\(ac,_)-> (Max_Symbols ac, etc)) (fromContents cs)
        | "Max_Size" `isPrefixOf` constr =
            (\(ab,_)-> (Max_Size ab, etc)) (fromContents cs)
        | "Max_Rules" `isPrefixOf` constr =
            (\(aa,_)-> (Max_Rules aa, etc)) (fromContents cs)
        | "Linear" `isPrefixOf` constr =
            (Linear,etc)
        | "Left_Linear" `isPrefixOf` constr =
            (Left_Linear,etc)
        | "Constructor" `isPrefixOf` constr =
            (Constructor,etc)
    toContents v@Left_Linear =
        [mkElemC (showConstr 0 (toHType v)) []]
    toContents v@Linear =
        [mkElemC (showConstr 1 (toHType v)) []]
    toContents v@Non_Overlapping =
        [mkElemC (showConstr 2 (toHType v)) []]
    toContents v@Constructor =
        [mkElemC (showConstr 3 (toHType v)) []]
    toContents v@(Max_Rules aa) =
        [mkElemC (showConstr 4 (toHType v)) (toContents aa)]
    toContents v@(Max_Size ab) =
        [mkElemC (showConstr 5 (toHType v)) (toContents ab)]
    toContents v@(Max_Symbols ac) =
        [mkElemC (showConstr 6 (toHType v)) (toContents ac)]
    toContents v@(Max_Variables ad) =
        [mkElemC (showConstr 7 (toHType v)) (toContents ad)]
-}
--  Imported from other files :-
