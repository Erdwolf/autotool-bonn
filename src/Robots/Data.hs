{-# LINE 1 "Robots/Data.hs.drift" #-}
{-# OPTIONS -fglasgow-exts #-}

module Robots.Data where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import Text.XML.HaXml.Haskell2Xml
import Data.Typeable

data Robots = Robots deriving ( Typeable )
data Robots_Inverse = Robots_Inverse deriving ( Typeable )

{-! for Robots derive : Reader, ToDoc, Haskell2Xml !-}
{-! for Robots_Inverse derive : Reader, ToDoc, Haskell2Xml !-}

type Position = ( Integer, Integer )

data Robot = Robot { name :: String
		   , position :: Position
		   , ziel :: Maybe Position
		   }
     deriving ( Eq, Ord, Typeable )

{-! for Robot derive : Reader, ToDoc, Haskell2Xml !-}

instance Hash Robot where
    hash r = hash ( name r, position r, ziel r )

data Richtung = N | O | S | W 
     deriving ( Eq, Ord, Enum, Bounded, Typeable )

{-! for Richtung derive : Reader, ToDoc, Haskell2Xml !-}

richtungen :: [ Richtung ]
richtungen = [ minBound .. maxBound ]

type Zug = ( String, Richtung )

instance Size Zug where size _ = 1

-- local variables:
-- mode: haskell
-- end:
instance Reader Richtung where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
                      ((do my_reserved "N"
                           return (N))
                       <|>
                       (do my_reserved "O"
                           return (O))
                       <|>
                       (do my_reserved "S"
                           return (S))
                       <|>
                       (do my_reserved "W"
                           return (W)))

instance ToDoc Richtung where
    toDocPrec d (N) = text "N"
    toDocPrec d (O) = text "O"
    toDocPrec d (S) = text "S"
    toDocPrec d (W) = text "W"

instance Haskell2Xml Richtung where
    toHType v =
        Defined "Richtung" []
                [Constr "N" [] [],Constr "O" [] [],Constr "S" [] [],
                 Constr "W" [] []]
    fromContents (CElem (Elem constr [] cs):etc)
        | "W" `isPrefixOf` constr =
            (W,etc)
        | "S" `isPrefixOf` constr =
            (S,etc)
        | "O" `isPrefixOf` constr =
            (O,etc)
        | "N" `isPrefixOf` constr =
            (N,etc)
    toContents v@N =
        [mkElemC (showConstr 0 (toHType v)) []]
    toContents v@O =
        [mkElemC (showConstr 1 (toHType v)) []]
    toContents v@S =
        [mkElemC (showConstr 2 (toHType v)) []]
    toContents v@W =
        [mkElemC (showConstr 3 (toHType v)) []]

instance Reader Robot where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
                      ((do guard (d < 9)
                           my_reserved "Robot"
                           my_braces ((do my_reserved "name"
                                          my_equals
                                          aa <- readerPrec 0
                                          my_comma
                                          my_reserved "position"
                                          my_equals
                                          ab <- readerPrec 0
                                          my_comma
                                          my_reserved "ziel"
                                          my_equals
                                          ac <- readerPrec 0
                                          return (Robot aa ab ac)))))

instance ToDoc Robot where
    toDocPrec d (Robot aa ab ac) = docParen (d >= 10)
              (text "Robot" </> dutch_record
               [text "name" <+> equals <+> toDocPrec 0 aa,
                text "position" <+> equals <+> toDocPrec 0 ab,
                text "ziel" <+> equals <+> toDocPrec 0 ac])

instance Haskell2Xml Robot where
    toHType v =
        Defined "Robot" []
                [Constr "Robot" [] [toHType aa,toHType ab,toHType ac]]
      where
        (Robot aa ab ac) = v
    fromContents (CElem (Elem constr [] cs):etc)
        | "Robot" `isPrefixOf` constr =
            (\(aa,cs00)-> (\(ab,cs01)-> (\(ac,_)-> (Robot aa ab ac, etc))
                                        (fromContents cs01))
                          (fromContents cs00))
            (fromContents cs)
    toContents v@(Robot aa ab ac) =
        [mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
                                                     toContents ab,toContents ac])]

instance Reader Robots_Inverse where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
                      ((do my_reserved "Robots_Inverse"
                           return (Robots_Inverse)))

instance ToDoc Robots_Inverse where
    toDocPrec d (Robots_Inverse) = text "Robots_Inverse"

instance Haskell2Xml Robots_Inverse where
    toHType v =
        Defined "Robots_Inverse" [] [Constr "Robots_Inverse" [] []]
    fromContents (CElem (Elem constr [] cs):etc)
        | "Robots_Inverse" `isPrefixOf` constr =
            (Robots_Inverse,etc)
    toContents v@Robots_Inverse =
        [mkElemC (showConstr 0 (toHType v)) []]

instance Reader Robots where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
                      ((do my_reserved "Robots"
                           return (Robots)))

instance ToDoc Robots where
    toDocPrec d (Robots) = text "Robots"

instance Haskell2Xml Robots where
    toHType v =
        Defined "Robots" [] [Constr "Robots" [] []]
    fromContents (CElem (Elem constr [] cs):etc)
        | "Robots" `isPrefixOf` constr =
            (Robots,etc)
    toContents v@Robots =
        [mkElemC (showConstr 0 (toHType v)) []]

--  Imported from other files :-
