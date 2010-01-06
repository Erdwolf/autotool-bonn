{-# language TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances #-}

module CL.Find where

import CL.Term
import CL.Combinator
import CL.Substitution
import CL.Derivation
import CL.Reduce

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Util.Size

import Autolib.Reporter
import Autolib.ToDoc

import qualified Challenger as C
import Inter.Types
import Data.Typeable

import Autolib.FiniteMap
import Autolib.Set

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S


data Combinatory_Logic_Find = Combinatory_Logic_Find 
    deriving ( Eq, Ord, Show, Read, Typeable )

data Instance = 
     Instance { base :: [ Combinator ]
              , start :: Term
              , domain :: Set Identifier 
              , goal :: Term
              , search_depth :: Int
              }
    deriving ( Typeable )

$(derives [makeToDoc,makeReader] [''Instance])

example_instance :: Instance
example_instance = Instance
        { base = standard_base_for_cl
        , start = read "(A x y)"
        , domain = mkSet [ read "A" ]
        , goal = read "(y x)"
        , search_depth = 5
        }

data Solution = 
     Solution { substitution :: Substitution
              , derivation_signposts :: [ Term ]
              }
    deriving ( Typeable )

instance Size Solution where
    size s = sum $ map size $ M.elems $ substitution s

$(derives [makeToDoc,makeReader] [''Solution])

instance C.Partial Combinatory_Logic_Find Instance Solution where

    describe clf i = vcat
        [ text "Wir betrachten Kombinatorische Logik mit der Basis:"
        , nest 4 $ vcat $ map toDoc $ base i
        , text "Gegeben ist der Term  t:" </> toDoc ( start i )
        , text "Gesucht ist eine Substitution  sigma"
        , text "mit Definitionsbereich:" </> toDoc ( domain i ) 
        , text "so daß man  t sigma  ableiten kann zu:" </> toDoc ( goal i )
        , text "Für die Ableitung müssen Sie ggf. Zwischenschritte (Terme) angeben."
        , text "autotool überprüft diese Schritte durch Breitensuche"
        , text "mit Maximaltiefe" <+> toDoc ( search_depth i )
        ]

    initial clf i = Solution
        { substitution = M.fromList 
                  $ zip ( S.toList $ domain i ) 
                  $ repeat $ some_term $ base i
        , derivation_signposts = [ ]
        }

    partial clf i sol = do

        let yourdom = dom $ substitution sol
        when ( yourdom /= domain i ) $ reject $ vcat
             [ text "Definitionsbereich Ihrer Substitution"
             , nest 4 $ toDoc yourdom
             , text "ist nicht korrekt."
             ]

        forM ( M.toList $ substitution sol ) $ \ (k,v) -> 
             forM ( subterms v ) $ \ s -> do
                let whine = reject $ vcat
                         [ text "Teil der Substitution" </> toDoc (k,v)
                         , text "enthält nicht erlaubtes Symbol" </> toDoc s
                         ]
                case s of
                    App {} -> return ()
                    Sym y  -> when ( not $ elem y $ map name $ base i ) $ whine
        
        return ()

    total clf i sol = do
        let sub = substitution sol
            t = apply sub $ start i
            coms = M.fromList $ do c <- base i ; return ( name c, c )
        check_derivation coms ( search_depth i ) 
            $  [ t ] ++ derivation_signposts sol ++ [ goal i ]


check_derivation coms depth points = do
    forM_ ( zip points $ tail points ) $ \ (p,q) -> do
        inform $ vcat 
               [ text "Suche (kurze) Ableitung von" </> toDoc p
               , text "nach" </> toDoc q
               ]
        case paths_from_to ( step coms ) depth p q of
            w : _ -> do
                inform $ text "gefunden:" </> toDoc w
            [] -> do
                 inform $ text "nicht gefunden."
                 let ps = paths_from ( step coms ) depth p 
                 inform $ text "einige erreichbare Ableitungen sind:"
                       </> ( toDoc $ take 10 $ reverse ps )
                 reject $ empty

some_term base = 
    let xs = map Sym $ map name $ take 6 $ concat $ repeat base
        build (x:y:zs) = build $ zs ++ [ App x y ]
        build [x] = x
    in  build xs

make_fixed :: Make
make_fixed = direct Combinatory_Logic_Find example_instance
