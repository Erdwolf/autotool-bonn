{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module PL.Split.Term.Quiz where

import PL.Data
import PL.Reader
import PL.Signatur hiding ( signatur )
import qualified PL.Signatur
import PL.Struktur
import qualified PL.Interpretation as I
import PL.Semantik
import qualified PL.Roll.Syntax
import qualified PL.Roll.Struktur

import qualified  PL.Split.Term.Fixed as F

import Inter.Quiz
import Inter.Types ( Make )

import Challenger.Partial ( total )
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size

import Data.Typeable
import Data.Maybe ( isJust )
import qualified Data.Set as S
import Data.Ord ( comparing )
import Data.List ( maximumBy )

data Config = 
     Config { signatur :: Signatur 
            , universum_size :: Int  
            , tries_while_generating :: Int  
            , max_terms_per_try :: Int  
            }
     deriving Typeable
     
example_config :: Config              
example_config = Config
    { signatur = PL.Signatur.signatur
                         ( read "p(s(z()),z())" :: Term )   
    , universum_size = 4
    , tries_while_generating = 50 
    , max_terms_per_try = 1000 
    }                        
              
$(derives [ makeReader, makeToDoc ] [ ''Config] )

make :: Make  
make = quiz F.PL_Split_Term example_config

instance Project F.PL_Split_Term ( F.Instance Int, Term ) ( F.Instance Int ) where
    project p ( i, t ) = i    
    
instance Generator F.PL_Split_Term Config ( F.Instance Int, Term ) where 
    generator p conf key = do
        tries <- forM [ 1 .. tries_while_generating conf ] $ \ k -> gen conf
        let candidates = do
                ( i, ts ) <- tries
                t <- ts
                return ( i, t )
        case candidates of
            [] -> error "Aufgabengenerator erfolglos, bitte  Bug Report einsenden."
            cs -> return $ maximumBy ( comparing $ \ (i,t) -> size t ) cs
            
    
gen conf = do
    let u = S.fromList [ 1 .. universum_size conf ]
    a <- PL.Roll.Struktur.roll ( signatur conf ) u    
    b <- PL.Roll.Struktur.mutate a
    let i = F.Instance { F.signatur = signatur conf
                     , F.a_struktur = a             
                     , F.b_struktur = b               
                     }               
        check t = isJust $ result $ total F.PL_Split_Term i t 
    let solutions = filter  check
           $ take ( max_terms_per_try conf )
           $ concat 
           $ PL.Roll.Syntax.terms ( signatur conf )
    return ( i, take 1 solutions )
  
