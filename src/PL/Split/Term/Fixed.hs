{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-} 
{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-} 

module PL.Split.Term.Fixed where

import PL.Data
import PL.Reader
import PL.Tree
import PL.Signatur hiding ( signatur )
import qualified PL.Signatur
import PL.Struktur
import qualified PL.Interpretation as I
import PL.Semantik
import qualified PL.Roll.Syntax

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Set
import Autolib.FiniteMap
import Autolib.Size
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Util.Zufall

import qualified Data.Map as M
import qualified Data.Set as S

import Inter.Types
import Data.Typeable

data PL_Split_Term = PL_Split_Term 
    deriving ( Show, Read, Typeable )

instance OrderScore PL_Split_Term where
    scoringOrder _ = Increasing
    
data Ord u => Instance u =     
     Instance { signatur :: Signatur
              , a_struktur :: Struktur u
              , b_struktur :: Struktur u
              } deriving ( Typeable )  
                         
$(derives [ makeReader, makeToDoc ] [ ''Instance] )
    
instance Verify PL_Split_Term ( Instance Int ) where  
    verify _ i = do
        when ( not $ has_nullary_functions $ signatur i )
             $ reject $ text "Die Signatur enthält keine nullstelligen Funktionssymbole."   
        when ( not $ S.null $ freie_variablen $ signatur i ) 
             $ reject $ text "Die Signatur enthält freie Variablen."

       
instance Partial PL_Split_Term ( Instance Int ) Term where
    report _ i = do
        inform $ vcat
            [ text "Finden Sie einen Term zur Signatur"
            , nest 4 $ toDoc ( signatur i )  
            , text ", der in der Struktur"  
            , nest 4 $ text "A" <+> equals <+> toDoc ( a_struktur i  )
            , text "eine anderen Wert hat" 
            , text "als in der Struktur"
            , nest 4 $ text "B" <+> equals <+> toDoc ( b_struktur i  )
            ]  
            
    initial _ i =  
        let ts = PL.Roll.Syntax.terms ( signatur i )
        in  last $ take 1000
                 $ takeWhile ( \ t -> size t < 10 )    
                 $ concat ts
                 
    partial _ i t = do                 
        inform $ text "Die Baumstruktur des Terms ist"
        peng t
        silent $ do
            inform $ text "paßt der Term zur Signatur?"
            PL.Signatur.check ( signatur i ) t
        
    total _ i t = do    
        let int s = I.Interpretation 
                { I.struktur = s , I.belegung = M.empty }
        inform $ text "Wert des Terms in der Struktur A ..." 
        a_wert <- 
            PL.Semantik.compute ( int $ a_struktur i ) t
        inform $ text "... ist" <+> toDoc a_wert    
        inform $ text "Wert des Terms in der Struktur B ..." 
        b_wert <- 
            PL.Semantik.compute ( int $ b_struktur i ) t
        inform $ text "... ist" <+> toDoc b_wert    
        assert ( a_wert /= b_wert ) 
               $ text "Werte sind verschieden?"
          
make :: Make          
make = direct PL_Split_Term example_instance

example_instance :: Instance Int
example_instance = 
  Instance { signatur = PL.Signatur.signatur
                         ( read "s(z())" :: Term )   
               , a_struktur = Struktur
                   { universum = S.fromList [0,1]
                   , functions = M.fromList 
                        $ read "[ (s,{(0,1),(1,0)}), (z,{0})]" 
                   , predicates = M.fromList []
                   }             
               , b_struktur = Struktur
                   { universum = S.fromList [0,1]
                   , functions = M.fromList 
                        $ read "[ (s,{(0,1),(1,1)}), (z,{0})]" 
                   , predicates = M.fromList []
                   }             
               }  
  
