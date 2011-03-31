{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}

module Petri.Type where
  
import Autolib.TES.Identifier  
import Autolib.FiniteMap  
import Data.Map ( Map )  
import qualified Data.Map as M
import Autolib.Set

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable  
import Autolib.Size  
  
type Connection s t = 
    ( [s], t, [s] )

type State s = FiniteMap s Int

data Ord s => Capacity s 
    = Unbounded
    | All_Bounded Int
    | Bounded ( FiniteMap s Int )
      deriving ( Typeable )
               
$(derives [makeReader, makeToDoc] [''Capacity])    

data ( Ord s, Ord t ) => Net s t = Net
    { places :: Set s
    , transitions :: Set t 
    , connections :: [ Connection s t ] 
    , capacity :: Capacity s 
    , start :: State s
    }  deriving Typeable
    
$(derives [makeReader, makeToDoc] [''Net])    

newtype Place = Place Int 
           deriving ( Eq, Ord, Typeable, Enum )

instance Reader Place where
    reader = do 
        my_symbol "S" ; n <- reader ; return $ Place n

instance ToDoc Place where
    toDoc ( Place n ) = text "S" <> toDoc n

newtype Transition = Transition Int 
                deriving ( Eq, Ord, Typeable, Enum )

instance Reader Transition where
    reader = do 
        my_symbol "T" ; n <- reader ; return $ Transition n

instance ToDoc Transition where
    toDoc ( Transition n ) = text "T" <> toDoc n

instance Size Transition where size _ = 1