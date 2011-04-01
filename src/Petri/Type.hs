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
import Autolib.Hash
  
type Connection s t = 
    ( [s], t, [s] )

newtype State s = State ( FiniteMap s Int )
    deriving ( Reader, ToDoc, Typeable, Hash )

instance Ord s => Eq ( State s ) where
    State f == State g = M.filter (/= 0) f == M.filter (/= 0) g

instance Ord s => Ord ( State s ) where
    compare (State f) (State g) = 
      compare (M.filter (/= 0) f ) ( M.filter (/= 0) g)

mark :: Ord s => State s -> s -> Int
mark ( State f ) s = M.findWithDefault 0 s f

data Ord s => Capacity s 
    = Unbounded
    | All_Bounded Int
    | Bounded ( FiniteMap s Int )
      deriving ( Eq, Ord, Typeable )
               

data ( Ord s, Ord t ) => Net s t = Net
    { places :: Set s
    , transitions :: Set t 
    , connections :: [ Connection s t ] 
    , capacity :: Capacity s 
    , start :: State s
    }  deriving ( Typeable, Eq, Ord )
    
$(derives [makeReader, makeToDoc] [''Capacity])    
$(derives [makeReader, makeToDoc] [''Net])    

instance ( Ord s, Ord t, Hash s, Hash t ) => Hash ( Net s t ) where
    hash n = hash ( ( places n, transitions n )
                  , (connections n , start n )
                  )
                  

newtype Place = Place Int 
           deriving ( Eq, Ord, Typeable, Enum, Hash )

instance Reader Place where
    reader = do 
        my_symbol "S" ; n <- reader ; return $ Place n

instance ToDoc Place where
    toDoc ( Place n ) = text "S" <> toDoc n

newtype Transition = Transition Int 
                deriving ( Eq, Ord, Typeable, Enum, Hash )

instance Reader Transition where
    reader = do 
        my_symbol "T" ; n <- reader ; return $ Transition n

instance ToDoc Transition where
    toDoc ( Transition n ) = text "T" <> toDoc n

instance Size Transition where size _ = 1


example :: (Net Place Transition, State Place )
example = ( Net
    { places = mkSet [ Place 1 , Place 2 , Place 3 , Place 4 ]
    , transitions = mkSet [ Transition 1 , Transition 2 , Transition 3 , Transition 4 ]
    , connections = [ ( [ Place 3 ] , Transition 1 , [ Place 2 , Place 3 ] )
                    , ( [ Place 4 ] , Transition 2 , [ Place 3 ] )
                    , ( [ Place 1 ] , Transition 3 , [ Place 4 ] )
                    , ( [ Place 2 ] , Transition 4 , [ Place 1 ] )
                    ]
    , capacity = All_Bounded
                     1
    , start = State $ listToFM
                  [ ( Place 1 , 1 ) ]
    }
    , State $ listToFM [(Place 1,1),(Place 2,1),(Place 3,1),(Place 4,1)]
    )
    
