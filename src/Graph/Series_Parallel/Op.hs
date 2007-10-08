module Graph.Series_Parallel.Op where

import Graph.Series_Parallel.Type
import Autolib.Graph.Type
import qualified Autolib.Graph.Ops as GO
import qualified Autolib.Graph.Basic as GB

import Autolib.FiniteMap
import Data.Maybe

point :: GraphC Int => STGraph Int
point = STGraph
     { source = 0
     , target = 0
     , contents = GB.path [0]
     }

edge :: GraphC Int => STGraph Int
edge = STGraph
     { source = 0
     , target = 1
     , contents = GB.path [0,1]
     }

-- | identifiziert jeweils sourcen und targets
parallel :: ( GraphC a, GraphC b,  GraphC ( Threeway a b ) )
         => STGraph a -> STGraph b
         -> STGraph ( Threeway a b )
parallel g h =
    union0 ( export This ( Both 0 ) ( Both 1 ) g )
           ( export That ( Both 0 ) ( Both 1 ) h )
           ( Both 0 )
           ( Both 1 )

-- | identif. target vom ersten mit source vom zweiten
serial :: ( GraphC a, GraphC b, GraphC ( Threeway a b ) )
         => STGraph a -> STGraph b
         -> STGraph ( Threeway a b )
serial g h =
    union0 ( export This ( Both 0 ) ( Both 1 ) g )
           ( export That ( Both 1 ) ( Both 2 ) h )
           ( Both 0 )
           ( Both 2 )


normalize :: ( GraphC a , GraphC Int )
           => STGraph a -> STGraph Int
normalize g =
    let ks = source g : target g : ( setToList $ knoten $ contents g )
        fm = listToFM 
           $ zip ks [ 0 .. ]
        fun = fromMaybe ( error $ "STGraph.normalize: " ++ show g ) 
            . lookupFM fm
    in  gmap fun g

gmap :: ( GraphC a, GraphC b ) 
     => ( a -> b ) -> ( STGraph a -> STGraph b )
gmap f g = 
    STGraph { source = f $ source g
            , target = f $ target g
            , contents = GO.gmap f $ contents g
            }

export :: ( GraphC a, GraphC c )
       => ( a -> c ) -> c -> c 
       -> ( STGraph a -> STGraph c )
export fun s t g = 
    let f x = if x == source g then s
              else if x == target g then t
              else fun x
    in  gmap f g

union0 :: GraphC a
       => STGraph a -> STGraph a 
       -> a -- ^ source
       -> a -- ^ target
       -> STGraph a
union0 g h s t =
    STGraph { source = s
            , target = t
            , contents = GO.union0 ( contents g ) 
                                   ( contents h )
            }

    

