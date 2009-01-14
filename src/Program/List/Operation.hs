module Program.List.Operation where

import qualified Program.List.Value as V
import qualified Program.List.Store as S

import Autolib.Reporter hiding ( result )
import Autolib.ToDoc

data Type = Void 
         | Index -- ^ 0 to length - 1
         | Index' -- ^ 0 to length (used to List.add)
         | Element -- ^ the element type of the collection
    deriving ( Eq, Show )

instance ToDoc Type where toDoc = text . show

data Operation =
     Operation { object :: V.Name
               , method :: String
               , args :: [ Type ]
               , result :: Type 
               , semantics :: S.Key -> [ S.Key ] 
                           -> S.ReporterST S.Key
               }

ops :: [ Operation ]
ops = 
    [ Operation { object = V.List , method = "add"
                , args = [ Index', Element ], result = Void
                , semantics = \ self [ a1, a2 ] -> do
                      sc <- S.access self
                      S.Scalar i <- S.access a1
                      let ( pre, post ) = splitAt ( fromIntegral i ) $ S.contents sc
                      S.update self $ sc { S.contents = pre ++ a2 : post }
                      S.void
                }
    ] 

