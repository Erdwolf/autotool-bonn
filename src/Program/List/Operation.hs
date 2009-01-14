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
                      i <- S.access a1
                      let ( pre, post ) = splitAt ( fromIntegral $ S.scontents i ) 
                                        $ S.contents sc
                      S.update self $ sc { S.contents = pre ++ a2 : post }
                      S.void
                }
    , Operation { object = V.List , method = "remove"
                , args = [ Index ], result = Element
                , semantics = \ self [ a1 ] -> do
                      sc <- S.access self
                      i <- S.access a1
                      let ( pre, a2 : post ) = splitAt ( fromIntegral $ S.scontents i ) 
                                        $ S.contents sc
                      S.update self $ sc { S.contents = pre ++ post }
                      return a2
                }
    , Operation { object = V.List , method = "get"
                , args = [ Index ], result = Element
                , semantics = \ self [ a1 ] -> do
                      sc <- S.access self
                      i <- S.access a1
                      let a2 = S.contents sc !! ( fromIntegral $ S.scontents i )
                      return a2
                }
    , Operation { object = V.List , method = "size"
                , args = [ ], result = Index
                , semantics = \ self [ ] -> do
                      sc <- S.access self
                      S.scalar $ fromIntegral $ length $ S.contents sc
                }

    ] 

