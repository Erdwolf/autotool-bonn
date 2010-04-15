module Program.List.Operation where

import qualified Program.List.Value as V
import qualified Program.List.Store as S

import Autolib.Reporter hiding ( result )
import Autolib.ToDoc

import Control.Monad.State
import Data.Ix ( inRange )

data Type = Void 
         | Index -- ^ 0 to length - 1
         | Index' -- ^ 0 to length (used to List.add)
         | Element -- ^ the element type of the collection
    deriving ( Eq, Show, Enum, Bounded )

instance ToDoc Type where toDoc = text . show

data Operation =
     Operation { object :: V.Name -- ^ the type of "this"
               , method :: String
               , args :: [ Type ]
               , result :: Type 
               , semantics :: S.Key -> [ S.Key ] 
                           -> S.ReporterST S.Key
               , toplevel :: Bool
               }

ops :: [ Operation ]
ops = list_ops ++ stack_ops ++ queue_ops

add = Operation { object = V.List , method = "add"
                , args = [ Index', Element ], result = Void
                , semantics = \ self [ a1, a2 ] -> do
                      sc <- S.access self
                      i <- S.access a1
                      let k = fromIntegral $ S.scontents i 
                          c = S.contents sc
                          bnd = (0, length c)
                      when ( not $ inRange bnd  k ) $ lift $ reject
                           $ text "index" <+> toDoc k <+> text "not in range" <+> toDoc bnd
                      let ( pre, post ) = splitAt k c
                      S.update self $ sc { S.contents = pre ++ a2 : post }
                      S.void
                , toplevel = True
                }
remove = Operation { object = V.List , method = "remove"
                , args = [ Index ], result = Element
                , semantics = \ self [ a1 ] -> do
                      sc <- S.access self
                      i <- S.access a1
                      let k = fromIntegral $ S.scontents i 
                          c = S.contents sc
                          bnd = (0,length c - 1)
                      when ( not $ inRange bnd  k ) $ lift $ reject
                           $ text "index" <+> toDoc k <+> text "not in range" <+> toDoc bnd
                      let ( pre, a2 : post ) = splitAt k c
                      S.update self $ sc { S.contents = pre ++ post }
                      return a2
                , toplevel = True
                }

list_ops =
    [ add
    , remove
    , Operation { object = V.List , method = "get"
                , args = [ Index ], result = Element
                , semantics = \ self [ a1 ] -> do
                      sc <- S.access self
                      i <- S.access a1
                      let k = fromIntegral $ S.scontents i 
                          c = S.contents sc
                          bnd = (0,length c - 1)
                      when ( not $ inRange bnd  k ) $ lift $ reject
                           $ text "index" <+> toDoc k <+> text "not in range" <+> toDoc bnd
                      let a2 = c !! k
                      return a2
                , toplevel = False
                }
    , Operation { object = V.List , method = "size"
                , args = [ ], result = Index
                , semantics = \ self [ ] -> do
                      sc <- S.access self
                      S.scalar $ fromIntegral $ length $ S.contents sc
                , toplevel = False
                }

    ] 

stack_ops = 
    [ Operation { object = V.Stack , method = "push"
                , args = [ Element ], result = Void
                , semantics = \ self [ a2 ] -> do
                      sc <- S.access self
                      S.update self $ sc { S.contents = S.contents sc ++ [ a2 ] }
                      S.void
                , toplevel = True
                }
    , Operation { object = V.Stack , method = "pop"
                , args = [ ], result = Element
                , semantics = \ self [ ] -> do
                      sc <- S.access self
                      lift $ when ( null $ S.contents sc ) $ reject
                           $ text "pop: Stack darf nicht leer sein"
                      S.update self $ sc { S.contents = init $ S.contents sc }
                      return $ last $ S.contents sc
                , toplevel = True
                }
    , Operation { object = V.Stack , method = "peek"
                , args = [ ], result = Element
                , semantics = \ self [ ] -> do
                      sc <- S.access self
                      sc <- S.access self
                      lift $ when ( null $ S.contents sc ) $ reject
                           $ text "peek: Stack darf nicht leer sein"
                      return $ last $ S.contents sc
                , toplevel = False
                }
    , Operation { object = V.Stack , method = "size"
                , args = [ ], result = Index
                , semantics = \ self [ ] -> do
                      sc <- S.access self
                      S.scalar $ fromIntegral $ length $ S.contents sc
                , toplevel = False
                }

    ] 

queue_ops = 
    [ Operation { object = V.Queue , method = "add"
                , args = [ Element ], result = Void
                , semantics = \ self [ a2 ] -> do
                      sc <- S.access self
                      S.update self $ sc { S.contents = S.contents sc ++ [ a2 ] }
                      S.void
                , toplevel = True
                }
    , Operation { object = V.Queue , method = "remove"
                , args = [ ], result = Element
                , semantics = \ self [ ] -> do
                      sc <- S.access self
                      lift $ when ( null $ S.contents sc ) $ reject
                           $ text "remove: Queue darf nicht leer sein"
                      S.update self $ sc { S.contents = tail $ S.contents sc }
                      return $ head $ S.contents sc
                , toplevel = True
                }
    , Operation { object = V.Queue , method = "size"
                , args = [ ], result = Index
                , semantics = \ self [ ] -> do
                      sc <- S.access self
                      S.scalar $ fromIntegral $ length $ S.contents sc
                , toplevel = False
                }

    ] 
