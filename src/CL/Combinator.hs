{-# language TemplateHaskell #-}

module CL.Combinator where

import CL.Term

import Autolib.Reader
import Autolib.ToDoc

data Combinator = 
     Combinator { name :: Identifier
                , arity :: Int
                , result :: Term
                }

$(derives [makeToDoc,makeReader] [''Combinator])

standard_base_for_cl :: [ Combinator ]
standard_base_for_cl =
    [ Combinator { name = read "S", arity = 3, result = read "(1 3 (2 3))" }
    , Combinator { name = read "K", arity = 2, result = read "1" }
    , Combinator { name = read "I", arity = 1, result = read "1" }
    ]

standard_base_for_cli :: [ Combinator ]
standard_base_for_cli = 
    [ Combinator { name = read "S", arity = 3, result = read "(1 3 (2 3))" }
    , Combinator { name = read "B", arity = 3, result = read "(1 3 2)" }
    , Combinator { name = read "C", arity = 3, result = read "(1 (2 3))" }
    , Combinator { name = read "I", arity = 1, result = read "1" }
    ]