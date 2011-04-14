{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Program.Cexp.Type where

import Autolib.Reader
import Text.ParserCombinators.Parsec.Expr hiding ( Prefix, Postfix )
import qualified Text.ParserCombinators.Parsec.Expr as E
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Autolib.ToDoc hiding ( Full )
import Data.Typeable
import Data.Tree

data Exp = Symbol Identifier
         | Literal    Integer
         | Apply Op [ Exp ]
    deriving Typeable
    
newtype Identifier = Identifier String deriving ( Eq, Ord )
instance Reader Identifier where reader = fmap Identifier $ identifier tp
instance ToDoc Identifier where toDoc (Identifier cs) = text cs

example :: ( Integer, Exp )
example = ( 8, read "x=3 , ++x + ++x" )

-----------------------------------------------------

type Position = [ Int ]

positions :: Exp -> [ Position ]
positions x = [] : case x of
    Apply _ ys -> do
        (k,y) <- zip [0..] ys
        ks <- positions y
        return $ k : ks
    _ -> []

peek :: Monad m => Position -> Exp -> m Exp
peek [] x = return x
peek (k:ks) (Apply _ ys) | 0 <= k && k < length ys = peek ks $ ys !! k
peek _ _ = fail "peek: not a position"

poke :: Monad m => Position -> Exp -> Exp -> m Exp
poke [] v x = return v
poke (k:ks) v (Apply op ys) | 0 <= k && k < length ys = do
    let ( pre, this: post) = splitAt k ys
    that <- poke ks v this
    return $  Apply op $ pre ++ that : post
poke _ _ _ = fail "poke: not a position"

-----------------------------------------------------


tree :: Exp -> Tree String
tree x = case x of
    Apply op ys -> Node ( name op ) $ reverse $ map tree ys
    _ -> Node ( show x ) []


-----------------------------------------------------
     
instance ToDoc Exp where
    toDocPrec p x = case x of
        Symbol cs -> toDoc cs
        Literal i -> toDoc i
        Apply op args -> ( if p > prec op then Autolib.ToDoc.parens else id )
            $ case ( name op, optype op, args ) of
                 ( n, Pre, [a] ) -> 
                     text n <+> toDocPrec ( prec op ) a
                 ( n, Post, [a] ) -> 
                     toDocPrec ( prec op ) a <+> text n
                 ( n, In AssocLeft , [a,b] ) -> 
                     toDocPrec ( prec op ) a <+> text n
                        <+> toDocPrec ( prec op + 1 ) b
                 ( n, In AssocRight , [a,b] ) -> 
                     toDocPrec ( prec op + 1 ) a <+> text n
                        <+> toDocPrec ( prec op  ) b
                 ( "?:", _ , [a,b,c] ) -> fsep
                     [ toDocPrec ( prec op ) a 
                     , text "?", toDocPrec ( prec op  ) b
                     , text ":", toDocPrec ( prec op  ) c
                     ]

-----------------------------------------------------

tp = makeTokenParser emptyDef

instance Reader Exp where
    reader = buildExpressionParser
        ( do ops <- opss
             return $ do
                 op <- ops
                 return $ case optype op of
                     Pre -> E.Prefix $ do 
                         try $ symbol tp ( name op ) 
                         return $ \ x -> Apply op [x]
                     Post -> E.Postfix $ do 
                         try $ symbol tp ( name op ) 
                         return $ \ x -> Apply op [x]
                     In a -> E.Infix ( do
                         try $ symbol tp $ name op
                         return $ \ x y -> case ( name op, y ) of
                             ( "?", Apply op' [l,r] ) | name op' == ":" -> 
                                  Apply branch [ x, l, r ]
                             _ -> Apply op [x, y] 
                       ) a
        ) factor

factor =    fmap Symbol reader
        <|> fmap Literal ( natural  tp )
        <|> Text.ParserCombinators.Parsec.Token.parens tp  reader

-----------------------------------------------------------

data Optype = Pre | In Assoc | Post 

data Strict = Full -- ^ all args must be evaluated
     | Head -- ^ head must be evaluated before all others
     | None

data Op = Op { prec :: Int
             , optype :: Optype
             , name :: String
             , oper :: Oper
             }


data Oper = Prefix Oper
          | Postfix Oper
          | Assign | AssignWith Oper
          | Shortcut Oper
          | Bitwise Oper
         | Increment | Decrement
         | Less | LessEquals | Equals | NotEquals | GreaterEquals | Greater
         | Plus | Minus | Times | Divide | Remainder
         | ShiftLeft | ShiftRight
         | And | Or | Xor | Not
         | Branch | Question | Colon
         | Sequence
    deriving ( Eq )

branch :: Op
branch = Op { name = "?:", prec = 90, optype = In AssocRight, oper = Branch }

-- | in groups of equal precedence, starting highest
opss :: [[ Op ]]
opss = [ [ Op { name = "!", prec = 200, optype = Pre, oper = Not  }
        , Op { name = "~", prec = 200, optype = Pre, oper = Bitwise Not }
        , Op { name = "++", prec = 200, optype = Pre, oper = Prefix Increment }
        , Op { name = "--", prec = 200, optype = Pre , oper = Prefix Decrement }
        , Op { name = "++", prec = 200, optype = Post, oper = Postfix Increment  }
        , Op { name = "--", prec = 200, optype = Post , oper = Postfix Decrement }
        , Op { name = "+", prec = 200, optype = Pre , oper = Prefix Plus }
        , Op { name = "-", prec = 200, optype = Pre , oper = Prefix Minus }
        ]
      , [ Op { name = "*", prec = 190, optype = In AssocLeft, oper = Times  }
        , Op { name = "/", prec = 190, optype = In AssocLeft, oper = Divide  }
        , Op { name = "%", prec = 190, optype = In AssocLeft, oper = Remainder  }
        ]
      , [ Op { name = "+", prec = 180, optype = In AssocLeft, oper = Plus  }
        , Op { name = "-", prec = 180, optype = In AssocLeft, oper = Minus  }
        ]
      , [ Op { name = "<<", prec = 170, optype = In AssocLeft, oper = ShiftLeft  }
        , Op { name = ">>", prec = 170, optype = In AssocLeft, oper = ShiftRight  }
        ]
      , [ Op { name = "<", prec = 160, optype = In AssocLeft , oper = Less }
        , Op { name = "<=", prec = 160, optype = In AssocLeft, oper = LessEquals  }
        , Op { name = ">", prec = 160, optype = In AssocLeft , oper = Greater }
        , Op { name = ">=", prec = 160, optype = In AssocLeft, oper = GreaterEquals  }
        ]
      , [ Op { name = "==", prec = 150, optype = In AssocLeft, oper = Equals  }
        , Op { name = "!=", prec = 150, optype = In AssocLeft, oper = NotEquals  }
        ]
      , [ Op { name = "&", prec = 140, optype = In AssocLeft, oper = Bitwise And  }
        ]
      , [ Op { name = "^", prec = 130, optype = In AssocLeft, oper = Bitwise Xor  }
        ]
      , [ Op { name = "|", prec = 120, optype = In AssocLeft, oper = Bitwise Or  }
        ]
      , [ Op { name = "&&", prec = 110, optype = In AssocLeft, oper = Shortcut And }
        ]
      , [ Op { name = "||", prec = 100, optype = In AssocLeft, oper = Shortcut Or }
        ]
      , [ Op { name = "?", prec = 90, optype = In AssocRight, oper = Question }
        , Op { name = ":", prec = 90, optype = In AssocRight, oper = Colon } -- only for parsing
        ]
      , [ Op { name = "=", prec = 80, optype = In AssocRight, oper = Assign  }
        , Op { name = "+=", prec = 80, optype = In AssocRight, oper = AssignWith Plus  }
        , Op { name = "-=", prec = 80, optype = In AssocRight, oper = AssignWith Minus  }
        , Op { name = "*=", prec = 80, optype = In AssocRight, oper = AssignWith Times  }
        , Op { name = "/=", prec = 80, optype = In AssocRight, oper = AssignWith Divide  }
        , Op { name = "%=", prec = 80, optype = In AssocRight, oper = AssignWith Remainder  }
        , Op { name = "&=", prec = 80, optype = In AssocRight, oper = AssignWith ( Bitwise And )  }
        , Op { name = "^=", prec = 80, optype = In AssocRight, oper = AssignWith ( Bitwise Xor )  }
        , Op { name = "|=", prec = 80, optype = In AssocRight, oper = AssignWith ( Bitwise Or )  }
        , Op { name = "<<=", prec = 80, optype = In AssocRight, oper = AssignWith ( ShiftLeft )  }
        , Op { name = ">>=", prec = 80, optype = In AssocRight, oper = AssignWith ( ShiftRight )  }
        ]
      , [ Op { name = ",", prec = 70, optype = In AssocLeft, oper = Sequence }
        ]
      ]


$(derives [makeToDoc, makeReader] [''Oper])
