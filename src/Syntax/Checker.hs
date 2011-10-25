{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax.Checker {- (check) -} where

import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow (second)

import Syntax.Syntax
import Syntax.Transformer


forks = foldr1 Fork

lang1  =
    [("Word", forks [ Terminal "a"
                    , Terminal "b" `Chain` Terminal "b"
                    , Empty
                    ])
    ]

lang2 =
    [("B",    forks [ Terminal "b" `Chain`  Symbol "B"
                    , Terminal "b"
                    ])
    ]

lang3 =
    [("Word", forks [ Symbol "B" `Chain` Terminal "b"
                    ])
    ,("B",    forks [ Terminal "b" `Chain` Symbol "B"
                    , Terminal "b"
                    ])
    ]

lang4 =
    [("Word", forks [ Terminal "a"
                    , Symbol "B" `Chain` Terminal "b"
                    , Empty
                    ])
    ,("B",    forks [ Terminal "b" `Chain` Symbol "B"
                    , Terminal "b"
                    ])
    ]

lang5 =
    [("Word", Loop (Terminal "a") `Chain` Loop (Terminal "b"))
    ]

lang6 =
    [ ( "S", Fork (Terminal "a")
                   (Fork (Symbol "B")
                         Empty))
    , ( "B", Fork (Chain (Terminal "b")
                         (Terminal "b"))
                  (Fork (Symbol "B")
                        Empty))
    ]

lang7 =
    [ ( "S", Fork (Terminal "a")
                  (Fork
                       (Symbol "B")
                       Empty))
    , ( "B", Fork (Chain (Terminal "b")
                         (Terminal "b"))
                  (Fork (Symbol "B") Empty))
    ]


toParser :: Graph -> LanguageParser ()
toParser (Terminal t)  = string t
toParser (Symbol s)    = ask >>= msum . lookupAll s
toParser (Fork g1 g2)  = toParser g1 `mplus` toParser g2
toParser (Chain g1 g2) = toParser g1 >> toParser g2
toParser (Loop g)      = toParser g >> (toParser (Loop g) `mplus` return ())
toParser Empty         = return ()

lookupAll key = map snd . filter ((key==).fst)


type Env = [(String, LanguageParser ())]

newtype LanguageParser a = LP
       { unwrapLP :: StateT String
                     (ReaderT Env [])
                     a
       }
 deriving ( Monad
          , MonadPlus
          , MonadState String
          , MonadReader Env
          )

runLP = (runReaderT .) . evalStateT . unwrapLP

string :: String -> LanguageParser ()
string (x:xs) = char x >> string xs
string []     = return ()

char :: Char -> LanguageParser ()
char c = do
  (x:xs) <- get
  guard (x == c)
  put xs

eof :: LanguageParser ()
eof = do
  "" <- get
  return ()



check :: Language -> String -> Bool
check lang word =
    let parsers = (map (second toParser) . transform) lang in
    if null parsers
        then False
        else let startSymbol = fst $ head $ lang
             in not $ null $ runLP (msum (lookupAll startSymbol parsers) >> eof) word parsers
