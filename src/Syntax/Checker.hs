{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax.Checker (check, lookupAll) where

import Syntax.Syntax
import Syntax.Transformer

import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow (second)



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

lang8 =
    [ ("A", Symbol "C" `Chain` Symbol "D")
    , ("B", Fork (Terminal "a") (Terminal "b"))
    , ("C", Terminal "d")
    , ("D", Fork Empty (Terminal "d"))
    ]

lang9 =
    [ ("A", Symbol "A" `Chain` Loop (Terminal "c") `Chain` Terminal "d")
    , ("B", Symbol "B")
    , ("C", Symbol "b")
    , ("D", Symbol "a" `Chain` Fork (Terminal "d") (Symbol "B"))
    ]

lang10 =
    [ ("A", Loop (Symbol "C" `Chain` Terminal "a"))
    , ("C", Loop (Loop (Terminal "c")))
    ]

lang11 =
    [ ("A", Symbol "A" `Chain` Terminal "a")
    , ("A", Terminal "b")
    ]

lang12 =
    [ ( "A", Fork (Chain (Chain (Symbol "B") (Symbol "A")) (Terminal "d"))
                  Empty)
    , ( "B", Fork (Chain (Symbol "C") (Terminal "c"))
                   Empty)
    , ( "C", Terminal "a")
    ]

lang12' =
    [ ("A", Symbol "B" `Chain` Symbol "A" `Chain` Terminal "d")
    , ("A", Empty)
    , ("B", Symbol "C" `Chain` Terminal "c")
    , ("B", Empty)
    , ("C", Terminal "a")
    ]

lang12'' =
    [ ("A", Symbol "B" `Chain` Symbol "A" `Chain` Terminal "d")
    , ("A", Symbol "B" `Chain` Terminal "d")
    , ("A", Symbol "A" `Chain` Terminal "d")
    , ("A", Terminal "d")
    , ("B", Symbol "C" `Chain` Terminal "c")
    , ("C", Terminal "a")
    ]

lang13 = [("A",Fork (Terminal "c") (Fork (Chain (Symbol "B") (Terminal "b")) Empty)),("B",Chain (Symbol "D") (Terminal "c")),("C",Chain (Terminal "d") (Terminal "a")),("D",Loop (Chain (Symbol "A") (Symbol "A")))]


toParser :: Graph -> LanguageParser ()
toParser (Terminal t)  = string t
toParser (Symbol s)    = ask >>= msum . lookupAll s
toParser (Fork g1 g2)  = toParser g1 `mplus` toParser g2
toParser (Chain g1 g2) = toParser g1 >> toParser g2
toParser (Loop g)      = toParser g >> (toParser (Loop g) `mplus` return ())
toParser Empty         = return ()

lookupAll key = liftM snd . filterMP ((key==).fst)

filterMP f = msum . map return . filter f

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
check = check' . transform

check' :: Language -> String -> Bool
check' lang word =
    let parsers = map (second toParser) lang in
    if null parsers
        then False
        else let startSymbol = fst $ head $ lang
             in not $ null $ runLP (msum (lookupAll startSymbol parsers) >> eof) word parsers
