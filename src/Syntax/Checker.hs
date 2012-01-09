{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax.Checker {- (check) -} where

import Syntax.Syntax
import Syntax.Transformer

import Control.Monad.Writer
import Data.List (nub)
import Data.List
import Data.Array


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

lang13 =
    [ ("A", Fork (Terminal "c")
                 (Fork (Symbol "B" `Chain` Terminal "b")
                        Empty))
    , ("B", Symbol "D" `Chain` Terminal "c")
    , ("C", Terminal "d" `Chain` Terminal "a")
    , ("D", Loop (Symbol "A" `Chain` Symbol "A"))
    ]

lang14 =
    [ ("A", Fork (Symbol "B")
                 (Chain (Fork (Terminal "a")
                              Empty)
                        (Terminal "c")))
    , ("B", Chain (Loop (Symbol "D"))
                  (Terminal "b"))
    , ("C", Terminal "d")
    , ("D", Fork (Chain (Symbol "B")
                        (Terminal "b"))
                 Empty)
    ]

words14 = [ "ac", "c" ] ++ [ replicate k 'b' | k <- [1..]\\[2] ]



{-

toParser :: Graph -> LanguageParser ()
toParser (Terminal t)  = string t
toParser (Symbol s)    = ask >>= msum . lookupAll s
toParser (Fork g1 g2)  = toParser g1 `mplus` toParser g2
toParser (Chain g1 g2) = toParser g1 >> toParser g2
toParser (Loop g)      = toParser g >> (toParser (Loop g) `mplus` return ())
toParser Empty         = return ()
-}

lookupAll key = liftM snd . filterMP ((key==).fst)

filterMP f = msum . map return . filter f

{-
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
-}


check_ = check_' 10

check_' :: Int -> Language -> String -> Bool
check_' limit lang word =
    go 0 [[startSymbol]]
 where
    n = length word

    go i ws = if target `elem` ws
               then True
               else let ws' = oneStep ws in
                    if ws' == ws || i >= limit
                       then False
                       else go (succ i) ws'

    target = [ T [c] | c <- word ]

    oneStep ws = nub $ ws ++ filter ((<=n).length.filter terminal) (ws >>= produceNew)

    terminal (T _) = True
    terminal _     = False

    produceNew w = partitions w >>= f

    f (as,b:bs) = ((as++).(++bs)) `liftM` replace b

    replace (N nt) = lookupAll nt productions
    replace x = [[x]]

    startSymbol = N $ fst $ head productions

    productions = map (\(Rule x y) -> (x,y)) $ toGrammar lang

    toGrammar = map toRule . removeForks . removeLoops

    partitions (x:xs) = return ([],x:xs) `mplus` liftM (\(as,bs) -> (x:as,bs)) (partitions xs)
    partitions []     = mzero



check ::Language -> String -> Bool
check = cyk . chomsky . toGrammar . removeForks . removeLoops

--cyk :: [Rule] -> String -> Bool
cyk [] _  = False -- Empty language
cyk rs@(Rule start _:_) []   = not $ null [ () | Rule a [] <- rs, a == start ] -- Empty word
cyk rs@(Rule start _:_) word =
    start `elem` head (head (cyk_table rs word))

cyk_table rs word =
    let n = length word -- at least 1
    in times (n-1) addRow [[ [ a | Rule a [T y] <- rs, x == y ] | x <- map return word ]]
 where
    addRow xsss = [ [ a | (bs,cs) <- yss,  b <- bs, c <- cs, Rule a [N b',N c'] <- rs, b == b', c == c' ] |  yss <- combine xsss ] : xsss
    combine xss = zipWith (zipWith (,)) (map reverse $ transpose xss) (tail (diagonals xss))

-- | Apply a function a certain number of times to an argument.
times :: Int -> (a -> a) -> a -> a
times n _ _ | n < 0 = error "A function can't be applied a negative number of times."
times 0 f x = x
times n f x = f (times (pred n) f x)

diagonals :: [[a]] -> [[a]]
diagonals = transpose . go 0
  where
    go i (xs:xss) = drop i xs : go (i+1) xss
    go _ []       = []


cnf =
    [ Rule "S" [N "A", N "B"]
    , Rule "A" [N "C", N "D"]
    , Rule "A" [N "C", N "F"]
    , Rule "B" [T "c"]
    , Rule "B" [N "E", N "B"]
    , Rule "C" [T "a"]
    , Rule "D" [T "b"]
    , Rule "E" [T "c"]
    , Rule "F" [N "A", N "D"]
    ]

cnf_eps =
    [ Rule "S" [T "a"]
    , Rule "S" [N "B", N "B"]
    , Rule "B" [T "b"]
    , Rule "S" []
    ]

cnf14 = {- wrong -}
    [ Rule "A"  [N "a",N "c"]
    , Rule "A"  [T "b"]
    , Rule "A"  [T "c"]
    , Rule "A"  [N "L1",N "b"]
    , Rule "B"  [N "L1",N "b"]
    , Rule "B"  [T "b"]
    , Rule "C"  [N "B",N "b"]
    , Rule "L1" [N "C",N "L1"]
    , Rule "L1" [N "B",N "b"]
    , Rule "a"  [T "a"]
    , Rule "b"  [T "b"]
    , Rule "c"  [T "c"]
    , Rule "X"  [T "d"]
    ]


isInCNF (Rule _ [N _, N _]) = True
isInCNF (Rule _ [T _])      = True
isInCNF _                   = False
