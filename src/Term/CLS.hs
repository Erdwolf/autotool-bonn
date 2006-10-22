module Term.CLS where

--   $Id$

-- untersuche, ob CL(S) rückwärts change-bounded ist
-- falls ja, dann wäre das ein argument für REG-erhaltung

import Term.Type
import Term.Dot
import Term.Match
import Term.Change

import ToDoc
import Data.FiniteMap
import Control.Monad ( when )

type STerm = Term Char

instance ToDoc (Int, Char) where
    toDoc (i, c) = text (show i ++ [c])
instance Show (Int, Char) where
    show = render . toDoc


build ::  [ STerm ] -> STerm
build ( x : xs ) = foldl bin x xs

spine :: STerm -> [ STerm ]
-- invers zu build
spine t = sp t []
    where sp t xs = case children t of
	     [l, r] -> sp l (r : xs)
	     []     -> t : xs

instance ToDoc STerm where
    toDoc t =
        let x : xs = spine t
	    par = if null xs then id else parens
	in  par $ text [ symbol x ] <+> fsep (map toDoc xs)

instance Show STerm where show = render . toDoc


buildV :: [ VTerm Char ] -> VTerm Char
buildV ( x : xs ) = foldl binV x xs

var :: String -> VTerm Char
var x = Term { symbol = Left x , children = [] }

con :: a -> VTerm a
con x = Term { symbol = Right x , children = [] }

binV :: VTerm Char -> VTerm Char -> VTerm Char
binV l r = Term { symbol = Right '@'
	       , children = [ l, r ]
	       }

cls :: TRS Char
cls = [ ( buildV [ con 'S', var "x", var "y", var "z" ]
	, buildV [ var "x", var "z" , buildV [ var "y", var "z" ] ]
	)
      ]

clsg :: TRS Char
clsg = do 
    let sub = listToFM [ ("z", s ) ]
    (l,r) <- cls
    return (applyV sub l, applyV sub r)

rev :: TRS a -> TRS a
rev trs = do (l,r) <- trs ; return (r,l)

icls, iclsg :: TRS Char
icls = rev cls
iclsg = rev clsg


bin :: STerm -> STerm -> STerm
bin l r = Term { symbol = '@'
	       , children = [ l, r ]
	       }

s :: STerm
s = Term { symbol = 'S' , children = [] }

t = build [s,s]
a = build [s,s,s]

stt = build [s, t, t]
om = bin stt stt


sterms :: Int -> [ STerm ]
-- zähle anzahl der S
sterms n | n == 1 = return s
sterms n = do
    a <- [ 1 .. n-1 ]
    let b = n - a
    l <- sterms a
    r <- sterms b
    return $ bin l r

check trs = mapM_ handle $ do 
    n <- [1 .. ]
    t <- sterms n
    s <- successors trs $ young t
    let m =  maximum $ map (age . symbol) $ subterms s 
    return ( m, t, s )

handle (m,t,s) = 
    if m < 3 then putStr (show m) 
    else do print (m,t,s)
	    when ( m > 3 ) $ display s






