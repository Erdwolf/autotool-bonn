module Fun.Table where

--   $Id$

-- berechnen von vielen funktionswerten
-- optimiert: mit durchgehender cache-benutzung

-- ergebnisbehandlung:
-- tabulate erzeugt ein Array mit werten (das ist Show/Read-fähig)
-- frame erzeugt daraus ein Doc

-- d. h. tabulate benutzen, um in cache zu schreiben
-- und frame zum anzeigen

import Fun.Type
import Fun.Machine
import Machine.Class
import Fun.Step
import Fun.State

import Fun.Examples -- for testing

import Reporter
import ToDoc
import Data.Array


anzeig :: Doc -> Reporter ()
anzeig doc = do
    inform $ text "Der Werteverlauf Ihrer Funktion ist:"
    newline
    inform $ doc	
    newline

tabelle2 :: Fun -- ^ funktions-Ausdruck
	 -> ( Integer, Integer ) -- ^ Dimension der Tabelle
	 -> Doc
tabelle2 fun dim = rollout $ frame2 $ tabulate2 fun dim

tabelle1 :: Fun -- ^ funktions-Ausdruck
	 -> Integer -- ^ Dimension der Tabelle
	 -> Doc
tabelle1 fun dim = rollout $ frame1 $ tabulate1 fun dim

-----------------------------------------------------------------------------

item_width :: Int
item_width = 5

table :: Fun -> [[Integer]] -> [([Integer],Integer)]
table f args = 
    let exps = do arg <- args ; return $ App f $ map Zahl arg
        t = final $ ( input f undefined ) { todo = exps }
    in  zip args $ reverse $ stack t

final :: State -> State
final s = case step s of
    [ t ] -> final t
    [   ] -> s

-------------------------------------------------------------------

type Tafel = Array (Integer, Integer) Integer 

tabulate2 :: Fun 
	 -> (Integer, Integer) 
	 -> Tafel
tabulate2 f (h, w) = 
    let bnd = ((0,0), (h,w))
	tuple ([x,y], v) = ((x,y), v) ; untuple (x,y) = [x,y]
    in  array bnd $ map tuple $ table f $ map untuple $ range bnd

tabulate1 :: Fun
	  -> Integer
	  -> Array Integer Integer
tabulate1 f w =
    let bnd = ( 0, w )
	tuple ([x], v) = ((x), v) ; untuple (x) = [x]
    in  array bnd $ map tuple $ table f $ map untuple $ range bnd
	
---------------------------------------------------------------------

frame1 :: Show a 
      => Array Integer a 
      -> Array ( Integer, Integer ) String
frame1 a = 
    let (0, w) = bounds a
    in accumArray ( \ alt neu -> neu ) "" ((-2, -2), (0, w)) 
	    $ topline w ++ topnums w ++ contents1 a

frame2 :: Show a 
      => Array (Integer, Integer) a 
      -> Array (Integer, Integer) String
frame2 a = 
    let ((0,0), (h,w)) = bounds a
    in accumArray ( \ alt neu -> neu ) "" ((-2, -2), (h, w)) 
	    $ sideline w ++  topline w ++ topnums w ++ sidenums h 
	       ++ contents2 a

contents2 a = do (k,v) <- assocs a ; return (k, show v)
contents1 a = do (k,v) <- assocs a ; return ((0,k), show v)

topnums w = do y <- [ 0 .. w ] ; return ((-2,y), show y)
topline w = do y <- [-2 .. w ] ; return ((-1,y), replicate item_width '-' )
sidenums h = do x <- [ 0 .. h ] ; return ((x,-2), show x)
sideline w = do x <- [-2 .. w ] ; return ((x,-1), "|" )

rollout :: ( Ix a, Ix b, Enum a, Enum b ) 
	=> Array ( a, b ) String
        -> Doc
rollout b = vcat $ do
    let ((o,l),(u,r)) = bounds b
    x <- [ o .. u ]
    return $ hcat $ do
        y <- [ l .. r ]
	return $ text $ trim item_width $ b ! (x,y)

-- fülle mit leerzeichen auf gesamtbreite w
trim w cs = 
    let l = length cs
	filler = replicate (w - l) ' '
    in  filler ++ cs


