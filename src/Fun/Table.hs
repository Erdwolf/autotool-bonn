module Fun.Table where

-- $Id$

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

import ToDoc
import Array

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

tabulate :: Fun 
	 -> (Integer, Integer) 
	 -> Array (Integer, Integer) Integer
tabulate f (h, w) = 
    let bnd = ((0,0), (h,w))
	tuple ([x,y], v) = ((x,y), v) ; untuple (x,y) = [x,y]
    in  array bnd $ map tuple $ table f $ map untuple $ range bnd

---------------------------------------------------------------------

frame :: Show a 
      => Array (Integer, Integer) a 
      -> Doc
frame a = vcat $ do
    let ((0,0), (h,w)) = bounds a
	contents = do (k,v) <- assocs a ; return (k, show v)
	topnums = do y <- [ 0 .. w ] ; return ((-2,y), show y)
	topline = do y <- [-2 .. w ] ; return ((-1,y), "-" )
	sidenums = do x <- [ 0 .. h ] ; return ((x,-2), show x)
	sideline = do x <- [-2 .. w ] ; return ((x,-1), "|" )
        b = accumArray ( \ alt neu -> neu ) "" ((-2, -2), (h,w)) 
	    $ topline ++ sideline ++ topnums ++ sidenums ++ contents
    x <- [ -2 .. h ]
    return $ hcat $ do
        y <- [ -2 .. w ]
	return $ text $ trim 5 $ b ! (x,y)

-- fülle mit leerzeichen auf gesamtbreite w
trim w cs = 
    let l = length cs
	filler = replicate (w - l) ' '
    in  filler ++ cs


