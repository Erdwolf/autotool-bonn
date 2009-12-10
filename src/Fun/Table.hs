-- | berechnen von vielen funktionswerten
-- optimiert: mit durchgehender cache-benutzung

-- ergebnisbehandlung:
-- tabulate erzeugt ein Array mit werten (das ist Show/Read-fähig)
-- frame erzeugt daraus ein Doc

-- d. h. tabulate benutzen, um in cache zu schreiben
-- und frame zum anzeigen

module Fun.Table where

import Fun.Matrix
import Fun.Type
import Fun.Check
import Fun.Machine
import Fun.Examples
import Machine.Class
import Fun.Step
import Fun.State
import Util.FailDoc

-- import Fun.Examples -- for testing

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader hiding ( newline )
import Data.Array
import Data.Typeable
import Data.List


anzeig :: Doc -> Reporter ()
anzeig doc = do
    inform $ text "Der Werteverlauf Ihrer Funktion ist:"
    newline
    inform $ doc	
    newline


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

data Tafel2 = Tafel2
           { unTafel2 :: Array (Integer, Integer) Integer }
    deriving ( Show, Read, Typeable )

data Tafel1 = Tafel1
           { unTafel1 :: Array Integer Integer }
    deriving ( Show, Read, Typeable )

instance ToDoc Tafel2 where
    toDocPrec p t = toDocPrec p (mkmatrix t)

instance Reader Tafel2 where
    atomic_readerPrec p = do
        t <- atomic_readerPrec p
        mktafel2 t

prettyTafel1 :: Tafel1 -> Doc
prettyTafel1 = rollout . frame1 . unTafel1

prettyTafel2 :: Tafel2 -> Doc
prettyTafel2 = rollout . frame2 . unTafel2

tabulate2 :: Fun 
	 -> (Integer, Integer) 
	 -> Tafel2
tabulate2 f (h, w) = 
    let bnd = ((0,0), (h,w))
	tuple ([x,y], v) = ((x,y), v) ; untuple (x,y) = [x,y]
    in  Tafel2 $ array bnd $ map tuple $ table f $ map untuple $ range bnd

tabulate1 :: Fun
	  -> Integer
	  -> Tafel1
tabulate1 f w =
    let bnd = ( 0, w )
	tuple ([x], v) = ((x), v) ; untuple (x) = [x]
    in  Tafel1 $ array bnd $ map tuple $ table f $ map untuple $ range bnd
	
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
	    $ sideline h ++  topline w ++ topnums w ++ sidenums h 
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

-- | fülle mit leerzeichen auf gesamtbreite w
trim w cs = 
    let l = length cs
	filler = replicate (w - l) ' '
    in  filler ++ cs


-- | convert Tafel2 to equivalent Matrix
mkmatrix :: Tafel2 -> Matrix Integer
mkmatrix t = 
    let ( (0,0), (h,w)) = bounds $ unTafel2 t
    in  Matrix { width = w + 1
	       , height = h + 1
	       , contents = [ [ unTafel2 t ! (x,y) | y <- [ 0 .. w ] ]
			    | x <- [ 0 .. h ]
			    ]
	       }

mktafel2 :: FailDoc m => Matrix Integer -> m Tafel2
mktafel2 m = do
    when ( genericLength ( contents m ) /= height m ) $ failDoc
	 $ text "Der Inhalt der Matrix hat nicht die Höhe"
         <+> toDoc ( height m )
    sequence $ do
        ( k, xs) <- zip [ 0 :: Int .. ] $ contents m
        return $ when ( genericLength xs /= width m ) $ failDoc
	     $ text "Die Zeile" <+> toDoc k
	     <+> text "hat nicht die Breite" <+> toDoc ( width m )
    return $ Tafel2 $ listArray ((0,0), (height m - 1, width m - 1 ))
		    $ concat $ contents m
