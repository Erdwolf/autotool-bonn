module Shift.Linear where

-- $Id$

import ToDoc
import Monad ( guard, mzero )
import Maybe
import List ( inits, tails )
import Util.Sort
import Util.Uniq
import Set
import FiniteMap

--------------------------------------------------------------

-- the data type,

data Linear a = Item { unItem :: a }
	    | Repeat   { start :: [ Linear a ]
		       , count  :: Int 
		       , diff  :: [ Diff ]
		       }
     deriving ( Eq )

instance ToDoc a => ToDoc ( Linear a ) where
    toDoc ( Item i ) = toDoc i
    toDoc ( r @ Repeat {} ) = 
	toDoc ( start r ) <+>
           if all nul (diff r)
	   then text "*" <+> toDoc ( count r )
	   else nest 4 ( text ("+ {" ++ show 0 ++ ".." ++ show (count r - 1) ++ "} *" )
		    <> toDoc ( diff r )
		    )

instance ToDoc a => Show ( Linear a ) where show = render . toDoc



data Diff = DZero -- no difference on items
	  | DRepeat { dcount :: Int
		    , dstart :: [ Diff ]
		    }
    deriving ( Eq, Ord )

instance ToDoc Diff where
    toDoc ( DZero ) = text "0"
    toDoc ( d @ DRepeat {} ) = braces $
	toDoc (dstart d) <+>  toDoc (dcount d) 

instance Show Diff where show = render . toDoc

-- and its intended interpretation

unfold :: Linear a -> [ a ]
unfold ( Item i ) = [ i ]
unfold ( x @ Repeat { } ) = do
    ls <- take ( count x ) $ repeater x
    unfolder ls

unfolder :: [ Linear a ] -> [ a ]
unfolder ls = do l <- ls ; unfold l

repeater :: Linear a -> [[ Linear a ]]
-- produce infinite list, not recursively unfolded
repeater x =  iterate (flip plusses (diff x)) (start x)

plusses :: [Linear a] -> [Diff] -> [Linear a]
plusses xs ds = 
    if length xs == length ds 
       then zipWith plus xs ds
       else error "Shift.Linear.plusses: different lengths"

plus :: Linear a -> Diff -> Linear a
plus ( Item x ) ( DZero ) = Item x
plus ( x @ Repeat {} ) ( y @ DRepeat {} ) =
    Repeat  { start = plusses (start x) (dstart y)
		, count = (+)  (count x) (dcount y)
		, diff  = diff x
		}
plus _ _ = error "Shift.Linear.plus: args do not match"

conform :: Linear a -> Diff -> Bool
conform ( Item x ) ( DZero ) = True
conform ( x @ Repeat {} ) ( y @ DRepeat {} ) = 
    conforms ( start x ) ( dstart y )
conform _ _ = False

conforms :: [ Linear a ] -> [ Diff ] -> Bool
conforms xs ds =
       length xs == length ds
    && all ( uncurry conform ) ( zip xs ds )

------------------------------------------------------------------

small :: Diff -> Bool
small ( DZero ) = True
small ( r @ DRepeat {} ) = 
    abs (dcount r) <= 1 &&  all small ( dstart r )

nul :: Diff -> Bool
nul ( DZero ) = True
nul ( r @ DRepeat {} ) = 
    abs (dcount r) == 0 &&  all nul   ( dstart r )

------------------------------------------------------------------

minus :: Eq a => Linear a -> Linear a -> Maybe Diff
-- must have same shape
-- and Repeats must have identical diffs
minus (Item x) (Item y) = do
    guard $ x == y
    return $ DZero
minus (x @ Repeat {}) (y @ Repeat {}) = do
    let dx = diff x ; dy = diff y
    guard $ dx == dy -- strictly!
    s <- minusses ( start x ) ( start y )
    let c = (-)     ( count x ) ( count y )
    return $ DRepeat { dstart = s 
		    , dcount = c
                    }
minus _ _ = mzero

minusses :: Eq a => [ Linear a ] -> [ Linear a ] -> Maybe [ Diff ]
minusses xs ys = do
    guard $ length xs == length ys
    sequence $ zipWith minus xs ys

--------------------------------------------------------------

diffs :: Eq a => Int -> [Linear a] -> [ [Diff] ]
diffs w xs = do
    ys <- tails xs
    k <- [ 1 .. w ]
    ( a : b : c : rest ) <- return $ cuts k ys
    guard $ not $ null a
    ba <- maybeToList $ minusses b a
    guard  $ all small ba
    cb <- maybeToList $ minusses c b
    guard $ ba == cb
    return $ ba

topdiffs :: Eq a => Int -> [ Linear a ] -> [[ Diff ]]
topdiffs w xs = uniq $ do
    let fm = addListToFM_C (+) emptyFM $ do 
	     ds <- diffs w xs
	     return ( ds, 1 )
    (c, ds) <- sortBy (negate . fst ) $ do 
	     ( ds, c ) <- fmToList fm
	     return ( c, ds )
    return ds

apply :: Eq a => [ Diff ] -> [ Linear a ] -> [ Linear a ]
apply ds [] = []
apply ds xs = 
    let k = length ds
        ys = cuts k xs
        y = head ys
	r = Repeat { start = y
		   , diff  = ds
		   , count = c -- recursively, aber egal
		   }
	zs = repeater r
	c  = length $ takeWhile ( uncurry (==) ) $ zip ys zs
    in  if   conforms y ds && c > 1
	then r       : apply ds ( drop ( c * k ) xs )
	else head xs : apply ds ( drop         1 xs )

cuts :: Int -> [a] -> [[a]]
-- schneidet in gleichlange stücke
cuts w [] = []
cuts w xs =
    let ( pre, post ) = splitAt w xs
    in pre : cuts w post

---------------------------------------------------------------------------

work :: Eq a => Int -> [ Linear a ] -> Maybe [ Linear a ]
-- apply one complete pass of analysis
work w xs = do
    let dss = take 5 $ topdiffs w xs 
    guard $ not $ null dss
    return $ foldr apply xs dss

worker :: Eq a => Int -> [ Linear a ] -> [[Linear a]]
worker w xs = 
    case work w xs of
        Just ys -> ys : worker w ys
	Nothing -> []

worker1 w xs = do
    ys <- maybeToList $ work 1 xs
    worker w ys
