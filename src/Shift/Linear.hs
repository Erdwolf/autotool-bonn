module Shift.Linear where

-- $Id$

import Shift.Iterate

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
	    | Wild
     deriving ( Eq )

instance ToDoc a => ToDoc ( Linear a ) where
    toDoc ( Item i ) = toDoc i
    toDoc ( r @ Repeat {} ) = vcat
          [ hcat [ toDoc ( count r ) 
		 , if all nul (diff r)  then empty else 
		   text "@" <> braces (toDoc ( diff r ) )
		 , text "*" 
		 ]
	  , nest 4 $ toDoc ( start r ) 
	  ]
    toDoc ( Wild ) = text "?"


instance ToDoc a => Show ( Linear a ) where show = render . toDoc



data Diff = DZero -- no difference on items
	  | DRepeat { dcount :: Int
		    , dstart :: [ Diff ]
		    }
	  | DWild
    deriving ( Eq, Ord )

instance ToDoc Diff where
    toDoc DZero = text "0"
    toDoc d | nul d  = text "o"
    toDoc ( d @ DRepeat {} ) = braces $
	( if all nul $ dstart d then empty else 
	  toDoc (dstart d)
	) <+>  toDoc (dcount d) 
    toDoc DWild = text "?"

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
repeater x =  iterate_strict (flip plusses (diff x)) (start x)

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
plus ( Wild ) ( DWild ) = Wild
plus _ _ = error "Shift.Linear.plus: args do not match"

conform :: Linear a -> Diff -> Bool
conform ( Item x ) ( DZero ) = True
conform ( x @ Repeat {} ) ( y @ DRepeat {} ) = 
    conforms ( start x ) ( dstart y )
conform ( Wild ) ( DWild ) = True
conform _ _ = False

conforms :: [ Linear a ] -> [ Diff ] -> Bool
conforms xs ds =
       length xs == length ds
    && all ( uncurry conform ) ( zip xs ds )

------------------------------------------------------------------

small :: Diff -> Bool
small ( DZero ) = True
small ( DWild ) = True
small ( r @ DRepeat {} ) = 
    abs (dcount r) <= 1 &&  all small ( dstart r )

nul :: Diff -> Bool
nul ( DZero ) = True
nul ( DWild ) = True
nul ( r @ DRepeat {} ) = 
    abs (dcount r) == 0 &&  all nul   ( dstart r )

depth :: Linear a -> Int
depth ( Item _ ) = 0
depth ( Wild ) = 0
depth ( r @ Repeat {} ) = 
    succ $ maximum $ map depth $ start r

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
minus Wild Wild = return DWild
minus _ _ = mzero

minusses :: Eq a => [ Linear a ] -> [ Linear a ] -> Maybe [ Diff ]
minusses xs ys = do
    guard $ length xs == length ys
    sequence $ zipWith minus xs ys

--------------------------------------------------------------

diffs :: Eq a => Int -> Int -> [Linear a] -> [ [Diff] ]
diffs k0 w xs = do
    ys <- tails xs
    k <- [ k0 .. w ]
    ( a : b : rest ) <- return $ cuts k ys
    guard $ not $ null a
    ba <- maybeToList $ minusses b a
    guard  $ all small ba
    -- cb <- maybeToList $ minusses c b
    -- guard $ ba == cb
    return $ ba

topdiffs :: Eq a => Int -> Int -> [ Linear a ] -> [[ Diff ]]
topdiffs k0 w xs = uniq $ do
    let fm = addListToFM_C (+) emptyFM $ do 
	     ds <- diffs k0 w xs
	     return ( ds, 1 )
    (c, ds) <- sortBy (negate . fst ) $ do 
	     ( ds, c ) <- fmToList fm
	     return ( c, ds )
    return ds

apply :: Eq a => Bool -> [ Diff ] -> [ Linear a ] -> [ Linear a ]
-- if compact == True, then insert Wild elements for non-matching items
apply compact ds [] = []
apply compact ds xs = 
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
	then r : apply compact ds ( drop ( c * k ) xs )
	else ( if compact then wild
	       else ( head xs : )
	     ) $ apply compact ds ( drop         1 xs )

wild xs = Wild : dropWhile (== Wild) xs

cuts :: Int -> [a] -> [[a]]
-- schneidet in gleichlange st�cke
cuts w [] = []
cuts w xs =
    let ( pre, post ) = splitAt w xs
    in pre : cuts w post

---------------------------------------------------------------------------

triv ( Repeat { start = [ Item foo ] } ) = True
triv _ = False

work :: Eq a => Bool -> Int -> Int -> [ Linear a ] -> [ Linear a ] 
-- apply one complete pass of analysis
-- try several of the topdiffs
work compact k0 w xs = 
    let dss = take 1 $ topdiffs k0 w xs 
    in	if null dss then []
	else foldr (apply compact) xs dss

worker :: Eq a => Bool -> Int -> [ Linear a ] -> [[ Linear a ]]
worker compact w xs = helper compact False w xs

helper compact f w xs = xs :
    case work compact (if f then 1 else 1 ) w xs of
	 [] -> []
	 ys -> helper compact True w ys

