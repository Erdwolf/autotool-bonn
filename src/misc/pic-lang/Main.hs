import Autolib.Exp
import Autolib.Exp.Inter
import Autolib.NFA
import Autolib.NFA.Shortest (is_accepted)
import Autolib.ToDoc

import System.Environment
import Control.Monad ( guard, when )
import Data.Array


main :: IO ()
main = do
    args <- getArgs
    when ( 2 /= length args ) $ error
         $ "example usage : ./Main \"(lu+rd)^*\" 4"     

    let [ exp0, dep0 ] = args

    let -- left, right, up, down
	env = std_sigma "lrud" 
    print $ vcat
        [ text "standard environment"
	, nest 4 $ toDoc env
	]

    let exp :: RX Char 
        exp = read exp0
    print $ vcat
	[ text "input expression"
	, nest 4 $ toDoc exp
	]

    let dep :: Int
	dep = read dep0
    print $ vcat 
	[ text "paint picture of depth"
	, nest 4 $ toDoc dep 
	]

    let aut :: NFA Char Int
	aut = inter env exp
    print $ vcat
        [ text "automaton"
	, nest 4 $ toDoc aut
	]

    let black :: [ (Path, Point)  ]
	black = do
            p <- paths dep
            guard $ is_accepted aut p
	    return ( p, position p )
    print $ vcat
        [ text "black pixels"
	, nest 4 $ toDoc black
	]

    let pic :: Array (Int,Int) Char
	pic = picture aut dep
    print $ vcat
	[ text "picture"
	, nest 4 $ format pic
	]

type Path = [ Char ]

paths :: Int -> [ Path ]
paths 0 = return []
paths d | d > 0 = do
    h <- "lr"
    v <- "ud"
    rest <- paths ( d - 1 )
    return $ h : v : rest

type Point = ( Int, Int )

position :: Path -> Point
position p = 
    let f (x,y) [] = (x, y)
	f (x,y) (h : v : rest) = 
	    let hh = case h of 'l' -> 0 ; 'r' -> 1 
		vv = case v of 'u' -> 0 ; 'd' -> 1
	    in  f ( 2 * x + hh, 2 * y + vv ) rest
    in  f (0,0) p

picture aut dep = 
    let top = 2^dep - 1
	bnd = ((0,0),(top,top))
    in  array bnd $ do
	    p <- paths dep
	    let c = if is_accepted aut p then '*' else '.'
	    return ( position p, c )

format :: Array (Int,Int) Char -> Doc
format a = vcat $ do
    let ((u,l),(d,r)) = bounds a
    row <- [ u .. d ]
    return $ hcat $ do
        col <- [ l .. r ]
	return $ text [ a ! (row,col), ' ' ]
