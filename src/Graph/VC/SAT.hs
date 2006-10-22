-- | Konstruktion des Graphen aus dem Beweis der NP-Vollständigkeit von VC

module Graph.VC.SAT ( vc ) where

--  $Id$

import Autolib.Graph.Type
import Autolib.Set ( mkSet )
import Autolib.FiniteMap ( listToFM )
import Autolib.Boxing.Position ( Position )

import SAT.Types

import Graph.Cross ( Punkt , pinN )

-------------------------------------------------------------------------------

class Var a where vars :: a -> [Variable]

instance Var Literal where vars = return . unLiteral
instance Var Klausel where vars = setToList . mkSet . concatMap vars . literale
instance Var Formel where vars = setToList . mkSet . concatMap vars . klauseln

vc :: Position -> Formel -> ( Graph String , Int )
vc p f = 
    let ks = selektor_knoten f ++ klausel_kreis_knoten f
	g = mkGraph (mkSet $ map fst ks)
	            (mkSet $ foldl1 (++) [ selektor_kanten f
					 , klausel_kreis_kanten f
					 , verbindungen f
					 ]
		    )
    in ( pinN p g (listToFM ks) 
       , length ( vars f ) + 2 * ( length $ klauseln f )
       )

selektor_knoten :: Formel -> [(String,Punkt)]
selektor_knoten f = do (i,v) <- zip [0..] (vars f)
		       (j,x) <- zip [0..] sigs
		       return ( x:v , ( 1 + 2*i + j , 2 ) )

selektor_kanten :: Formel -> [Kante String]
selektor_kanten f = do v <- vars f ; return $ kante ( p:v ) ( n:v )

kk_name :: Integer -> String -> String
kk_name i v = foldl1 (++) [ "c_" , show i , "_" , v ]

kk_knoten :: Integer -> (Klausel,Integer) -> [(String,Punkt)]
kk_knoten nv (c,i) = do (j,v) <- zip [0..] (vars c)
			return ( kk_name i v , pos j )
    where pos :: Integer -> Punkt
	  (q,r) = divMod i nv
	  pos 0 = ( 3 * r     ,   - ( 2 * (q+1) ))
	  pos 1 = ( 3 * r + 2 ,   - ( 2 * (q+1) ))
	  pos _ = ( 3 * r + 1 , 1 - ( 2 * (q+1) ))

klausel_kreis_knoten ::  Formel -> [(String,Punkt)]
klausel_kreis_knoten f = concatMap (kk_knoten (numv f)) (kis f)

kk_kanten :: Integer -> (Klausel,Integer) -> [Kante String]
kk_kanten nv (c,i) = let ks = map fst $ kk_knoten nv (c,i)
		     in map (uncurry kante) $ zip ks (tail $ cycle ks)

klausel_kreis_kanten :: Formel -> [Kante String]
klausel_kreis_kanten f = concatMap (kk_kanten (numv f)) (kis f)

verbindungen :: Formel -> [Kante String]
verbindungen f = do (c,i) <- kis f
		    l <- literale c
		    return $ case l of Pos v -> kante (kk_name i v) ( p:v )
				       Neg v -> kante (kk_name i v) ( n:v )

kis :: Formel -> [(Klausel,Integer)]
kis f = zip (klauseln f) [0..]

numv :: Formel -> Integer
numv = fromIntegral . length . vars

sigs :: String ; p,n :: Char
sigs@[p,n] = "+-"