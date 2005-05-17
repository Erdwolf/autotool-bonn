-- | Konstruktion des Graphen aus dem Beweis der NP-Vollständigkeit von VC

module Graph.VC.SAT ( vc ) where

-- $Id$

import Autolib.Graph.Type
import Autolib.Set

import SAT.Types

-------------------------------------------------------------------------------

class Var a where vars :: a -> [Variable]

instance Var Literal where vars = return . unLiteral
instance Var Klausel where vars = setToList . mkSet . concatMap vars . literale
instance Var Formel where vars = setToList . mkSet . concatMap vars . klauseln

vc :: Formel -> ( Graph String , Int )
vc f = 
    let sigs@[p,n]      = "+-"
	selektor_knoten = do v <- vars f ; x <- sigs ; return (x:v)
        selektor_kanten = do v <- vars f ; return $ kante (p:v) (n:v)
        kk i v          = foldl1 (++) [ "c_" , show i , "_" , v ]
        kk_knoten (c,i) = map (kk i) (vars c)
        kk_kanten (c,i) = let ks = kk_knoten (c,i)
			  in map (uncurry kante) $ zip ks (tail $ cycle ks)
        kis             = zip (klauseln f) [(0::Int)..]
        klausel_kreis_knoten = concatMap kk_knoten kis
        klausel_kreis_kanten = concatMap kk_kanten kis
        verbindungen = do (c,i) <- kis
			  l <- literale c
			  return $ case l of
			            Pos v -> kante (kk i v) (p:v)
				    Neg v -> kante (kk i v) (n:v)
    in ( mkGraph 
	  (mkSet $ foldl1 (++) [ selektor_knoten , klausel_kreis_knoten ])
          (mkSet $ foldl1 (++) [ selektor_kanten , klausel_kreis_kanten 
			       , verbindungen 
			       ]
	  )
       , length (vars f) + 2 * (length $ klauseln f)
       )
