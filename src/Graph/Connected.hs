module Graph.Connected where

import qualified Graph.MST.DSF as DSF
import Autolib.Graph.Graph
import qualified Data.Set as S
import Control.Monad ( forM )

-- | liefert Just (x,y), wenn x und y in versch. Komponenten
-- oder Nothing, wenn Graph zusammenhängend ist
-- (fast) Linearzeit (benutzt DSF-Implementierung)
connected :: GraphC v 
          => Graph v -> Maybe ( v, v )
connected g = DSF.run $ do 
    forM ( S.toList $ kanten g ) $ \ k -> DSF.join ( von k ) ( nach k )
    let handle (x : y : zs) = do
            was_different <- DSF.join x y
            if was_different
               then return $ Just (x,y)
               else handle (y : zs)
        handle _ = return Nothing
    handle $ S.toList $ knoten g


-- | wird  g  (2. Arg.) durch  t (1. Arg) aufgespannt?
-- liefert Just (x,y), wenn x und y in g verbunden,
-- aber in t getrennt sind, oder Nothing
-- (fast) Linearzeit (benutzt DSF-Implementierung)
spanning :: GraphC v 
          => Graph v 
          -> Graph v
          -> Maybe ( v, v )
spanning t g = DSF.run $ do 
    -- compute Komponenten von t  (Gerüst)
    forM ( S.toList $ kanten t ) $ \ k -> DSF.join ( von k ) ( nach k )
    -- check Kanten von  g  (Original)
    let handle (x : y : zs) = do
            was_different <- DSF.join x y
            if was_different
               then return $ Just (x,y)
               else handle (y : zs)
        handle _ = return Nothing
    handle $ S.toList $ knoten g
