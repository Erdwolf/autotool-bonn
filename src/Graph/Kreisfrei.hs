module Graph.Kreisfrei where

import qualified Graph.MST.DSF as DSF
import Autolib.Graph.Graph
import qualified Data.Set as S

-- | liefert Just Kante, die auf einem Kreis liegt,
-- oder Nothing, wenn Graph kreisfrei ist.
-- (fast) Linearzeit (benutzt DSF-Implementierung)
kreisfrei :: GraphC v 
          => Graph v -> Maybe ( Kante v )
kreisfrei g = DSF.run $ do
    let handle [] = return Nothing
        handle (e : es) = do
            was_different <- DSF.join ( von e ) ( nach e )
            if not was_different
               then return $ Just e
               else handle es
    handle $ S.toList $ kanten g
