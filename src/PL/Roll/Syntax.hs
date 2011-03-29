module PL.Roll.Syntax where

import PL.Signatur
import PL.Data

import qualified Data.Map as M
import qualified Data.Set as S
import Autolib.Util.Zufall
import Control.Monad ( guard )

-- | lazy list of all terms, by size
terms :: Signatur -> [[ Term ]]
terms sig = ts 
  where ts = do
        s <- [ 0 .. ] 
        return $ do
            ( f, a ) <- M.toList $ funktionen sig
            ss <- distributions (s - 1) a
            ts <- sequence ( map ( ts !! ) ss )
            return $ Apply f ts
          ++ do
            guard $ 1 == s
            v <- S.toList $ freie_variablen sig
            return $ Variable v

distributions :: Int -> Int -> [[Int]]
distributions 0 0 = return []
distributions total width | width > 0 = do
    x <- [ 1 .. total - width + 1 ] 
    xs <- distributions ( total - x ) ( width - 1 )
    return $ x : xs
distributions _ _ = [] -- should not happen    