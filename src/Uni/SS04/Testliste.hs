module Uni.SS04.Testliste where

--   $Id$ 

import qualified Random


data Testliste = Make 
    { arity       :: Int                -- ^ Stelligkeit z.b. 2
    , len         :: Int                -- ^ Gesamtlaenge 
    , fixs        :: [ [Integer] ]      -- ^ Fixe Liste z.B. [[1,2]]
    , randVonBis  :: (Integer,Integer)  -- ^ Random z.B. (3,5)
    }

genTestliste :: Testliste
             -> IO [ [ Integer ] ] -- z.B. [ [1,2],[3,5], ... ]
genTestliste Make { arity = arity 
                  , len = len 
                  , fixs = fixs
                  , randVonBis = randVonBis 
                  }
    = 
 do 
 let lenRand = len - length fixs
     randTupel = do t <- sequence $ replicate arity $ Random.randomRIO randVonBis
                    return t
 rands <- sequence $ replicate lenRand $  randTupel
 return $ fixs ++ rands


