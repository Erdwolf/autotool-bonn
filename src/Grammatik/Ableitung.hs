{-# language DeriveDataTypeable #-}

module Grammatik.Ableitung where

import Grammatik.Type
import Grammatik.Ableitung.Config

import Machine.History

import Control.Monad (guard)
import Autolib.Set
import Autolib.Schichten
import Autolib.ToDoc
import Autolib.Size

import Autolib.Util.Wort

import Data.Typeable
import Data.List ( tails )

-------------------------------------------------------------------

-- | trick: ableitung steht hierin falschrum
data Ableitung = Ableitung [ String ]
    deriving Typeable

instance Size Ableitung where 
    size (Ableitung ws) = length ws

instance Eq Ableitung where 
    a == b = car a == car b

instance Ord Ableitung where 
    a `compare` b = car a `compare` car b

instance ToDoc Ableitung where
    toDoc (Ableitung xs) = 
        toDoc $ reverse xs

instance History Ableitung where
    history (Ableitung a) = do
        ws <- drop 1 $ tails a
        guard $ not $ null ws
        return $ Ableitung ws

car  :: Ableitung -> String
car (Ableitung xs) = head xs

cdr  :: Ableitung -> Ableitung
cdr (Ableitung xs) = Ableitung $ tail xs

cadr :: Ableitung -> String
cadr = car . cdr

mcadr :: Ableitung -> Maybe String
mcadr x = do d <- mcdr x ; mcar d

mcar :: Ableitung -> Maybe String
mcar (Ableitung xs) = do
    guard $ not $ null xs
    return $ head xs

mcdr :: Ableitung -> Maybe Ableitung
mcdr (Ableitung xs) = do
    guard $ not $ null xs
    return $ Ableitung $ tail xs
  
cons :: String -> Ableitung -> Ableitung
cons x (Ableitung ys) = Ableitung (x : ys)

nil :: Ableitung
nil = Ableitung []

-------------------------------------------------------------------


-- | nur die wörter, die nicht zu lang sind
schritt :: Config
	-> Grammatik -> Ableitung -> Set Ableitung
schritt conf g a = mkSet $ do
    let w = car a
    (vorn, hinten) <- zerlegungen w
    (links, rechts) <- rules g
    let (mitte, rest) = splitAt (length links) hinten
    guard $ mitte == links    
    let w' = vorn ++ rechts ++ rest
    guard $ length w' <= max_length conf
    return $ cons w' a

-- | beachte config
ableitungen :: Config -> Grammatik -> [ Set Ableitung ]
ableitungen conf g 
    = take ( max_depth conf )
    $ takeWhile ( \ s -> cardinality s < max_width conf )
    $ schichten ( schritt conf g ) 
    $ cons [ startsymbol g ] nil



