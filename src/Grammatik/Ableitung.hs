module Grammatik.Ableitung where

import Control.Monad (guard)
import Autolib.Set
import Autolib.Schichten
import Autolib.ToDoc

import Autolib.Util.Wort
import Grammatik.Type

-------------------------------------------------------------------

data Config = Config
	   { max_length :: Int -- wortlänge
	   , max_depth :: Int -- anzahl schichten
	   , max_width :: Int -- breite der schicht
	   }
    deriving ( Read, Show )

config :: Config -- als default benutzbar
config = Config
       { max_length = 10
       , max_depth = 10
       , max_width = 1000
       }

-- | trick: ableitung steht hierin falschrum
data Ableitung = Ableitung [ String ]


instance Eq Ableitung where 
    a == b = car a == car b

instance Ord Ableitung where 
    a `compare` b = car a `compare` car b

instance ToDoc Ableitung where
    toDoc (Ableitung xs) = toDoc $ reverse xs


instance Show Ableitung where 
    show = render . toDoc

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



schritt :: Config
	-> Grammatik -> Ableitung -> Set Ableitung
-- nur die wörter, die nicht zu lang sind
schritt conf g a = mkSet $ do
    let w = car a
    (vorn, hinten) <- zerlegungen w
    (links, rechts) <- rules g
    let (mitte, rest) = splitAt (length links) hinten
    guard $ mitte == links    
    let w' = vorn ++ rechts ++ rest
    guard $ length w' <= max_length conf
    return $ cons w' a

ableitungen :: Config -> Grammatik -> [ Set Ableitung ]
-- beachte config
ableitungen conf g 
    = take ( max_depth conf )
    $ takeWhile ( \ s -> cardinality s < max_width conf )
    $ schichten ( schritt conf g ) 
    $ cons [ startsymbol g ] nil



