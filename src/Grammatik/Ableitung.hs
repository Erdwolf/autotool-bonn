module Grammatik.Ableitung where

import Monad (guard)
import Set
import Schichten
import ToDoc

import Util.Wort
import Grammatik.Type

-------------------------------------------------------------------


data Ableitung = Ableitung [ String ]
     -- trick: ableitung steht hierin falschrum

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

cons :: String -> Ableitung -> Ableitung
cons x (Ableitung ys) = Ableitung (x : ys)

nil :: Ableitung
nil = Ableitung []

-------------------------------------------------------------------

schritt :: Maybe Int -- wörter höchstens so lang
	-> Grammatik -> Ableitung -> Set Ableitung
schritt schranke g a = mkSet $ do
    let w = car a
    (vorn, hinten) <- zerlegungen w
    (links, rechts) <- rules g
    let (mitte, rest) = splitAt (length links) hinten
    guard $ mitte == links    
    let w' = vorn ++ rechts ++ rest
    guard $ case schranke of Nothing -> True; Just n -> length w' <= n
    return $ cons w' a

ableitungen :: Maybe Int -> Grammatik -> [ Set Ableitung ]
-- erzeugt unendliche Liste (schichten)
ableitungen schranke g = do
    let w0 = [ startsymbol g ]
    schichten (schritt schranke g) $ cons w0 nil



