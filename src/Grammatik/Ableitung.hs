module Grammatik.Ableitung where

import Monad (guard)
import Set
import Schichten
import ToDoc

import Util.Wort
import Grammatik.Type

-------------------------------------------------------------------


data Ableitung = Ableitung [ String ]
    deriving (Eq, Ord)

-- instance Show Ableitung where
--    showsPrec p (Ableitung xs) =
--        showString $ concat $ intersperse " -> " $ reverse xs

instance ToDoc Ableitung where
    toDoc (Ableitung xs) = 
        fsep $ punctuate (text "->") $ map text xs

instance Show Ableitung where show = render . toDoc


car  :: Ableitung -> String
car (Ableitung xs) = head xs

cdr  :: Ableitung -> Ableitung
cdr (Ableitung xs) = Ableitung $ tail xs

cons :: String -> Ableitung -> Ableitung
cons x (Ableitung ys) = Ableitung (x : ys)

nil :: Ableitung
nil = Ableitung []

-------------------------------------------------------------------

schritt :: Maybe Int
	-> Grammatik -> String -> Set String
schritt schranke g w = mkSet $ do
    (vorn, hinten) <- zerlegungen w
    (links, rechts) <- rules g
    let (mitte, rest) = splitAt (length links) hinten
    guard $ mitte == links    
    let w' = vorn ++ rechts ++ rest
    guard $ case schranke of Nothing -> True; Just n -> length w' <= n
    return $ w'



erzeugte_sprache :: Maybe Int -> Grammatik -> [ String ]
-- erzeugt unendliche Liste
erzeugte_sprache schranke g = do
    ws <- schichten (schritt schranke g) [ startsymbol g ]
    w <- setToList ws
    guard $ and [ x `elementOf` terminale g | x <- w ]
    return w

