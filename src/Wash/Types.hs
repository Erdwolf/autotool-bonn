module Wash.Types where

import Maybe

data TySpec = 
     TS TyRep [TyDecl]					    -- declarations sorted!
  deriving (Read, Show, Eq)

data TyRep =
     TRBase  String
   | TRVar   String
   | TRTuple [TyRep]
   | TRData  String [TyRep]
  deriving (Read, Show, Eq)

data TyDecl =
     TD String [String] [ConRep]
  deriving (Read, Show, Eq)

data ConRep =
     CR String						    -- ctor name
       	(Maybe [String])				    -- field names
	[TyRep]						    -- field types
  deriving (Read, Show, Eq)

class Types a where
  ty :: a -> TySpec

instance Types () where
  ty i = TS (TRBase "()") []

instance Types Bool where
  ty i = TS (TRBase "Bool") []
  
instance Types Int where
  ty i = TS (TRBase "Int") []
  
instance Types Integer where
  ty i = TS (TRBase "Integer") []
  
--instance Types Rational where
--  ty i = TS (TRBase "Rational") []

instance Types Double where
  ty d = TS (TRBase "Double") []

instance Types Float where
  ty d = TS (TRBase "Float") []

instance Types Char where
  ty c = TS (TRBase "Char") []

instance (Types a, Types b) => Types (a,b) where
  ty xy = TS (TRTuple [tra, trb]) (merge defsa defsb)
    where TS tra defsa = ty (fst xy)
	  TS trb defsb = ty (snd xy)

instance (Types a, Types b, Types c) => Types (a,b,c) where
  ty ~(xa,xb,xc) = TS (TRTuple [tra, trb, trc]) (merge (merge defsa defsb) defsc)
    where TS tra defsa = ty xa
	  TS trb defsb = ty xb
	  TS trc defsc = ty xc

instance (Types a) => Types [a] where
  ty xs = TS (TRData (tdName listDef) [tr]) (merge defs [listDef])
    where TS tr defs = ty (head xs)

listDef = TD "[]" ["a"]	[CR "[]" Nothing []
			,CR ":"  Nothing [TRVar "a"
					 ,TRData (tdName listDef) [TRVar "a"]]]

instance (Types a) => Types (Maybe a) where
  ty mx = TS (TRData (tdName maybeDef) [tr]) (merge defs [maybeDef])
    where TS tr defs = ty (fromJust mx)

-- fromJust (Just x) = x -- in Module Maybe

maybeDef = 
	TD "Maybe" ["a"][CR "Nothing" Nothing []
			,CR "Just"    Nothing [TRVar "a"]]

instance (Types a, Types b) => Types (Either a b) where
  ty xy = TS (TRData (tdName eitherDef) [tra, trb]) 
  	     (merge (merge defsa defsb) [eitherDef])
    where TS tra defsa = ty (fromLeft xy)
	  TS trb defsb = ty (fromRight xy)

fromLeft  (Left x)  = x
fromRight (Right y) = y

eitherDef =
	TD 	"Either"
		["a", "b"]
		[CR "Left"  Nothing [TRVar "a"]
		,CR "Right" Nothing [TRVar "b"]]

tdName (TD name _ _) = name

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
  | x == y               = x : merge xs ys
  | tdName x == tdName y = error ("Different types with identical name: " ++ tdName x)
  | tdName x <= tdName y = x : merge xs (y:ys)
  | otherwise            = y : merge (x:xs) ys
