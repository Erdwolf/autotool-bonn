module Expression.Type where

import ToDoc

data Expression
     = Id  String | Strg String | Num Integer
     | Positional String [ Expression ]
     | Named      String [ ( String, Expression ) ]
     | List	  [ Expression ]
     | Tuple	  [ Expression ]
     deriving (Eq, Ord)



instance ToDoc Expression where toDoc = docker False
instance Show Expression where show = render . toDoc


docker :: Bool -> Expression -> Doc
-- first arg: if true, then inside positional (=> use parens)
docker f (Id foo) = text foo
docker f (Strg foo) = text $ show foo
docker f (Num foo) = text $ show foo

docker f (List  xs) = brackets ( fsep $ punctuate comma $ map toDoc xs )
docker f (Tuple xs) = parens ( fsep $ punctuate comma $ map toDoc xs )

docker f (Positional fun args) =
    ( if f then parens else id )
    $ text fun <+> fsep (map (docker True) args)
docker f (Named fun binds) = 
    ( if f then parens else id )
    $ text fun <+> braces ( fsep $ punctuate comma $ do
	     ( b, v ) <- binds
	     return $ text b <+> equals <+> toDoc v )


	     














