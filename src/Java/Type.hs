module Java.Type where

-- $Id$

import ToDoc

data Java = Java [ Statement ]

data Statement = Block  [ Statement ]
	       | Exp Expression
	       | While  Expression Statement
	       | If     Expression Statement Statement
	       | Declare_Int Expression
	       | Return Expression

data Expression = Number Integer
		| Name String
		| Operation String Expression Expression
		| Parens Expression

---------------------------------------------------------------------------

instance Show Java where show = render . toDoc

instance ToDoc Java where
    toDoc ( Java sts ) = vcat $ map toDoc sts 

instance Show Statement where show = render . toDoc

instance ToDoc Statement where
    toDoc ( Block sts ) = braces $ vcat $ map toDoc sts
    toDoc ( While e st ) = text "while" <+> parens (toDoc e) <+> toDoc st
    toDoc ( If e y n ) = text "if" <+> parens (toDoc e) <+> toDoc y
			     <+> text "else" <+> toDoc n

    toDoc ( Exp e ) = toDoc e <+> semi
    toDoc ( Declare_Int e ) = text "int" <+> toDoc e <> semi
    toDoc ( Return e ) = text "return" <+> toDoc e <> semi

instance Show Expression where show = render . toDoc

instance ToDoc Expression where
    toDoc ( Number i ) = toDoc i
    toDoc ( Name n ) = text n
    toDoc ( Operation op l r ) = toDoc l <+> text op <+> toDoc r
    toDoc ( Parens e ) = parens $ toDoc e

---------------------------------------------------------------------------

assign l r = Exp $ Operation "=" l r

example :: Java
example = Java 
	[ Declare_Int ( Name "a" )
	, assign ( Name "a" ) ( Number 0 )
	, While ( Operation ">" ( Name "x0" ) ( Number 0 ) )
	        $ Block [ assign ( Name "x0" ) 
			         ( Operation "-" ( Name "x0" ) ( Number 1 ) )
			, assign ( Name "a" ) 
			         ( Operation "+" ( Name "a" ) ( Number 1 ) )
			]
	, Return ( Name "a" )
	]
