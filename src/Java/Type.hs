module Java.Type where

-- $Id$

import ToDoc

data Java = Java [ Statement ]

data Statement = Block  [ Statement ]
	       | Assign Expression Expression
	       | While  Expression Statement
	       | If     Expression Statement Statement
	       | Declare String String
	       | Return Expression

data Expression = Number Integer
		| Name String
		| Operation String Expression Expression
		| Paren Expression

---------------------------------------------------------------------------

instance Show Java where show = render . toDoc

instance ToDoc Java where
    toDoc ( Java sts ) = vcat $ map toDoc sts 

instance Show Statement where show = render . toDoc

instance ToDoc Statement where
    toDoc ( Block sts ) = braces $ vcat $ map toDoc sts
    toDoc ( While e st ) = text "while" <+> toDoc e <+> toDoc st
    toDoc ( If e y n ) = text "if" <+> toDoc e <+> toDoc y
			     <+> text "else" <+> toDoc n
    toDoc ( Assign l r ) = toDoc l <+> equals <+> toDoc r <+> semi
    toDoc ( Declare ty na ) = text ty <+> text na <+> semi
    toDoc ( Return e ) = text "return" <+> toDoc e <+> semi

instance Show Expression where show = render . toDoc

instance ToDoc Expression where
    toDoc ( Number i ) = toDoc i
    toDoc ( Name n ) = text n
    toDoc ( Operation op l r ) = toDoc l <+> text op <+> toDoc r
    toDoc ( Paren e ) = parens $ toDoc e

---------------------------------------------------------------------------

example :: Java
example = Java 
	[ Declare "int" "a"
	, Assign ( Name "a" ) ( Number 0 )
	, While ( Paren $ Operation ">" ( Name "x0" ) ( Number 0 ) )
	        $ Block [ Assign ( Name "x0" ) 
			         ( Operation "-" ( Name "x0" ) ( Number 1 ) )
			, Assign ( Name "a" ) 
			         ( Operation "+" ( Name "a" ) ( Number 1 ) )
			]
	, Return ( Name "a" )
	]
