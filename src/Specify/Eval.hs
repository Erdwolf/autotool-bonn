{-# OPTIONS -fglasgow-exts #-}

module Specify.Eval where



import Specify.Expression
import Specify.Definition

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reporter


eval :: ToDoc a
     => Program -> Expression a -> Reporter a
eval p x = case x of

        Constant c -> return c

	Apply fun args -> do
	    d @ ( Definition _ params body ) <- find p fun
	    when ( length args /= length params ) $ reject $ vcat
	        [ text "Ausdruck:" <+> toDoc x
		, text "Anzahl der Argumente paßt nicht:"
		, text "Deklaration:" <+> toDoc d
		]
            values <- mapM ( eval p ) args
            eval ( extend p $ zip params values ) body

	Branch c y z -> do
           cc <- eval p c
           eval p $ if cc then y else z
     
        Plus      x y -> bin (+) p x y
        Minus     x y -> bin (-) p x y
        Negate    x   -> un  negate p x
        Times     x y -> bin (*) p x y
        Quotient  x y -> bin_nonzero div p x y
        Remainder x y -> bin_nonzero mod p x y

        Less      x y -> bin ( < ) p x y
        LessEqual x y -> bin ( <= ) p x y
        Equal     x y -> bin ( == ) p x y
        GreaterEqual x y -> bin ( >= ) p x y
        Greater   x y -> bin ( > ) p x y
        NotEqual  x y -> bin ( /= ) p x y

        Or        x y -> bin ( || ) p x y
        And       x y -> bin ( && ) p x y
        Implies   x y -> bin ( >= ) p x y
        Not       x   -> un  not    p x

bin :: ( ToDoc a, ToDoc b, ToDoc c )
    => ( a -> b -> c ) 
    -> Program 
    -> Expression a 
    -> Expression b 
    -> Reporter c
bin op p x y = do
    a <- eval p x
    b <- eval p y
    return $ op a b

bin_nonzero op p x y = do
    a <- eval p x
    b <- eval p y
    when ( 0 == b ) $ reject $ text "division by zero"
    return $ op a b

un :: ( ToDoc a, ToDoc b )
   => ( a -> b ) 
   -> Program 
   -> Expression a 
   -> Reporter b
un op p x = do
    a <- eval p x
    return $ op a

