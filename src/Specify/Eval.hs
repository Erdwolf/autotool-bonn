module Specify.Eval where



import Specify.Expression
import Specify.Definition

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reporter


eval :: ToDoc a
     => Program 
     -> Expression a 
     -> Reporter ( Maybe a )
eval p x = case x of

        Constant c -> return $ Just c
        Undefined  -> return $ Nothing

	Apply fun args -> do
	    d @ ( Definition _ params body ) <- find p fun
	    when ( length args /= length params ) $ reject $ vcat
	        [ text "Ausdruck:" <+> toDoc x
		, text "Anzahl der Argumente paßt nicht:"
		, text "Deklaration:" <+> toDoc d
		]
            values <- mapM ( eval p ) args
            -- static binding: evaluate in fresh environment
            res <- eval ( make $ zip params values ) body
	    when ( not $ null args ) $ inform 
		 $  toDoc fun <+> parens ( sepBy comma $ map ( toDoc . einpack ) values ) 
			      <+> equals <+> toDoc ( einpack res )
            return res

	Branch c y z -> do
           mcc <- eval p c
           case mcc of
                Nothing -> return Nothing
                Just cc -> eval p $ if cc then y else z
     
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
    -> Reporter ( Maybe c )
bin op p x y = do
    ma <- eval p x
    mb <- eval p y
    case ( ma, mb ) of
        ( Just a, Just b ) -> return $ Just $ op a b
        _                  -> return $ Nothing

bin_nonzero op p x y = do
    ma <- eval p x
    mb <- eval p y
    case ( ma, mb ) of
        ( _ , Just 0 ) -> reject $ text "division by zero"
        ( Just a, Just b ) -> return $ Just $ op a b
        _                  -> return $ Nothing

un :: ( ToDoc a, ToDoc b )
   => ( a -> b ) 
   -> Program 
   -> Expression a 
   -> Reporter ( Maybe b )
un op p x = do
    ma <- eval p x
    return $ fmap op ma

