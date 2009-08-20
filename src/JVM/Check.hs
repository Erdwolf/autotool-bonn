{-# LINE 1 "JVM/Check.hs.drift" #-}
-- -*- mode: haskell -*-

module JVM.Check where



import JVM.Type
import JVM.Builtin

import Machine.Numerical.Config

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Typeable

data Checker = Builtins (Set Statement)
	   | Smallnums Integer
     deriving Typeable

{-! for Checker derive: Reader, ToDoc , Haskell2Xml !-}

instance Check Checker Program where
    check (Builtins allowed) p = do
        inform $ text "erlaubt sind diese Rechenbefehle:"
        inform $ nest 4 $ toDoc allowed
        let you = mkSet $ do
	        b <- flatten p
	        guard $ is_builtin b
	        return b
        inform $ text "Sie benutzen:" <+> toDoc you
        let wrong = minusSet you allowed
        assert ( isEmptySet wrong ) $ text "sind alle zugelassen?"
    check (Smallnums allowed) p = do
        inform $ text "Push (i) ist nur erlaubt für  abs(i) <= " 
		   <+> toDoc allowed
        let you = mkSet $ do
        	    Push i <- flatten p
        	    return $ abs i
        inform $ text "Sie benutzen:" <+> toDoc you
        let wrong = sfilter ( > allowed ) you
        assert ( isEmptySet wrong ) $ text "sind alle zugelassen?"



    
instance Reader Checker where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
                      ((do guard (d < 9)
                           my_reserved "Builtins"
                           aa <- readerPrec 9
                           return (Builtins aa))
                       <|>
                       (do guard (d < 9)
                           my_reserved "Smallnums"
                           aa <- readerPrec 9
                           return (Smallnums aa)))

instance ToDoc Checker where
    toDocPrec d (Builtins aa) = docParen (d >= 10)
              (text "Builtins" </> fsep [toDocPrec 10 aa])
    toDocPrec d (Smallnums aa) = docParen (d >= 10)
              (text "Smallnums" </> fsep [toDocPrec 10 aa])

{-
instance Haskell2Xml Checker where
    toHType v =
        Defined "Checker" []
                [Constr "Builtins" [] [toHType aa],
                 Constr "Smallnums" [] [toHType ab]]
      where
        (Builtins aa) = v
        (Smallnums ab) = v
    fromContents (CElem (Elem constr [] cs):etc)
        | "Smallnums" `isPrefixOf` constr =
            (\(ab,_)-> (Smallnums ab, etc)) (fromContents cs)
        | "Builtins" `isPrefixOf` constr =
            (\(aa,_)-> (Builtins aa, etc)) (fromContents cs)
    toContents v@(Builtins aa) =
        [mkElemC (showConstr 0 (toHType v)) (toContents aa)]
    toContents v@(Smallnums ab) =
        [mkElemC (showConstr 1 (toHType v)) (toContents ab)]
-}

--  Imported from other files :-
