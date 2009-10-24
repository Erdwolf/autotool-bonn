{-# LANGUAGE OverloadedStrings #-}

module Basic (
    basic,
    unbox, unboxed, unboxFunc, boxFunc
) where

import Types
import Util ()
import Text.PrettyPrint.HughesPJ

basic :: String -> AType
basic "Name" = basic "String"
basic "Task" = basic "String"
basic "Seed" = basic "String"
basic "Signature" = basic "Digest"
basic "Digest" = basic "String"
basic "Integer" = AType "BigInteger" []
basic "Int" = AType "Integer" []
basic xs = AType xs []

unbox :: AType -> AType
unbox (AType "Integer" []) = AType "int" []
unbox (AType "Double" []) = AType "double" []
unbox (AType "Boolean" []) = AType "boolean" []
unbox t = t

unboxed :: AType -> Bool
unboxed  (AType "Integer" []) = True
unboxed (AType "Double" []) = True
unboxed (AType "Boolean" []) = True
unboxed _ = False

unboxFunc :: AType -> Doc -> Doc
unboxFunc (AType "Integer" []) d = d <> "." <> "intValue" <> "()"
unboxFunc (AType "Double" []) d = d <> "." <> "doubleValue" <> "()"
unboxFunc (AType "Boolean" []) d = d <> "." <> "booleanValue" <> "()"
unboxFunc _ d = d

boxFunc (AType "Integer" []) d = "new" <+> "Integer" <> "(" <> d <> ")"
boxFunc (AType "Double" []) d = "new" <+> "Double" <> "(" <> d <> ")"
boxFunc (AType "Boolean" []) d = "new" <+> "Boolean" <> "(" <> d <> ")"
boxFunc _ d = d
