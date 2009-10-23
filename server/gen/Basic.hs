module Basic (
    basic,
    unbox, unboxed, unboxFunc, boxFunc
) where

import Types
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
unboxFunc (AType "Integer" []) d = d <> text "." <> text "intValue" <> text "()"
unboxFunc (AType "Double" []) d = d <> text "." <> text "doubleValue" <> text "()"
unboxFunc (AType "Boolean" []) d = d <> text "." <> text "booleanValue" <> text "()"
unboxFunc _ d = d

boxFunc (AType "Integer" []) d = text "new" <+> text "Integer" <> text "(" <> d <> text ")"
boxFunc (AType "Double" []) d = text "new" <+> text "Double" <> text "(" <> d <> text ")"
boxFunc (AType "Boolean" []) d = text "new" <+> text "Boolean" <> text "(" <> d <> text ")"
boxFunc _ d = d
