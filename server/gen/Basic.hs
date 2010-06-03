{-# LANGUAGE OverloadedStrings #-}

-- deal with basic types

module Basic (
    basic,
    unbox, unboxed, unboxFunc, boxFunc
) where

import Types
import Util ()
import Text.PrettyPrint.HughesPJ

-- recognize basic types
basic :: String -> AType
-- newtypes
basic "Config" = basic "String"
basic "Solution" = basic "String"
basic "Description" = basic "String"
-- aliases
basic "Name" = basic "String"
basic "Task" = basic "String"
basic "Seed" = basic "String"
basic "Signature" = basic "Digest"
basic "Digest" = basic "String"
basic "Integer" = AType "BigInteger" []
basic "Int" = AType "Integer" []
basic xs = AType xs []

-- find unboxed equivalent of a given type
unbox :: AType -> AType
unbox (AType "Integer" []) = AType "int" []
unbox (AType "Double" []) = AType "double" []
unbox (AType "Boolean" []) = AType "boolean" []
unbox t = t

-- determine whether a type can be unboxed
unboxed :: AType -> Bool
unboxed  (AType "Integer" []) = True
unboxed (AType "Double" []) = True
unboxed (AType "Boolean" []) = True
unboxed _ = False

-- unbox a boxed value, if possible
unboxFunc :: AType -> Doc -> Doc
unboxFunc (AType "Integer" []) d = d <> "." <> "intValue" <> "()"
unboxFunc (AType "Double" []) d = d <> "." <> "doubleValue" <> "()"
unboxFunc (AType "Boolean" []) d = d <> "." <> "booleanValue" <> "()"
unboxFunc _ d = d

-- rebox an unboxed value, if possible
boxFunc (AType "Integer" []) d = "new" <+> "Integer" <> "(" <> d <> ")"
boxFunc (AType "Double" []) d = "new" <+> "Double" <> "(" <> d <> ")"
boxFunc (AType "Boolean" []) d = "new" <+> "Boolean" <> "(" <> d <> ")"
boxFunc _ d = d
