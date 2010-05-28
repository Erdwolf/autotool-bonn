{-# LANGUAGE OverloadedStrings #-}

-- Generate Serializers for a data type.
--
-- Serializers are built from hand-written basic parsers, namely
--  StringSerializer, ...    (basic types)
--  EitherSerializer<A, B>   (Either a b)
--  ListSerializer<A>        ([a])
-- and the XmlRpc* data types from the Redstone XML-RPC library
--
-- See the corresponding Java code for details.

module JSerializer (
    dir,
    jSerializer
) where

import Types
import Java
import Basic
import Package

import Data.String
import System.FilePath
import Control.Monad
import Text.PrettyPrint.HughesPJ

package :: String
package = base ++ ".xmlrpc.serialize"

tpackage :: String
tpackage = base ++ ".types"

dir :: FilePath
dir = "out" </> "serialize"

clasz :: IsString s => s
clasz = "Serializer"

-- file header
header = [
    "package" <+> text package <> ";",
    "import" <+> text (tpackage ++ ".*") <> ";",
    "import" <+> "java.util.List" <> ";",
    "import" <+> "redstone.xmlrpc.*" <> ";",
    "",
    "@SuppressWarnings" <> "(" <> "{" <> string "unused" <> ","
        <+> string "unchecked" <> "}" <> ")"
 ]

-- generate serializer
jSerializer :: AData -> IO ()
-- no type arguments: provide singleton
jSerializer (AData ty@(AType nm tv) cons) | null tv =
    vWriteFile (dir </> (nm ++ clasz) <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "class" <+> text (nm ++ clasz),
        block [
            "private" <+> "static" <+> "final" <+> clasz
                <> vars [AType nm []] <+> "inst" <+> "=",
            nest 4 $ mkSerializer ty cons <> ";",
            "",
            "public" <+> "static" <+> clasz
                <> vars [AType nm []] <+> "getInstance" <> "()",
            block [
                "return" <+> "inst" <> ";"
            ]
        ]
   ]
jSerializer (AData ty@(AType nm tv) cons) =
    vWriteFile (dir </> (nm ++ clasz) <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "class" <+> tipe (AType (nm ++ clasz) tv),
        nest 4 $ "implements" <+> clasz <> vars [ty],
        block $ [
            "private" <+> "final" <+> clasz <> vars [ty] <+>
                 "serializer" <> ";",
            "",
            "public" <+> text (nm ++ clasz) <> "(" <> sep (punctuate "," [
                "final" <+> clasz <> vars [ty] <+> ident [nm, clasz]
                | ty@(AVar nm) <- tv
            ]) <> ")",
            block $ [
                "serializer" <+> "=",
                nest 4 $ mkSerializer ty cons <> ";"
            ],
            "public" <+> "Object" <+> "serialize" <> "("
                     <> tipe ty <+> "val" <> ")",
            block [
                "return" <+> "serializer" <> "." <> "serialize"
                    <> "(" <> "val" <> ")" <> ";"
            ]
        ]
    ]

-- make serializers for all alternatives, then pick the right one
mkSerializer :: AType -> [ACons] -> Doc
mkSerializer ty@(AType nm tv) [con] | consName con == nm
    = mkSingleSerializer ty con
mkSerializer ty@(AType nm tv) cons = vcat [
    "new" <+> clasz <> vars [ty] <> "()",
    block $ concat [
        [
             clasz <> vars [ty'] <+> ident [nm', clasz] <+> "=",
             nest 4 $ mkSingleSerializer ty' con <> ";"
        ]
        | con <- cons, let nm' = consName con, let ty' = AType nm' tv
    ] ++ [
         "public" <+> "Object" <+> "serialize"
             <> "(" <> tipe ty <+> "val" <> ")",
         block $ concat [
             [
                 "if" <+> "(" <> "val" <> "." <> ident ["is", nm'] <> "()" <> ")",
                 nest 4 $ "return" <+> ident [nm', clasz] <> "."
                     <> "serialize" <> "(" <> "val" <> "."
                     <> ident ["get", nm'] <> "()" <> ")" <> ";"
             ]
             | con <- cons, let nm' = consName con
         ] ++ [
             "return" <+> "null" <> ";"
         ]
    ]
 ]

-- make a serializer for the given type constructor
mkSingleSerializer :: AType -> ACons -> Doc
mkSingleSerializer ty@(AType nm tv) con = vcat [
    "new" <+> clasz <> vars [ty] <> "()",
    block $ [
         clasz <> vars [ty'] <+> ident [nm', clasz] <+> "=" <+> "null" <> ";"
         | (nm', ty') <- consArgs con
    ] ++ [
         "",
         "public" <+> "Object" <+> "serialize"
             <> "(" <> tipe ty <+> "val" <> ")",
         block $ concat [
              [
                  "if" <+> "(" <> ident [nm', clasz] <+> "==" <+> "null" <> ")",
                  nest 4 $ ident [nm', clasz] <+> "="
                      <+> makeTypeSerializer ty' <> ";"
              ]
              | (nm', ty') <- consArgs con
         ] ++ case con of
         -- named fields: use XmlRpcStruct
         ARec {} -> [
             "",
             "XmlRpcStruct" <+> "inner" <+> "="
                 <+> "new" <+> "XmlRpcStruct" <> "()" <> ";"
          ] ++ [
             "inner" <> "." <> "put" <> "(" <> string nm' <> ","
                 <+> ident [nm', clasz] <> "." <> "serialize" <> "("
                 <> boxFunc ty' ("val" <> "." <> ident ["get", nm'] <> "()")
                 <> ")" <> ")" <> ";"
             | (nm', ty') <- consArgs con
          ]
         -- anonymous fields: use XmlRpcArray
         ACons {} -> [
             "",
             "XmlRpcArray" <+> "inner" <+> "=" <+> "new"
                 <+> "XmlRpcArray" <> "()" <> ";"
          ] ++ [
             "inner" <> "." <> "add" <> "(" <> boxFunc ty' ("val" <> "."
                 <> ident ["get", nm'] <> "()") <> ")" <> ";"
             | (nm', ty') <- consArgs con
          ]
        ++ [
          "",
          "XmlRpcStruct" <+> "outer" <+> "=" <+> "new"
              <+> "XmlRpcStruct" <> "()" <> ";",
          "outer" <> "." <> "put"
              <> "(" <> string nm <> "," <+> "inner" <> ")" <> ";",
          "return" <+> "outer" <> ";"
        ]
    ]
 ]

-- construct a serializer for the given type
makeTypeSerializer :: AType -> Doc
makeTypeSerializer (AType nm tys) | null tys =
    text (upcase $ nm ++ clasz) <> "." <> "getInstance" <> "()"
makeTypeSerializer (AType nm tys) =
    "new" <+> text (upcase $ nm ++ clasz) <> vars tys <> "("
        <> sep (punctuate "," (map makeTypeSerializer tys)) <> ")"
makeTypeSerializer (AVar nm) =
    ident [nm, clasz]
