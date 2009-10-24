{-# LANGUAGE OverloadedStrings #-}

module JSerializer (
    dir,
    jSerializer
) where

import Types
import Java
import Basic

import Data.String
import System.FilePath
import Control.Monad
import Text.PrettyPrint.HughesPJ

package :: String
package = "de.htwk.autolat.connector.xmlrpc.serialize"

tpackage :: String
tpackage = "de.htwk.autolat.connector.types"

dir :: FilePath
dir = "out" </> "serialize"

clasz :: IsString s => s
clasz = "Serializer"

header = [
    "package" <+> text package <> ";",
    "import" <+> text (tpackage ++ ".*") <> ";",
    "import" <+> "java.util.List" <> ";",
    "import" <+> "redstone.xmlrpc.*" <> ";",
    "",
    "@SuppressWarnings" <> "(" <> "{" <> string "unused" <> ","
        <+> string "unchecked" <> "}" <> ")"
 ]

jSerializer :: AData -> IO ()
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

makeTypeSerializer :: AType -> Doc
makeTypeSerializer (AType nm tys) | null tys =
    text (upcase $ nm ++ clasz) <> "." <> "getInstance" <> "()"
makeTypeSerializer (AType nm tys) =
    "new" <+> text (upcase $ nm ++ clasz) <> vars tys <> "("
        <> sep (punctuate "," (map makeTypeSerializer tys)) <> ")"
makeTypeSerializer (AVar nm) =
    ident [nm, clasz]
