{-# LANGUAGE OverloadedStrings #-}

module JSerializer (
    dir,
    jSerializer
) where

import Types
import Java
import Basic

import System.FilePath
import Control.Monad
import Text.PrettyPrint.HughesPJ

package :: String
package = "de.htwk.autolat.connector.xmlrpc.serialize"

tpackage :: String
tpackage = "de.htwk.autolat.connector.types"

dir :: FilePath
dir = "out" </> "serializer"

header = [
    "package" <+> text package <> ";",
    "import" <+> text (tpackage ++ ".*") <> ";",
    "import" <+> "java.util.List" <> ";",
    "import" <+> "redstone.xmlrpc.*" <> ";",
    ""
 ]

jSerializer :: AData -> IO ()
jSerializer (AData ty@(AType nm tv) cons) | null tv = do
    vWriteFile (dir </> (nm ++ "Serializer") <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "class" <+> text (nm ++ "Serializer"),
        block [
            "private" <+> "static" <+> "final" <+> "Serializer"
                <> vars [AType nm []] <+> "inst" <+> "=",
            nest 4 $ mkSerializer ty cons <> ";",
            "",
            "public" <+> "static" <+> "Serializer"
                <> vars [AType nm []] <+> "getInstance" <> "()",
            block [
                "return" <+> "inst" <> ";"
            ]
        ]
     ]
jSerializer (AData ty@(AType nm tv) cons) = do
    vWriteFile (dir </> (nm ++ "Serializer") <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "class" <+> tipe (AType (nm ++ "Serializer") tv),
        nest 4 $ "implements" <+> "Serializer" <> vars [ty],
        block $ [
            "private" <+> "final" <+> "Serializer" <> vars [ty] <+>
                 ident ["serializer"] <> ";",
            "",
            "public" <+> text (nm ++ "Serializer") <> "(" <> sep (punctuate "," [
                "final" <+> "Serializer" <> vars [ty] <+> ident [nm, "serializer"]
                | ty@(AVar nm) <- tv
            ]) <> ")",
            block $ [
                "serializer" <+> "=",
                nest 4 $ mkSerializer ty cons <> ";"
            ],
            "public" <+> "Object" <+> "serialize" <> "("
                     <> tipe ty <+> "val" <> ")",
            block [
                "return" <+> "serializer" <> "." <> "serialize" <> "(" <> "val" <> ")" <> ";"
            ]
         ]
     ]

mkSerializer :: AType -> [ACons] -> Doc
mkSerializer ty@(AType nm tv) [con] = vcat [
    "new" <+> "Serializer" <> vars [ty] <> "()",
    block $ [
         "Serializer" <> vars [ty'] <+> ident [nm', "serializer"] <+> "=" <+> "null" <> ";"
         | (nm', ty') <- consArgs con
    ] ++ [
         "",
         "public" <+> "Object" <+> "serialize" <> "(" <> tipe ty <+> "val" <> ")",
         block $ concat [
              [
                  "if" <+> "(" <> ident [nm', "serializer"] <+> "==" <+> "null" <> ")",
                  nest 4 $ ident [nm', "serializer"] <+> "=" <+> makeTypeSerializer ty' <> ";"
              ]
              | (nm', ty') <- consArgs con
         ] ++ case con of
         ARec {} -> [
             "",
             "XmlRpcStruct" <+> "inner" <+> "=" <+> "new" <+> "XmlRpcStruct" <> "()" <> ";"
          ] ++ [
             "inner" <> "." <> "put" <> "(" <> string nm' <> "," <+> ident [nm', "serializer"] <> "." <> "serialize" <> "(" <> boxFunc ty' ("val" <> "." <> ident ["get", nm'] <> "()") <> ")" <> ")" <> ";"
             | (nm', ty') <- consArgs con
          ]
         ACons {} -> [
             "",
             "XmlRpcArray" <+> "inner" <+> "=" <+> "new" <+> "XmlRpcArray" <> "()" <> ";"
          ] ++ [
             "inner" <> "." <> "add" <> "(" <> boxFunc ty' ("val" <> "." <> ident ["get", nm'] <> "()") <> ")" <> ";"
             | (nm', ty') <- consArgs con
          ]
        ++ [
          "",
          "XmlRpcStruct" <+> "outer" <+> "=" <+> "new" <+> "XmlRpcStruct" <> "()" <> ";",
          "outer" <> "." <> "put" <> "(" <> string nm <> "," <+> "inner" <> ")" <> ";",
          "return" <+> "outer" <> ";"
        ]
    ]
 ]
mkSerializer ty@(AType nm tv) cons = vcat [
    "new" <+> "Serializer" <> vars [ty] <> "()",
    block $ concat [
        [
             "Serializer" <> vars [ty] <+> ident [nm', "serializer"] <+> "=",
             mkSerializer ty [con]
        ]
        | con <- cons, let nm' = consName con
    ] ++ [
         "public" <+> "Object" <+> "serialize" <> "(" <> tipe ty <+> "val" <> ")",
         block $ concat [
             [
                 "if" <+> "(" <> "val" <> "." <> ident ["is", nm'] <> "()" <> ")",
                 nest 4 $ "return" <+> ident [nm', "serializer"] <> "." <> "serialize" <> "(" <> "val" <> ")" <> ";"
             ]
             | con <- cons, let nm' = consName con
         ] ++ [
             "return" <+> "null" <> ";"
         ]
    ]
 ]

makeTypeSerializer :: AType -> Doc
makeTypeSerializer (AType nm tys) | null tys =
    text (upcase $ nm ++ "Serializer") <> "." <> "getInstance" <> "()"
makeTypeSerializer (AType nm tys) =
    "new" <+> text (upcase $ nm ++ "Serializer") <> vars tys <> "(" <> sep (punctuate "," (map makeTypeSerializer tys)) <> ")"
makeTypeSerializer (AVar nm) =
    ident [nm, "Serializer"]
