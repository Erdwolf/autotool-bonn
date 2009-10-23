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
    text "package" <+> text package <> text ";",
    text "import" <+> text (tpackage ++ ".*") <> text ";",
    text "import" <+> text "java.util.List" <> text ";",
    text "import" <+> text "redstone.xmlrpc.*" <> text ";",
    text ""
 ]

jSerializer :: AData -> IO ()
jSerializer (AData ty@(AType nm tv) cons) | null tv = do
    vWriteFile (dir </> (nm ++ "Serializer") <.> "java") $ show $ vcat $ header ++ [
        text "public" <+> text "class" <+> text (nm ++ "Serializer"),
        block [
            text "private" <+> text "static" <+> text "final" <+> text "Serializer"
                <> vars [AType nm []] <+> text "inst" <+> text "=",
            nest 4 $ mkSerializer ty cons <> text ";",
            text "",
            text "public" <+> text "static" <+> text "Serializer"
                <> vars [AType nm []] <+> text "getInstance" <> text "()",
            block [
                text "return" <+> text "inst" <> text ";"
            ]
        ]
     ]
jSerializer (AData ty@(AType nm tv) cons) = do
    vWriteFile (dir </> (nm ++ "Serializer") <.> "java") $ show $ vcat $ header ++ [
        text "public" <+> text "class" <+> tipe (AType (nm ++ "Serializer") tv),
        nest 4 $ text "implements" <+> text "Serializer" <> vars [ty],
        block $ [
            text "private" <+> text "final" <+> text "Serializer" <> vars [ty] <+>
                 ident ["serializer"] <> text ";",
            text "",
            text "public" <+> text (nm ++ "Serializer") <> text "(" <> cat (punctuate (text ", ") [
                text "final" <+> text "Serializer" <> vars [ty] <+> ident [nm, "serializer"]
                | ty@(AVar nm) <- tv
            ]) <> text ")",
            block $ [
                text "serializer" <+> text "=",
                nest 4 $ mkSerializer ty cons <> text ";"
            ],
            text "public" <+> text "Object" <+> text "serialize" <> text "("
                     <> tipe ty <+> text "val" <> text ")",
            block [
                text "return" <+> text "serializer" <> text "." <> text "serialize" <> text "(" <> text "val" <> text ")" <> text ";"
            ]
         ]
     ]

mkSerializer :: AType -> [ACons] -> Doc
mkSerializer ty@(AType nm tv) [con] = vcat [
    text "new" <+> text "Serializer" <> vars [ty] <> text "()",
    block $ [
         text "Serializer" <> vars [ty'] <+> ident [nm', "serializer"] <+> text "=" <+> text "null" <> text ";"
         | (nm', ty') <- consArgs con
    ] ++ [
         text "",
         text "public" <+> text "Object" <+> text "serialize" <> text "(" <> tipe ty <+> text "val" <> text ")",
         block $ concat [
              [
                  text "if" <+> text "(" <> ident [nm', "serializer"] <+> text "==" <+> text "null" <> text ")",
                  nest 4 $ ident [nm', "serializer"] <+> text "=" <+> makeTypeSerializer ty' <> text ";"
              ]
              | (nm', ty') <- consArgs con
         ] ++ case con of
         ARec {} -> [
             text "",
             text "XmlRpcStruct" <+> text "inner" <+> text "=" <+> text "new" <+> text "XmlRpcStruct" <> text "()" <> text ";"
          ] ++ [
             text "inner" <> text "." <> text "put" <> text "(" <> string nm' <> text "," <+> ident [nm', "serializer"] <> text "." <> text "serialize" <> text "(" <> boxFunc ty' (text "val" <> text "." <> ident ["get", nm'] <> text "()") <> text ")" <> text ")" <> text ";"
             | (nm', ty') <- consArgs con
          ]
         ACons {} -> [
             text "",
             text "XmlRpcArray" <+> text "inner" <+> text "=" <+> text "new" <+> text "XmlRpcArray" <> text "()" <> text ";"
          ] ++ [
             text "inner" <> text "." <> text "add" <> text "(" <> boxFunc ty' (text "val" <> text "." <> ident ["get", nm'] <> text "()") <> text ")" <> text ";"
             | (nm', ty') <- consArgs con
          ]
        ++ [
          text "",
          text "XmlRpcStruct" <+> text "outer" <+> text "=" <+> text "new" <+> text "XmlRpcStruct" <> text "()" <> text ";",
          text "outer" <> text "." <> text "put" <> text "(" <> string nm <> text "," <+> text "inner" <> text ")" <> text ";",
          text "return" <+> text "outer" <> text ";"
        ]
    ]
 ]
mkSerializer ty@(AType nm tv) cons = vcat [
    text "new" <+> text "Serializer" <> vars [ty] <> text "()",
    block $ concat [
        [
             text "Serializer" <> vars [ty] <+> ident [nm', "serializer"] <+> text "=",
             mkSerializer ty [con]
        ]
        | con <- cons, let nm' = consName con
    ] ++ [
         text "public" <+> text "Object" <+> text "serialize" <> text "(" <> tipe ty <+> text "val" <> text ")",
         block $ concat [
             [
                 text "if" <+> text "(" <> text "val" <> text "." <> ident ["is", nm'] <> text "()" <> text ")",
                 nest 4 $ text "return" <+> ident [nm', "serializer"] <> text "." <> text "serialize" <> text "(" <> text "val" <> text ")" <> text ";"
             ]
             | con <- cons, let nm' = consName con
         ] ++ [
             text "return" <+> text "null" <> text ";"
         ]
    ]
 ]

makeTypeSerializer :: AType -> Doc
makeTypeSerializer (AType nm tys) | null tys =
    text (upcase $ nm ++ "Serializer") <> text "." <> text "getInstance" <> text "()"
makeTypeSerializer (AType nm tys) =
    text "new" <+> text (upcase $ nm ++ "Serializer") <> vars tys <> text "(" <> cat (punctuate (text ", ") (map makeTypeSerializer tys)) <> text ")"
makeTypeSerializer (AVar nm) =
    ident [nm, "Serializer"]
