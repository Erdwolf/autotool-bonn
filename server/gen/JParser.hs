{-# LANGUAGE OverloadedStrings #-}

module JParser (
    dir,
    jParser
) where

import Types
import Java
import Basic

import System.FilePath
import Control.Monad
import Text.PrettyPrint.HughesPJ

package :: String
package = "de.htwk.autolat.connector.xmlrpc.parse"

tpackage :: String
tpackage = "de.htwk.autolat.connector.types"

dir :: FilePath
dir = "out" </> "parse"

header = [
    "package" <+> text package <> ";",
    "import" <+> text (tpackage ++ ".*") <> ";",
    "import" <+> "java.util.List" <> ";",
    "",
    "@SuppressWarnings" <> "(" <> string "unused" <> ")"
 ]

jParser :: AData -> IO ()
jParser (AData ty@(AType nm tv) cons) | null tv = do
    vWriteFile (dir </> (nm ++ "Parser") <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "class" <+> text (nm ++ "Parser"),
        block [
            "private" <+> "static" <+> "final" <+> "Parser"
                <> vars [AType nm []] <+> "inst" <+> "=",
            (nest 4 $ foldAlternatives nm tv $ map (makeAlternative nm tv) cons) <> ";",
            "",
            "public" <+> "static" <+> "Parser"
                <> vars [AType nm []] <+> "getInstance" <> "()",
            block [
                "return" <+> "inst" <> ";"
            ]
        ]
     ]
jParser (AData ty@(AType nm tv) cons) = do
    vWriteFile (dir </> (nm ++ "Parser") <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "class" <+> tipe (AType (nm ++ "Parser") tv),
        nest 4 $ "implements" <+> "Parser" <> vars [ty],
        block $ [
            "private" <+> "final" <+> "Parser" <> vars [ty] <+>
                 ident ["parser"] <> ";",
            "",
            "public" <+> text (nm ++ "Parser") <> "(" <> sep (punctuate "," [
                "final" <+> "Parser" <> vars [ty] <+> ident [nm, "parser"]
                | ty@(AVar nm) <- tv
            ]) <> ")",
            block $ [
                "parser" <+> "=" <+>
                    (nest 4 $ foldAlternatives nm tv $ map (makeAlternative nm tv) cons) <> ";"
            ],
            "public" <+> tipe ty <+> "parse" <> "("
                     <> "Object" <+> "val" <> ")"
                     <+> "throws" <+> "ParseErrorBase",
            block [
                "return" <+> "parser" <> "." <> "parse" <> "(" <> "val" <> ")" <> ";"
            ]
         ]
     ]

foldAlternatives :: String -> [AType] -> [Doc] -> Doc
foldAlternatives nm tv [x] = x
foldAlternatives nm tv (x:xs) = vcat [
    "new" <+> "AlternativeParser" <> vars [AType nm tv] <> "(",
    nest 4 (x <> "," $$ foldAlternatives nm tv xs),
    ")"
 ]

makeAlternative :: String -> [AType] -> ACons -> Doc
makeAlternative nm tv con = let
    cn = consName con
    wrapField = case con of
        ACons {} -> \nm ty i f -> "new" <+> "ArrayElemParser" <>
            vars [ty] <> "(" <> text (show i) <> ","
                $$ nest 4 (f <> ")")
        ARec {} ->  \nm ty i f -> "new" <+> "StructFieldParser" <>
            vars [ty] <> "(" $$ nest 4 (string nm <> ","
                $$ f <> ")")
  in
    vcat [
        "new" <+> "StructFieldParser" <> vars [AType nm tv]
            <> "(",
        nest 4 $ vcat [
            string cn <> ",",
            "new" <+> "Parser" <> vars [AType nm tv] <> "()",
            block $ concat [
                [
                    "Parser" <> vars [ty'] <+> ident [nm', "parser"]
                        <+> "=" <+> "null" <> ";"
                ]
                | (nm', ty') <- consArgs con
            ] ++ [
                "",
                "public" <+> tipe (AType nm tv) <+> "parse" <> "("
                     <> "Object" <+> "val" <> ")"
                     <+> "throws" <+> "ParseErrorBase",
                block $ concat [
                    [
                        "if" <+> "(" <> ident [nm', "parser"] <+> "==" <+> "null" <> ")",
                        nest 4 $ ident [nm', "parser"] <+> "=" <+> wrapField nm' ty' i (makeTypeParser ty') <> ";"
                    ]
                    | ((nm', ty'), i) <- zip (consArgs con) [0..]
                ] ++ [
                    "return" <+> "new" <+> tipe (AType cn tv) <> "(",
                    nest 4 $ vcat (punctuate "," [
                        unboxFunc ty (ident [nm, "parser"] <> "." <> "parse" <> "(" <> "val" <> ")")
                        | (nm, ty) <- consArgs con
                    ]) <> ")" <> ";"
                ]
            ]
        ],
        ")"
    ]

makeTypeParser :: AType -> Doc
makeTypeParser (AType nm tys) | null tys =
    text (upcase $ nm ++ "Parser") <> "." <> "getInstance" <> "()"
makeTypeParser (AType nm tys) =
    "new" <+> text (upcase $ nm ++ "Parser") <> vars tys <> "(" <> sep (punctuate "," (map makeTypeParser tys)) <> ")"
makeTypeParser (AVar nm) =
    ident [nm, "Parser"]
