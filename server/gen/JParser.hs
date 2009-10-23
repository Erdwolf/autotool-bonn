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
    text "package" <+> text package <> text ";",
    text "import" <+> text (tpackage ++ ".*") <> text ";",
    text "import" <+> text "java.util.List" <> text ";",
    text "",
    text "@SuppressWarnings" <> text "(" <> string "unused" <> text ")"
 ]

jParser :: AData -> IO ()
jParser (AData ty@(AType nm tv) cons) | null tv = do
    vWriteFile (dir </> (nm ++ "Parser") <.> "java") $ show $ vcat $ header ++ [
        text "public" <+> text "class" <+> text (nm ++ "Parser"),
        block [
            text "private" <+> text "static" <+> text "final" <+> text "Parser"
                <> vars [AType nm []] <+> text "inst" <+> text "=",
            (nest 4 $ foldAlternatives nm tv $ map (makeAlternative nm tv) cons) <> text ";",
            text "",
            text "public" <+> text "static" <+> text "Parser"
                <> vars [AType nm []] <+> text "getInstance" <> text "()",
            block [
                text "return" <+> text "inst" <> text ";"
            ]
        ]
     ]
jParser (AData ty@(AType nm tv) cons) = do
    vWriteFile (dir </> (nm ++ "Parser") <.> "java") $ show $ vcat $ header ++ [
        text "public" <+> text "class" <+> tipe (AType (nm ++ "Parser") tv),
        nest 4 $ text "implements" <+> text "Parser" <> vars [ty],
        block $ [
            text "private" <+> text "final" <+> text "Parser" <> vars [ty] <+>
                 ident ["parser"] <> text ";",
            text "",
            text "public" <+> text (nm ++ "Parser") <> text "(" <> cat (punctuate (text ", ") [
                text "final" <+> text "Parser" <> vars [ty] <+> ident [nm, "parser"]
                | ty@(AVar nm) <- tv
            ]) <> text ")",
            block $ [
                text "parser" <+> text "=" <+>
                    (nest 4 $ foldAlternatives nm tv $ map (makeAlternative nm tv) cons) <> text ";"
            ],
            text "public" <+> tipe ty <+> text "parse" <> text "("
                     <> text "Object" <+> text "val" <> text ")"
                     <+> text "throws" <+> text "ParseErrorBase",
            block [
                text "return" <+> text "parser" <> text "." <> text "parse" <> text "(" <> text "val" <> text ")" <> text ";"
            ]
         ]
     ]

foldAlternatives :: String -> [AType] -> [Doc] -> Doc
foldAlternatives nm tv [x] = x
foldAlternatives nm tv (x:xs) = vcat [
    text "new" <+> text "AlternativeParser" <> vars [AType nm tv] <> text "(",
    nest 4 (x <> text "," $$ foldAlternatives nm tv xs),
    text ")"
 ]

makeAlternative :: String -> [AType] -> ACons -> Doc
makeAlternative nm tv con = let
    cn = consName con
    wrapField = case con of
        ACons {} -> \nm ty i f -> text "new" <+> text "ArrayElemParser" <>
            vars [ty] <> text "(" <> text (show i) <> text ","
                $$ nest 4 (f <> text ")")
        ARec {} ->  \nm ty i f -> text "new" <+> text "StructFieldParser" <>
            vars [ty] <> text "(" $$ nest 4 (string nm <> text ","
                $$ f <> text ")")
  in
    vcat [
        text "new" <+> text "StructFieldParser" <> vars [AType nm tv]
            <> text "(",
        nest 4 $ vcat [
            string cn <> text ",",
            text "new" <+> text "Parser" <> vars [AType nm tv] <> text "()",
            block $ concat [
                [
                    text "Parser" <> vars [ty'] <+> ident [nm', "parser"]
                        <+> text "=" <+> text "null" <> text ";"
                ]
                | (nm', ty') <- consArgs con
            ] ++ [
                text "",
                text "public" <+> tipe (AType nm tv) <+> text "parse" <> text "("
                     <> text "Object" <+> text "val" <> text ")"
                     <+> text "throws" <+> text "ParseErrorBase",
                block $ concat [
                    [
                        text "if" <+> text "(" <> ident [nm', "parser"] <+> text "==" <+> text "null" <> text ")",
                        nest 4 $ ident [nm', "parser"] <+> text "=" <+> wrapField nm' ty' i (makeTypeParser ty') <> text ";"
                    ]
                    | ((nm', ty'), i) <- zip (consArgs con) [0..]
                ] ++ [
                    text "return" <+> text "new" <+> tipe (AType cn tv) <> text "(",
                    nest 4 $ vcat (punctuate (text ",") [
                        unboxFunc ty (ident [nm, "parser"] <> text "." <> text "parse" <> text "(" <> text "val" <> text ")")
                        | (nm, ty) <- consArgs con
                    ]) <> text ")" <> text ";"
                ]
            ]
        ],
        text ")"
    ]

makeTypeParser :: AType -> Doc
makeTypeParser (AType nm tys) | null tys =
    text (upcase $ nm ++ "Parser") <> text "." <> text "getInstance" <> text "()"
makeTypeParser (AType nm tys) =
    text "new" <+> text (upcase $ nm ++ "Parser") <> vars tys <> text "(" <> cat (punctuate (text ", ") (map makeTypeParser tys)) <> text ")"
makeTypeParser (AVar nm) =
    ident [nm, "Parser"]
