{-# LANGUAGE OverloadedStrings #-}

module JData (
    dir,
    jData
) where

import Types
import Basic
import Java

import System.FilePath
import Control.Monad
import Text.PrettyPrint.HughesPJ

package :: String
package = "de.htwk.autolat.connector.types"

dir :: FilePath
dir = "out" </> "types"

header = [
    "package" <+> text package <> ";",
    "import" <+> "java.util.List" <> ";",
    "",
    "@SuppressWarnings" <> "(" <> string "unused" <> ")"
 ]

jData :: AData -> IO ()
jData (AData ty@(AType nm _) [con]) | nm == consName con = do
    vWriteFile (dir </> nm <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "class" <+> tipe ty,
        block $ contents ty con
     ]
jData (AData ty@(AType nm tv) cons) = do
    vWriteFile (dir </> nm <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "interface" <+> tipe ty,
        block $ [
             -- getters
             "public" <+> text cn <+> text ("get" ++ cn) <> "()" <> ";"
             | con <- cons, let cn = consName con
        ] ++ [
             -- distinguishers
             "public" <+> "boolean" <+> text ("is" ++ cn) <> "()" <> ";"
             | con <- cons, let cn = consName con
        ]
     ]
    forM_ cons $ \con -> do
        let cn = consName con
        vWriteFile (dir </> cn <.> "java") $ show $ vcat $ header ++ [
            "public" <+> "class" <+> text cn <+> "implements" <+> tipe ty,
            block $ contents ty con ++ concat [
                -- getters
                [
                    "@Override",
                    "public" <+> text cn' <+> text ("get" ++ cn') <> "()",
                    block [
                         "return" <+> res <> ";"
                         | let res = if cn == cn' then "this" else "null"
                    ]
                ]
                | con' <- cons, let cn' = consName con'
            ] ++ concat [
                -- distinguishers
                [
                    "@Override",
                    "public" <+> "boolean" <+> text ("is" ++ cn') <> "()",
                    block [
                         "return" <+> res <> ";"
                         | let res = if cn == cn' then "true" else "false"
                    ]
                ]
                | con' <- cons, let cn' = consName con'
            ]
         ]
    return ()

contents ty@(AType nm ts) con = let
    cn = consName con
    ty' = AType cn (map (const (AVar "?")) ts)
  in
    [
        -- fields
        "private" <+> "final" <+> tipe (unbox ty) <+> ident [nm] <> ";"
        | (nm, ty) <- consArgs con
    ] ++ [
        "",
        -- constructor
        hsep ["public", consProto con],
        block [
             "this" <> "." <> ident [nm] <+> "=" <+> ident [nm] <> ";"
             | (nm, _) <- consArgs con
        ]
    ] ++ concat [
        -- getters
        [
            "public" <+> tipe (unbox ty) <+> ident ["get", nm] <> "()",
            block [
                "return" <+> ident [nm] <> ";"
            ]
        ]
        | (nm, ty) <- consArgs con
     ] ++ [
        -- toString
        "public" <+> "String" <+> "toString" <> "()",
        block [
             "return" <+> string (cn ++ "("),
             nest 4 $ vcat (punctuate (space <> "+" <+> string ", ") [
                 "+" <+> ident [nm]
                 | (nm, _) <- consArgs con
             ]) <+> "+" <+> string ")" <> ";"
        ],
        -- equals
        "public" <+> "boolean" <+> "equals"
            <> "(" <> "Object" <+> "other" <> ")",
        block $ [
            "if" <+> "(" <> "!" <+> "(" <> "other" <+> "instanceof"
                <+> tipe ty' <> ")" <> ")",
            nest 4 $ "return" <+> "false" <> ";",
            tipe ty' <+> ident ["o", nm] <+> "="
                <+> "(" <> tipe ty' <> ")" <+> "other" <> ";"
        ] ++ concat [
           [
               if unboxed ty' then
                   "if" <+> "(" <> ident [nm'] <+> "!=" <+> rhs <> ")"
               else
                   "if" <+> "(" <> "!" <> lhs <> "." <> "equals"
                       <> "(" <> rhs <> ")" <> ")",
               nest 4 $ "return" <+> "false" <> ";"
           ]
           | (nm', ty' ) <- consArgs con,
             let lhs = ident [nm']
                 rhs = ident ["o", nm] <> "." <> ident ["get", nm'] <> "()"
        ] ++ [
           "return" <+> "true" <> ";"
        ]
    ]
