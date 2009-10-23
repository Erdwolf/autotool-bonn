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
    text "package" <+> text package <> text ";",
    text "import" <+> text "java.util.List" <> text ";",
    text "",
    text "@SuppressWarnings" <> text "(" <> string "unused" <> text ")"
 ]

jData :: AData -> IO ()
jData (AData ty@(AType nm _) [con]) | nm == consName con = do
    vWriteFile (dir </> nm <.> "java") $ show $ vcat $ header ++ [
        text "public" <+> text "class" <+> tipe ty,
        block $ contents ty con
     ]
jData (AData ty@(AType nm tv) cons) = do
    vWriteFile (dir </> nm <.> "java") $ show $ vcat $ header ++ [
        text "public" <+> text "interface" <+> tipe ty,
        block $ [
             -- getters
             text "public" <+> text cn <+> text ("get" ++ cn) <> text "()" <> text ";"
             | con <- cons, let cn = consName con
        ] ++ [
             -- distinguishers
             text "public" <+> text "boolean" <+> text ("is" ++ cn) <> text "()" <> text ";"
             | con <- cons, let cn = consName con
        ]
     ]
    forM_ cons $ \con -> do
        let cn = consName con
        vWriteFile (dir </> cn <.> "java") $ show $ vcat $ header ++ [
            text "public" <+> text "class" <+> text cn <+> text "implements" <+> tipe ty,
            block $ contents ty con ++ [
            ] ++ concat [
                -- getters
                [
                    text "@Override",
                    text "public" <+> text cn' <+> text ("get" ++ cn') <> text "()",
                    block [
                         text "return" <+> (if cn == cn' then text "this" else text "null") <> text ";"
                    ]
                ]
                | con' <- cons, let cn' = consName con'
            ] ++ concat [
                -- distinguishers
                [
                    text "@Override",
                    text "public" <+> text "boolean" <+> text ("is" ++ cn') <> text "()",
                    block [
                         text "return" <+> (if cn == cn' then text "true" else text "false") <> text ";"
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
        text "private" <+> text "final" <+> tipe (unbox ty) <+> ident [nm] <> text ";"
        | (nm, ty) <- consArgs con
    ] ++ [
        text "",
        -- constructor
        hsep [text "public", consProto con],
        block [
             text "this" <> text "." <> ident [nm] <+> text "=" <+> ident [nm] <> text ";"
             | (nm, _) <- consArgs con
        ]
    ] ++ concat [
        -- getters
        [
            text "public" <+> tipe (unbox ty) <+> ident ["get", nm] <> text "()",
            block [
                text "return" <+> ident [nm] <> text ";"
            ]
        ]
        | (nm, ty) <- consArgs con
     ] ++ [
        -- toString
        text "",
        text "public" <+> text "String" <+> text "toString" <> text "()",
        block [
             text "return" <+>
                  string (cn ++ "("),
             nest 4 $ vcat $ [
                 text "+" <+> ident [nm] <+> text "+" <+> cm
                 | (nm, _) <- consArgs con,
                 let cm | nm == fst (last (consArgs con)) = string ")" <> text ";"
                        | otherwise = string ", "
             ]
        ],
        -- equals
        text "",
        text "public" <+> text "boolean" <+> text "equals" <> text "(" <> text "Object" <+> text "other" <> text ")",
        block $ [
            text "if" <+> text "(" <> text "!" <+> text "(" <> text "other" <+> text "instanceof" <+> tipe ty' <> text ")" <> text ")",
            nest 4 $ text "return" <+> text "false" <> text ";",
            tipe ty' <+> ident ["o", nm] <+> text "=" <+> text "(" <> tipe ty' <> text ")" <+> text "other" <> text ";"
        ] ++ concat [
           [
               if unboxed ty' then
                   text "if" <+> text "(" <> ident [nm'] <+> text "!=" <+> ident ["o", nm] <> text "." <> ident ["get", nm'] <> text "()" <> text ")"
               else 
                   text "if" <+> text "(" <> text "!" <> ident [nm'] <> text "." <> text "equals" <> text "(" <> ident ["o", nm] <> text "." <> ident ["get", nm'] <> text "()" <> text ")" <> text ")",
               nest 4 $ text "return" <+> text "false" <> text ";"
           ]
           | (nm', ty' ) <- consArgs con
        ] ++ [
           text "return" <+> text "true" <> text ";"
        ]
    ]
