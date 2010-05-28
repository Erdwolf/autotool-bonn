{-# LANGUAGE OverloadedStrings #-}

-- Turn data types into equivalent Java classes.
--
-- easy case:
--     data Foo a b = Foo { frotz :: a, xyzzy :: b }
--
-- becomes
-- class Foo<A, B> {
--     private A frotz;
--     private B xyzzy;
--     Baz(A frotz, B xyzzy) { this.frotz = frotz; this.xyzzy = xyzzy; }
--     A getFrotz() { return frotz; }
--     B getFrotz() { return xyzzy; }
--     // equals, hashCode
-- }

--
-- complex case:
--     data Foo a b = Bar | Baz { frotz :: a, xyzzy :: b }
--
-- is turned into
--
-- interface Foo<A, B> {
--     Bar<A, B> getBar();
--     Baz<A, B> getBaz();
--     boolean isBar();
--     boolean isBaz();
-- }
--
-- class Bar<A, B> implements Foo<A, B> {
--     Bar() { }
--     Bar<A, B> getBar() { return this; }
--     Baz<A, B> getBaz() { return null; }
--     boolean isBar() { return true; }
--     boolean isBaz() { return false; }
-- }
--
-- class Baz<A, B> implements Foo<A, B> {
--     private A frotz;
--     private B xyzzy;
--     Baz(A frotz, B xyzzy) { this.frotz = frotz; this.xyzzy = xyzzy; }
--     A getFrotz() { return frotz; }
--     B getFrotz() { return xyzzy; }
--     ...
-- }
--
-- Anonymous fields (as in data Foo = Foo Int Int) will be called
-- fiedl1, field2 etc.

module JData (
    dir,
    jData
) where

import Types
import Basic
import Java
import Package

import System.FilePath
import Control.Monad
import Text.PrettyPrint.HughesPJ
import Data.Int

package :: String
package = base ++ ".types"

dir :: FilePath
dir = "out" </> "types"

-- file header
header = [
    "package" <+> text package <> ";",
    "import" <+> "java.util.List" <> ";",
    "",
    "@SuppressWarnings" <> "(" <> string "unused" <> ")"
 ]

-- actual worker
jData :: AData -> IO ()
-- simple case: one alternative
jData (AData ty@(AType nm _) [con]) | nm == consName con = do
    vWriteFile (dir </> nm <.> "java") $ show $ vcat $ header ++ [
        "public" <+> "class" <+> tipe ty,
        block $ contents ty con
     ]
-- complex case: several alternatives
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

-- actual class contents
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
        ],
        -- hashCode
        "public" <+> "int" <+> "hashCode" <> "()",
        block $ [
            "return",
            if null (consArgs con) then nest 4 $ text "0;" else
            nest 4 $ (vcat $ punctuate " +" $ [
                boxFunc ty' (ident [nm']) <> "." <> "hashCode" <> "()"
                    <+> "*" <+> text (show i)
                | ((nm', ty'), i) <- zip (consArgs con) (iterate (*37) (1 :: Int32))
            ]) <> ";"
        ]
    ]
