module Syntax.LaTeX where

import Text.PrettyPrint.HughesPJ

import System.FilePath (replaceExtension,(</>),(<.>))
import System.Directory (removeFile, getTemporaryDirectory, doesFileExist)
import System.IO (openTempFile, hClose, hPutStr, getContents)
import System.IO.Unsafe (unsafePerformIO)
import System (system)
import Control.Monad (unless)
import Data.Hashable (Hashable(hash), combine)
import Text.XHtml (showHtml, Html, image, (!), src, alt, title, anchor, href)

import Prolog.Programming.Hex (hex)
import Syntax.Syntax
import Syntax.Printer (ascii)

instance Hashable Graph where
   hash graph =
      case graph of
         Chain g1 g2 -> hash "Chain"    `combine` hash g1 `combine` hash g2
         Fork  g1 g2 -> hash "Fork"     `combine` hash g1 `combine` hash g2
         Loop  g     -> hash "Loop"     `combine` hash g
         Terminal t  -> hash "Terminal" `combine` hash t
         Symbol s    -> hash "Symbol"   `combine` hash s
         Empty       -> hash "Empty"


picsDir = ".."</>"pics"

asImage :: [(String,Graph)] -> String
asImage lang = unsafePerformIO $ do
   let name = hex $ fromIntegral $ hash lang
   let imageFile = picsDir</>name<.>"png"

   alreadyCached <- doesFileExist imageFile
   unless alreadyCached $ do

      tmp_dir <- getTemporaryDirectory
      (tempPath, tempHandle) <- openTempFile tmp_dir (name<.>"tex")
      let pathTo ext = replaceExtension tempPath ext

      hPutStr tempHandle $ render $ latexFull lang
      hClose tempHandle

      system $ "latex -interaction=batchmode --output-dir=\"" ++ tmp_dir ++ "\" " ++ pathTo "tex" ++ " > /dev/null"
      removeFile $ pathTo "tex"
      removeFile $ pathTo "aux"
      removeFile $ pathTo "log"

      system $ "dvipng -T tight -z 9 -bg transparent -o " ++ imageFile ++ " " ++ pathTo "dvi" ++ " > /dev/null"
      removeFile $ pathTo "dvi"

   return $ showHtml $ image ! [ src ("../pics/" ++ name ++ ".png"), title "Railroad-Diagramm der Sprache", alt $ unlines $ concat [ a : ascii g | (a,g) <- lang ] ]


{-
latexFull :: [(String,Graph)] -> Doc 
latexFull lang =
   text "\\documentclass{article}" $$
   text "\\usepackage{rail}" $$
   text "\\begin{document}" $$
   text "\\begin{rail}" $$
   vcat [ text symb <+> latex graph <+> text ";"
        | (symb, graph) <- lang
        ] $$
   text "\\end{rail}" $$
   text "\\end{document}"

latex :: Graph -> Doc
latex (Chain g1 g2) = latex g1 <+> latex g2
latex (Fork g1 g2) = text "(" <+> latex g1 <+> text "|" <+> latex g2 <+> text ")"
latex (Terminal t) = text "'" <> text t <> text "'"
latex (Symbol s) = text s
latex Empty = empty
-}


latexFull :: [(String,Graph)] -> Doc
latexFull lang =
   text "\\documentclass{article}" $$
   text "\\usepackage{rail}" $$
   text "\\begin{document}" $$
   text "\\pagestyle{empty}" $$
   text "\\makeatletter" $$
   text "\\rail@p {acith}" $$
   text "\\rail@o {1}{" $$
   vcat [ text ("\\rail@begin{" ++ show (height graph) ++ "}{" ++ symb ++ "}") $$
          latex 0 graph $$
          text "\\rail@vend"
        | (symb, graph) <- lang
        ] $$
   text "}" $$
   text "\\csname rail@o@\\number1\\endcsname" $$
   text "\\end{document}"

latex :: Int -> Graph -> Doc
latex h (Chain g1 g2) = latex h g1 $$ latex h g2
latex h (Fork g1 g2) = text "\\rail@bar" $$
                       latex h g1 $$
                       let h' = h + height g1 in
                       text ("\\rail@nextbar{" ++ show h' ++ "}") $$
                       latex h' g2 $$
                       text "\\rail@endbar"
latex h (Loop g) = text "\\rail@plus" $$
                   latex h g $$
                   let h' = h + height g in
                   text ("\\rail@nextplus{" ++ show h' ++ " }") $$
                   text "\\rail@endplus"
latex h (Terminal t) = text ("\\rail@rterm{" ++ t ++ "}[]")
latex h (Symbol s) = text ("\\rail@rnont{" ++ s ++ "}[]")
latex h Empty = empty


height :: Graph -> Int
height (Chain g1 g2) = height g1 `max` height g2
height (Fork g1 g2) = height g1 + height g2
height (Loop g) = height g + 1
height (Terminal t) = 1
height (Symbol s) = 1
height Empty = 1
