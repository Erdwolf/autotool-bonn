module Multi.Generator where

-- -- $Id$

import Multi.Paths

import Util.Datei
import Dot.Dot
import Informed
import ToDoc (render)
import Random

generator :: ( ToDot a, Informed a )
	  => Datei -> [a] 
	  -> IO ()
-- prefix ist z. b. [ "multi", "graph" ]
-- pngs stehen dann unter [ "public_html", "multi", "graph" ] foo.png
-- indexfile unter [ "data", "multi", "graph" ] index.text
generator prefix xs = do
    fis <- mapM ( \ x -> do
	        n <- randomRIO (1, 100000 :: Int)
		let p = public_html 
		      $ prefix { name = show n } -- ohne extension
		f <- deng p x -- f hat extension
		return ( name f -- ohne dir, mit extension
		       , unwords $ words $ render $ info x 
		       )
	   ) xs
    let d = data_index prefix
    schreiben d $ unlines $ do
        ( f, i ) <- fis
	return $ unwords [ f, i ]

    
