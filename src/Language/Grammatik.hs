module Language.Grammatik where

import Language.Type

import Grammatik.Type

import qualified Grammatik.CF.Chomsky as C
import qualified Grammatik.CF.Chomsky.Generate as G
import qualified Grammatik.CF.DPL_CYK as D


import Autolib.Util.Zufall
import Autolib.Util.Edit

import Autolib.ToDoc

import Data.List

grammatik :: Grammatik -> Language
grammatik g =
    let ch = C.make g
        ws = G.main ch
        l = Language
            { nametag = "Grammatik"
            , abbreviation = show $ vcat
                  [ text "die durch"
                  , nest 4 $ toDoc g
                  , text "erzeugte Sprache"
                  ]
            , alphabet = terminale g
            , contains = D.accepted ch
            , sample = \ num len -> 
                pick num $ do
                     k <- take num [ len .. ]
                     ws !! k
            , anti_sample = \ num len -> do
	        positive <- sample l num len
                css <- sequence $ do
                    w <- positive
                    return $ mapM edits $ replicate 5 w
                return $ filter ( not . contains l ) $ nub $ concat css
            }
    in l

pick num xs = do
    let f k [] = return []
        f k (y : ys) = do
            i <- randomRIO ( 0, k )
            zs <- f (k+1) $ drop i ys
            return $ y : zs
    f 1 $ concat $ repeat xs

          
