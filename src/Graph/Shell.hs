{-# OPTIONS -fallow-incoherent-instances -fglasgow-exts #-}

module Main where

import Graph.Op
import Expression.Op
import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Graph
import Gateway.CGI
import Inter.Evaluate
import Autolib.ToDoc
import Autolib.Reporter
import Text.XHtml ( Html )

main :: IO ()
main = Gateway.CGI.execute "Graph.cgi" $ Gateway.CGI.wrap $ do
    open table
    input <- defaulted_textarea "Graph expression" $ show Graph.Op.example
    -- open row ; submit "submit" ; close -- row
    lout <- click_choice "Layouter" $ do
            lout <- [  Dot , Neato , Twopi , Circo , Fdp ]
            return ( show lout, lout )
    close -- table
    ( res, com :: Html ) <- io $ run $ handler input lout
    html com

handler input lout = do
    exp :: Exp Graph_Or_Natural <- parse_or_complain input
    Gra g <- return $ eval0 exp
    inform $ toDoc g
    peng $ g { layout_program = lout
             , layout_hints = [ "-Nheight=0.1", "-Nwidth=0.1"
                              , "-Nfixedsize=true"
                              , "-Gsize=5,5"
                              ]
             }

