module Grammatik.CF.Eindeutig

( eindeutig
)

where

import Grammatik.Type
import Grammatik.CF.Baum

import Grammatik.Reduziert
import qualified Autolib.Reporter.Checker as C

import Autolib.Reporter
import Autolib.ToDoc

eindeutig :: Int
          -> C.Type Grammatik
eindeutig cut = 
    C.make "Ein" ( text "Die Grammatik soll eindeutig sein." ) $ \ g0 -> do
        let g = reduktion g0
        let dups = ambigs cut g
        when ( not $ null dups ) $ reject $ vcat
             [ text "Diese Grammatik ist nicht eindeutig."
             , text "Die folgenden Wörter haben mehrere Ableitungsbäume"
             , text "(die eventuell noch vorkommenden Variablen sind produktiv,"
             , text "d. h. die Ableitungen können zu Terminalwörtern fortgesetzt werden):"
             , nest 4 $ vcat $ do
                   (b, b') <- take 4 $ dups
                   return $ vcat
                          [ toDoc $ yield b
                          , nest 4 $ vcat $ map toDoc [ b, b' ]
                          ]
             ]
