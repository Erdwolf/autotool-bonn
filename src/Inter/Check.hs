-- this is a main module

import qualified SAT.SAT
import qualified TM.Steps

import Inter.Types
import Inter.Variant

variants :: [ Variant ]
variants = [ Variant SAT.SAT.var ]

main = do
     steps <- TM.Steps.variantsIO 
     interface $ steps ++ variants
