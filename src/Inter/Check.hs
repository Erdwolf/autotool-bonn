-- this is a main module

import qualified SAT.SAT
import Inter.Types
import Inter.Variant

variants :: [ Variant ]
variants = [ Variant SAT.SAT.var ]

main = interface variants
