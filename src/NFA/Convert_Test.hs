module NFA.Convert_Test where

import Convert.Type
import NFA.Convert
import Autolib.NFA
import NFA.Property
import Autolib.Set
import Autolib.ToDoc

import Autolib.NFA.Shortest

import qualified Challenger as C
import Autolib.Reporter

inst :: ( Convert, [ Property Char ] )
inst =( Convert { name = Just [ "{ w | \"ababcd\" ist Suffix von w}" ]
          , input = read "Exp All ababcd"
          }
      , [ Max_Size 12 , Alphabet (mkSet "abcd") , Deterministic ]
      )

aut :: NFA Char Int
aut = NFA { alphabet = mkSet "abcd" , states = mkSet [ 1 , 2 , 3,4,5,6,7 ]
    , starts = mkSet [ 1 ] , finals = mkSet [ 7 ]
    , trans = collect [ ( 1 , 'b' , 1 ) , ( 1 , 'c' , 1 )
                      , ( 1 , 'd' , 1 ) , ( 1 , 'a' , 2 )
                      , ( 2 , 'b' , 3 ) , ( 3 , 'a' , 4 ) , ( 4 , 'b' , 5 )
                      , ( 5 , 'c' , 6 ) , ( 6 , 'd' , 7 ) , ( 2 , 'a' , 1 )
                      , ( 2 , 'c' , 1 ) , ( 2 , 'd' , 1 ) , ( 3 , 'b' , 1 )
                      , ( 3 , 'c' , 1 ) , ( 3 , 'd' , 1 ) , ( 4 , 'a' , 1 )
                      , ( 4 , 'c' , 1 ) , ( 4 , 'd' , 1 ) , ( 5 , 'a' , 1 )
                      , ( 5 , 'b' , 1 ) , ( 5 , 'd' , 1 ) , ( 6 , 'a' , 1 )
                      , ( 6 , 'b' , 1 ) , ( 6 , 'c' , 1 )
                      ]
    }

test :: ( Maybe () , Doc )
test = export $ C.total Convert_To_NFA inst aut
