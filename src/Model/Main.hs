import Model.SRS
import qualified Relation

main = mapM_  print
     $ take 1
     $ filter ( (< 3) . length . snd )   
     $ labeled ( thing 5 )
     $ Relation.linear [1 .. 2]

thing n = 
    let w = "b" ++ replicate n 'a' ++ "b"
    in  [ ( "a" ++ w, w ++ w ++ "a" ) ]

