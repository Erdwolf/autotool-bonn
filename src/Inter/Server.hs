-- main module

import Inter.Action
import Inter.Crypt
import qualified Inter.Param as P
import Inter.Util
import Inter.Types

import Challenger.Partial
import Autolib.Reporter
import Text.Html
import Autolib.ToDoc
import Inter.Bank
import Inter.Evaluate

import Control.Types ( VNr, SNr, ANr, Wert (..), fromCGI, toString )
import Control.TH
import Control.Student.TH


import qualified Control.Student.CGI
import qualified Control.Vorlesung as V
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Control.Schule as U
import qualified Control.Stud_Aufg as SA

import Network.XmlRpc.Server
import Network.XmlRpc.Internals

import Control.Monad ( when, guard )



import qualified Collatz.Plain
import qualified Collatz.Inverse
import qualified Faktor.Faktor
import qualified Faktor.Euklid
import qualified Faktor.Inverse


main :: IO ()
main = do
    qas <- qa_functions 
    cgiXmlRpcServer $ listing ++ qas

-----------------------------------------------------------------------------

listing = 
     [ ("vorlesungen", fun vorlesungen)
     , ("aufgaben", fun aufgaben)
     , ("check_tuple", fun check_tuple )
     , ("check_list", fun check_list )
     , ("reverse", fun revs )
     ]

check_tuple :: IO ( Int, Int )
check_tuple = return ( 4, 5 )

check_list :: IO [ Int ]
check_list = return  [1 .. 7]


revs :: [ Integer ] -> IO [ Integer ]
revs xs = return $ reverse xs


vorlesungen :: Actor -> IO [ ( VNr, String ) ]
vorlesungen act = do
    stud <- login act
    vs <- V.get_attended $ S.snr stud
    return $ do v <- vs ; return ( V.vnr v, toString $ V.name v )

aufgaben :: Actor -> VNr -> IO [ ( ANr, String ) ]
aufgaben act v = do
    stud <- login act
    aufs <- A.get_current v
    return $ do auf <- aufs ; return ( A.anr auf, toString $ A.name auf )

--------------------------------------------------------------------------

qa_functions = do
    aufs <- A.get Nothing -- alle Aufgaben
    let qafs = do
        auf <- aufs
        guard $ A.current auf
        let tag = toString $ A.anr auf
        [   ( "frage"   ++ tag, frage   auf )
          , ( "antwort" ++ tag, antwort auf ) 
          ]
    return qafs

-- |  login überhaupt
login :: Actor -> IO S.Student
login act = do
    appendFile "/tmp/Server.log" $ show act 
    [ u ] <- U.gets $ schule act
    [ stud ] <- S.get_unr_mnr 
                 ( U.unr u , fromCGI $ matrikel act )
    when ( not $ Inter.Crypt.compare ( S.passwort stud ) $ passwort act )
	 $ error "password does not match"
    return stud

-- | login für Aufgabe
alogin auf actor = do
     stud <- login actor
     vs <- V.get_attended $ S.snr stud
     when ( not $ A.vnr auf `elem` map V.vnr vs )
          $ error "Sie sind nicht in der Vorlesung, zu der die Aufgabe gehört"
     when ( not $ A.current auf )
          $ error "Aufgabe ist nicht aktuell"
     return stud

frage auf = 
    case filter ( \ mk -> show mk == toString ( A.typ auf ) ) $ makers  of
        [ Make n ( make :: conf -> Var p i b ) default_conf ] 
          -> fun $ \ actor -> do
            stud <- alogin auf actor
            ( p, i, icom ) <- make_instant stud make auf
            appendFile "/tmp/Server.log" $ "F: " ++ show i
            return ( i :: i )
        _ -> fun ( return "RPC service not available" :: IO String )

antwort auf =
   case filter ( \ mk -> show mk == toString ( A.typ auf ) ) $ makers  of
        [ Make n ( make :: conf -> Var p i b ) default_conf ] 
          -> fun $ \ actor ( b :: b ) -> do
            stud <- alogin auf actor
            appendFile "/tmp/Server.log" $ "A:" ++ show b
            ( p, i, icom :: Text.Html.Html ) <- make_instant stud make auf
            ( res, com :: Text.Html.Html ) <- run $ evaluate' p i b
	    let p = ( mkpar stud auf )  
		     { P.minstant = Just $ icom
		     , P.input = Just $ show b
		     , P.report = Just $ com
		     , P.mresult = res 
		     }
            appendFile "/tmp/Server.log" $ "A:" ++ show res
	    msg <- bank p
            return $ res
        _ -> fun ( return "RPC service not available" :: IO String )

make_instant stud fun auf = do
    let conf = read $ toString $ A.config auf
        var = fun  conf
        p = problem var
    let mat = S.mnr stud
    k <- key var $ toString mat 
    g <- gen var (A.vnr auf) (Just $ A.anr auf) k 
    let ( Just i  , _ :: Text.Html.Html ) = export g
    ( _, icom :: Text.Html.Html ) <- run $ report p i
    return ( p, i, icom )

----------------------------------------------------------

makers =
      [ Collatz.Plain.make
      , Collatz.Plain.qmake
      , Collatz.Inverse.make
      , Collatz.Inverse.qmake
      , Faktor.Faktor.make_fixed
      , Faktor.Faktor.make_quiz
      , Faktor.Euklid.make_fixed
      , Faktor.Euklid.make_quiz
      , Faktor.Inverse.make_fixed
      , Faktor.Inverse.make_quiz
      ]
         
