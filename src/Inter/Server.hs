-- main module

import Inter.Action
import Inter.Crypt
import qualified Inter.Param as P
import Inter.Util
import Inter.Types
import Inter.Collector ( makers )

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
     ]

check_tuple :: IO ( Int, Int )
check_tuple = return ( 4, 5 )

check_list :: IO [ Int ]
check_list = return  [1 .. 7]


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
    case filter ( \ mk -> show mk == toString ( A.typ auf ) )  
             $ Inter.Collector.makers  of
        [ Make n make default_conf ] -> fun $ \ actor -> do
            stud <- alogin auf actor
            ( p, i, icom ) <- make_instant stud make auf
            return i
        _ -> fun ( return "RPC service not available" :: IO String )

antwort auf =
   case filter ( \ mk -> show mk == toString ( A.typ auf ) ) 
             $ Inter.Collector.makers  of
        [ Make n make default_conf ] -> fun $ \ actor b -> do
            stud <- alogin auf actor
            ( p, i, icom :: Text.Html.Html ) <- make_instant stud make auf
            ( res, com :: Text.Html.Html ) <- run $ evaluate' p i b
	    let p = ( mkpar stud auf )  
		     { P.minstant = Just $ icom
		     , P.input = Just $ show b
		     , P.report = Just $ com
		     , P.mresult = res 
		     }
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

         
    
--------------------------------------------------------------------------

tutor_functions = 
     [ ( "list_all", fun list_all )
     , ( "list_pending", fun list_pending )
     , ( "get_student", fun get_student )
--     , ( "get_input", fun get_input )
--     , ( "put_answer", fun put_answer )
     ]


tutor_login act prob = do
    tut <- login act
    vors <- V.get_tutored $ S.snr tut
    let vor = case filter ( \ vor -> V.name vor == fromCGI (vorlesung prob )) 
                  vors of
            [] -> error $ "no vorlesungen for " ++ show (act, prob)
            [vor] -> vor
            vors -> error "more than one vorlesung for you"
    aufs <- A.get $ Just $ V.vnr vor
    let auf = case filter ( \ auf -> A.name auf == fromCGI (aufgabe prob)) 
                  aufs of
            [] -> error $ "no such aufgabe " ++ show (act, prob)
            [auf] -> auf
            aufs -> error "more than one aufgabe"
    return auf
    

-- | all students that have sent in a solution to given problem
list_all :: Actor -> Problem -> IO [ SNr ]
list_all act prob = do
    auf <- tutor_login act prob
    saufs <- SA.get_anr $ A.anr auf
    return $ map SA.snr saufs

-- | all students that have sent in a solution to given problem
-- and whose status is Pending
list_pending :: Actor -> Problem -> IO [ SNr ]
list_pending act prob = do
    auf <- tutor_login act prob
    saufs <- SA.get_anr $ A.anr auf
    return $ map SA.snr 
	   $ filter ( \ sauf -> SA.result sauf == Just Pending )
	   $ saufs
    
get_student :: Actor -> Problem -> SNr -> IO S.Student
get_student act prob snr = do
    tutor_login act prob
    [ stud ] <- S.get_snr snr
    return stud
    
get_input :: Actor -> Problem -> IO String
get_input = undefined

send_answer :: Actor -> Problem -> Answer -> IO ()
send_answer = undefined


