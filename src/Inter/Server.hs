-- main module

import Inter.Action
import Inter.TH
import Inter.Crypt

import Control.Types ( VNr, SNr, ANr, Wert (..), fromCGI )
import Control.TH
import Control.Student.TH


import qualified Control.Student.CGI
import qualified Control.Vorlesung as V
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Control.Student.DB
import qualified Control.Stud_Aufg as SA
import qualified Control.Stud_Aufg.DB

import Network.XmlRpc.Server
import Control.Monad ( when )

main :: IO ()
main = cgiXmlRpcServer 
     [ ( "list_all", fun list_all )
     , ( "list_pending", fun list_pending )
     , ( "get_student", fun get_student )
--     , ( "get_input", fun get_input )
--     , ( "put_answer", fun put_answer )
     ]

-- | check that actor is tutor for given lecture/problem
-- if not, throw exception
login :: Actor -> Problem -> IO A.Aufgabe
login act prob = do
    [ tut ] <- Control.Student.DB.get_mnr $ fromCGI $ matrikel act
    when ( not $ Inter.Crypt.compare ( S.passwort tut ) $ passwort act )
	 $ error "password does not match"
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
    auf <- login act prob
    saufs <- Control.Stud_Aufg.DB.get_anr $ A.anr auf
    return $ map SA.snr saufs

-- | all students that have sent in a solution to given problem
-- and whose status is Pending
list_pending :: Actor -> Problem -> IO [ SNr ]
list_pending act prob = do
    auf <- login act prob
    saufs <- Control.Stud_Aufg.DB.get_anr $ A.anr auf
    return $ map SA.snr 
	   $ filter ( \ sauf -> SA.result sauf == Just Pending )
	   $ saufs
    
get_student :: Actor -> Problem -> SNr -> IO S.Student
get_student act prob snr = do
    login act prob
    [ stud ] <- S.get_snr snr
    return stud
    
get_input :: Actor -> Problem -> IO String
get_input = undefined

send_answer :: Actor -> Problem -> Answer -> IO ()
send_answer = undefined


