-- main module

import Inter.Action
import Inter.Crypt

import Inter.Collector
import Inter.Common
import Inter.Types

import Control.Types (toString)
import Challenger.Partial

import qualified Inter.Param as P
import qualified Util.Datei as D
import qualified Control.Punkt
import qualified Inter.Store

import Control.Types ( VNr, SNr, ANr, Wert (..), fromCGI )
import Control.TH
import Control.Student.TH


import qualified Control.Student.CGI
import qualified Control.Vorlesung as V
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Control.Schule as U
import qualified Control.Student.DB
import qualified Control.Stud_Aufg as SA
import qualified Control.Stud_Aufg.DB

import Network.XmlRpc.Server
import Control.Monad ( when )

main :: IO ()
main = cgiXmlRpcServer 
     $ {- for_tutor ++ -} for_student

#if(0)
for_tutor = 
     [ ( "list_all", fun list_all )
     , ( "list_pending", fun list_pending )
     , ( "get_student", fun get_student )
     ]
#endif

for_student = 
     [ ( "get_question", fun get_question )
     , ( "put_answer", fun put_answer )
     ]

-- | student login
login :: Actor -> Problem -> IO (V.Vorlesung, S.Student, A.Aufgabe)
login act prob = do
    us <- U.get
    u <- case [ u | u <- us , U.name u == fromCGI ( schule act ) ] of
        [u] -> return u
	[]  -> error "diese Schule gibt es nicht"
	_   -> error "mehr als eine solche Schule"
    [ stud ] <- Control.Student.DB.get_unr_mnr ( U.unr u , fromCGI $ matrikel act )
    when ( not $ Inter.Crypt.compare ( S.passwort stud ) $ passwort act )
	 $ error "password does not match"
    vors <- V.get_attended $ S.snr stud
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
    return (vor, stud, auf)
    
#if(0)
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
#endif

----------------------------------------------------------------------------------
    
get_question :: Actor -> Problem -> IO String
get_question act prob = do
    (vor, stud, auf) <- login act prob
    -- FIXME: duplicated code from Inter.Super follows
    let mmk = lookup ( toString $ A.typ auf )
            $ do mk <- Inter.Collector.makers ; return ( show mk, mk )
    case mmk of
            Nothing -> do
                error "Aufgabenstellung nicht auffindbar"
            Just ( Make doc fun veri ex ) -> do
                ( _, i, com ) <- make_instant_common
                    (V.vnr vor) ( Just $ A.anr auf ) stud 
			   ( fun $ read $ toString $ A.config auf )
                let p = mkpar stud auf
                    d = Inter.Store.location Inter.Store.Instant
                           p "latest" False
                file <- D.schreiben d $ show com
                let inst = fromCGI file
                Control.Punkt.bepunkteStudentDB
                         (P.ident p) (P.anr p)
                         ( Just inst )
                         Nothing (P.highscore p)
                         Nothing
                         Nothing
                return $ show i

put_answer :: Actor -> Problem -> Answer -> IO Bool
put_answer act prob ans = do
    return False



