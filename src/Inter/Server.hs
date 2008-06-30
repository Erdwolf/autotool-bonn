{-# language TemplateHaskell, CPP, OverlappingInstances, IncoherentInstances #-}

-- main module

import Inter.Action
import Inter.Crypt

import Inter.Collector
import Inter.Common
import Inter.Types
import Inter.Evaluate

import Control.Types (toString)
import Challenger.Partial

import qualified Autolib.Multilingual 

import qualified Inter.Param as P
import qualified Util.Datei as D
import qualified Control.Punkt
import qualified Inter.Store

import Control.Types ( VNr, SNr, ANr, Wert (..), fromCGI, HiLo(..), TimeStatus(..) )
import Control.TH
import Control.Student.TH
import Autolib.Reporter
import Autolib.ToDoc

import qualified Control.Student.CGI
import qualified Control.Vorlesung as V
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Control.Schule as U
import qualified Control.Student.DB
import qualified Control.Stud_Aufg as SA
import qualified Control.Stud_Aufg.DB

import Network.XmlRpc.Server
import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType
import Control.Monad ( when )
import Control.Monad.Error

import Data.List ( intersperse )
import Data.Typeable
import System.Random
import System.Time

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
     [ ( "get_current_questions_for_type", fun get_current_questions_for_type )
     , ( "get_question", fun get_question )
     , ( "get_random_question", fun get_question )
     , ( "put_answer", fun put_answer )
     ]

-- | student login
login :: Actor -> Problem -> IO (V.Vorlesung, S.Student, A.Aufgabe)
login act prob = do
    ( vor, stud, aufs ) <- login0 act $ vorlesung prob
    appendFile "/tmp/RPC.log" $ show prob
    let auf = case filter ( \ auf -> A.name auf == fromCGI (aufgabe prob)) 
                          aufs of
           [] -> error $ "no such aufgabe " ++ show (act, prob)
	   [auf] -> auf
	   aufs -> error "more than one aufgabe"
    return (vor, stud, auf)

login0 :: Actor -> String -> IO (V.Vorlesung, S.Student, [A.Aufgabe])
login0 act vorles = do
    appendFile "/tmp/RPC.log" $ show $ act { passwort = "..." }

    us <- U.get
    u <- case [ u | u <- us , U.name u == fromCGI ( schule act ) ] of
        [u] -> return u
	[]  -> error "diese Schule gibt es nicht"
	_   -> error "mehr als eine solche Schule"
    [ stud ] <- Control.Student.DB.get_unr_mnr ( U.unr u , fromCGI $ matrikel act )
    when ( not $ Inter.Crypt.compare ( S.passwort stud ) $ passwort act )
	 $ error "password does not match"
    vors <- V.get_attended $ S.snr stud
    let vor = case filter ( \ vor -> V.name vor == fromCGI vorles ) 
                          vors of
           [] -> error $ "no vorlesungen for " ++ show (act, vorles)
	   [vor] -> vor
	   vors -> error "more than one vorlesung for you"
    aufs <- A.get $ Just $ V.vnr vor
    return ( vor, stud, aufs )
    
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

get_current_questions_for_type :: Actor -> String -> String -> IO [ String ]
get_current_questions_for_type act vorles typ = do
    (vor, stud, aufs) <- login0 act vorles
    let selected = filter ( \ auf ->  typ == toString ( A.typ auf ) ) 
		 $ filter ( \ auf ->  Current == A.timeStatus auf )
		 $ aufs
    return $ map ( \ auf -> toString $ A.name auf
		 ) selected

get_question :: Actor -> Problem -> IO Value
get_question act prob = do
    get_question_with act prob  ( toString . S.mnr )

get_random_question :: Actor -> Problem -> IO Value
get_random_question act prob = do
    ct <- System.Time.getClockTime
    get_question_with act prob  ( const $ show ct )

get_question_with :: Actor -> Problem -> ( S.Student -> String ) -> IO Value
get_question_with act prob seed = do
    (vor, stud, auf) <- login act prob
    -- FIXME: duplicated code from Inter.Super follows
    let mmk = lookup ( toString $ A.typ auf )
            $ do mk <- Inter.Collector.makers ; return ( show mk, mk )
    case mmk of
            Nothing -> do
                error "Aufgabenstellung nicht auffindbar"
            Just ( Make p doc fun veri ex ) -> do
                ( _, i, com ) <- make_instant_common_with
                    (V.vnr vor) ( Just $ A.anr auf ) stud 
			   ( fun $ read $ toString $ A.config auf )
			   ( seed stud )
                let p = mkpar stud auf
                    d = Inter.Store.location Inter.Store.Instant
                           p "latest" False
                file <- D.schreiben d $ show $ outform com
{-
                let inst = fromCGI file
                Control.Punkt.bepunkteStudentDB
                         (P.ident p) (P.anr p)
                         ( Just inst )
                         Nothing (P.highscore p)
                         Nothing
                         Nothing
-}
                return $ toValue i

put_answer :: Actor -> Problem -> Value -> IO String
put_answer act prob val = do
    (vor, stud, auf) <- login act prob
    -- FIXME: duplicated code from Inter.Super follows
    let mmk = lookup ( toString $ A.typ auf )
            $ do mk <- Inter.Collector.makers ; return ( show mk, mk )
    case mmk of
            Nothing -> do
                error "Aufgabenstellung nicht auffindbar"
            Just ( Make p doc fun veri ex ) -> do
                ( p, i, icom ) <- make_instant_common
                    (V.vnr vor) ( Just $ A.anr auf ) stud 
			   ( fun $ read $ toString $ A.config auf )
                appendFile "/tmp/RPC.log" $ "argument (external): " ++ show val
                mobj <- runErrorT $ fromValue val 
		let obj = case mobj of
		        Left msg -> error $ "parse error, msg: " ++ msg
		        Right obj -> obj
                let ans = show $ asTypeOf obj ( Challenger.Partial.initial p i ) 
                appendFile "/tmp/RPC.log" $ "argument (internal): " ++ show ans

		( res, com2 ) <- run $ evaluate p i ans
                appendFile "/tmp/RPC.log"
                       $ unwords 
                       $ intersperse ":"
                                 [ schule act, matrikel act
                                 , vorlesung prob, aufgabe prob
                                 , show res 
                                 ]
                Inter.Common.pure_punkte False stud auf
                     ( Just $ outform icom, Just ans, res, Just $ outform com2 )
		return $ show res
                       
outform = Autolib.Multilingual.specialize Autolib.Multilingual.UK



