module Wash.Email (sendmail, inventMessageId, module Wash.MIME) where

import Wash.Auxiliary
import IO
import Wash.MIME
import System
import Wash.Unique

import Wash.Config

-- facilities for sending email

sendmailFlags   = ["-i", "-t", "-U"]			    -- , "-v" for verbose mode

sendmail :: Mail -> IO ExitCode
sendmail mail =
  do filename <- inventBoundary
     let tempfilename  = emailTmpDir ++ filename
         tempfilename2 = emailTmpDir ++ "T" ++ filename
     h <- openFile tempfilename WriteMode
     hSend h mail
     hClose h
     system (sendmailProgram ++ pFlags sendmailFlags ++ " < " ++ tempfilename ++ " > " ++ tempfilename2)
     system ("rm " ++ tempfilename)
     system ("rm " ++ tempfilename2)

pFlags [] = ""
pFlags (flag:flags) = ' ' : flag ++ pFlags flags

inventMessageId :: IO Header
inventMessageId =
  do randomKey <- inventStdKey
     hostname  <- protectedGetEnv "SERVER_NAME" "localhost"
     let messageId = "<" ++ randomKey ++ ".Email@" ++ hostname ++ ">"
     return (Header ("Message-Id", messageId))

