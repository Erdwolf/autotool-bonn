module Wash.Config where

-- temporary storage
tmpDir = "/tmp/"
-- persistent, mutable storage
varDir = "/tmp/"

imageDir 	= tmpDir ++ "Images/"
emailTmpDir 	= tmpDir
persistentDir 	= varDir ++ "Persistent/"
persistent2Dir 	= varDir ++ "Persistent2/"
registryDir	= tmpDir ++ "REGISTRY/"
keyFile		= varDir ++ "KEYFILE"

-- path to PBMplus programs
pbmPath		= "/usr/X11R6/bin/"
-- path of cat program
catProgram	= "/bin/cat"
-- path of sendmail program
sendmailProgram	= "/usr/sbin/sendmail"
