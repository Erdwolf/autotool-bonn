module Control.Admin where

-- ================================================================================
-- ADMIN


-- | 
-- Login des Admins
-- 
-- Input:   Name, Passwort
-- Output:  IO True\/False
--
checkAdminNamePasswortDB :: String -> String -> IO Bool
checkAdminNamePasswortDB nme pas = do
       conn <- myconnect
       stat <- query conn
               ( concat
                 [ "SELECT admin.Name AS Name \n"
                 , "FROM    admin \n"
                 , "WHERE   \n"
                 , "admin.Name = \"" ++ (quoteQuots nme)++ "\" "
                 , "AND admin.Passwort = \"" ++ (quoteQuots pas)++ "\" "
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                    b <- getFieldValue state "Name"
                    return (b :: String)
            ) stat
       disconnect conn
       let loginok = length inh > 0
       return loginok


failDB nme pas = do
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "SELECT admin.Name AS Name \n"
                 , "FROM    admin \n"
                 , "WHERE   \n"
                 , "admin.Name = \"" ++ (quoteQuots nme)++ "\" "
                 , "AND admin.Passwort = \"" ++ (quoteQuots pas)++ "\" "
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            b <- getFieldValue state "Name"
                            return (b :: String)
                          ) stat
       disconnect conn
       let loginok = length inh > 0
       return loginok


-- |
-- Liefert Studenten-Daten die auf Vorname,Name,Matrikel,Email und Vorlesung passen.
--
-- > Hinweis: Und-Verknuepfung, wobei leere (="") Parameter ignoriert werden.
-- > Input: Vorname,Name,Matrikel,Email,Vorlesungsname
-- > Output: IO [ [ SNr, MNr, Vorname , Email, Status ] ]
--
-- TODO optinale Teilwort suche (%part%)
-- TODO optinale or Verknüpfung?
--
findStudDB vnm nme mat eml vrl= do
       conn <- myconnect
       let 
            ifexist_pre pre x sqlnme =
                if length x > 0 
                then pre ++ sqlnme ++ " LIKE \"%" ++ (quoteQuots x)++ "%\" "
                else ""
            ifexist0 = ifexist_pre ""
            ifexist = ifexist_pre "AND "

       stat <- query conn 
               ( concat 
                 [ "SELECT DISTINCT \n"
                 , "student.SNr     AS SNr , \n"
                 , "student.MNr     AS MNr, \n"
                 , "student.Vorname AS Vorname, \n"
                 , "student.Name    AS Name, \n"
                 , "student.Email   AS Email, \n"  
                 , "student.Status  AS Status \n"
		 --, "FROM    student , vorlesung , stud_vorl \n"
                 , "FROM    student \n"
                 , "WHERE "
--                 , "student.SNr = stud_vorl.SNr " 
--                 , "AND stud_vorl.VNr = vorlesung.VNr " 
                 , ifexist0 mat "student.MNr" 
                 , ifexist vnm "student.Vorname"
                 , ifexist nme "student.Name"
                 , ifexist eml "student.Email"
                 , ifexist vrl "vorlesung.Name"  
                 , "ORDER BY MNr "  
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            s <- getFieldValue state "SNr"
                            m <- getFieldValue state "MNr"
                            a <- getFieldValue state "Vorname"
                            b <- getFieldValue state "Name"
                            c <- getFieldValue state "Email"
                            d <- getFieldValue state "Status"
                            return [ I s, S m , S a,  S b , S c , S d ]
                          ) stat
       disconnect conn 
       return (  showColTypes stat, inh )

-- |
-- Modifiziert vom Student Matrikel,Vorname,Name,Email,Passwort
--
-- Input:   AlteMatrikelnr., NeueMatrikelnr., Vorname,Name,Email,Passwort
-- Output:  IO ()
--
updateStudDB oldmat mat vnm nme eml pas = do
   cpas <- Inter.Crypt.encrypt pas

   conn <- myconnect
   stat <- query conn
           ( concat 
             [ "UPDATE student \n"
             , "SET \n"
             , "student.MNr = \""       ++ quoteQuots mat ++ "\", " 
             , "student.Vorname = \""   ++ quoteQuots vnm ++ "\", " 
             , "student.Name = \""      ++ quoteQuots nme ++ "\", "
             , "student.Email = \""     ++ quoteQuots eml ++ "\", "
             , "student.Passwort = \""  ++ show cpas ++ "\" "
             , "WHERE student.MNr = \"" ++ quoteQuots oldmat ++ "\" "
             ] )
   disconnect conn 
   return ()

-- |
-- Liefert vom Studenten: Vorname,Name,Email,Status,Passwort
--
-- Input: Matrikelnr.
-- Output: IO [( Vorname, Name , Email, Status, Passwort )]
--
getStudentDB mnr = do 
    conn <- myconnect
    stat <- query conn 
            ( concat 
              [ "SELECT student.MNr AS MNr, \n"
              , "student.Vorname AS Vorname, \n"
              , "student.Name AS Name, \n"
              , "student.Email AS Email, \n"  
              , "student.Status AS Status, \n"
              , "student.Passwort AS Passwort \n"
              , "FROM   student \n"
              , "WHERE  student.MNr = \"" ++ (filterQuots mnr)++ "\" "
              , ";"
              ] )
    inh <- collectRows ( \ state -> do
                         a <- getFieldValue state "Vorname"
                         b <- getFieldValue state "Name"
                         c <- getFieldValue state "Email"
                         d <- getFieldValue state "Status"
                         e <- getFieldValue state "Passwort"
                         return (  S a,  S b , S c , S d , S e )
                       ) stat
    disconnect conn 
    return inh


-- |
-- Liefert Email und Passwort von Matrikelnr
--
-- Input:   Matrikelnr
-- Output:  (Email,Passwort) 
--
getEmailPasswortDB :: String -> IO [(String, String)]
getEmailPasswortDB mnr = do 
    conn <- myconnect
    stat <- query conn 
            ( concat 
              [ "SELECT "
              , "student.Email AS Email, \n"  
              , "student.Passwort AS Passwort \n"
              , "FROM   student \n"
              , "WHERE  student.MNr = \"" ++ (filterQuots mnr)++ "\" "
              , ";"
              ] )
    inh <- collectRows ( \ state -> do
                    e <- getFieldValue state "Email"
                    p <- getFieldValue state "Passwort"
                    return (e,p)
               ) stat
    disconnect conn 
    return inh


-- |
-- input: email-adresse
-- output: ( Id, Matrikel )
--
getIdMat :: String -> IO ( Maybe ( String, String ))
getIdMat email = do 
    conn <- myconnect
    putStrLn $ "connected"
    stat <- query conn $ unlines
       [ "SELECT SNr, Email, MNr"
       , "FROM  student"
       , "WHERE Email = \"" ++ (filterQuots email)++ "\""
       -- kein semikolon? sonst absturz?
       ] 
    putStrLn $ "queried"
    inh <- collectRows ( \ state -> do
         a <- getFieldValue state "SNr"
         b <- getFieldValue state "MNr"
         return (  a, b )
                       ) stat
    putStrLn $ "collected"
    disconnect conn 
    putStrLn $ "disconnected"
    case inh of
         [ line ] -> return $ Just line
         _        -> return Nothing



-- | Vnr -> [ Mnr ]
teilnehme :: String -> IO [ String ]
teilnehme vnr = do
       conn <- myconnect
       state <- query conn 
               ( unwords
                 [ "SELECT student.mnr"
                 , "FROM   student, stud_grp, gruppe"
                 , unwords [ "WHERE gruppe.vnr = ", filterQuots vnr ]
                 , "AND stud_grp.gnr = gruppe.gnr"
                 , "AND student.snr = stud_grp.snr"
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "mnr"
                            return a
                          ) state
       disconnect conn 
       return inh


-- |
-- Liefer Vorlesungen von Matrikelnr.
--
-- Input:   Matrikelnr
-- Output:  IO [ Vorlesungsname ]
--
studVorlDB :: String -> IO [ String ]
studVorlDB mnr = 
    do 
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "SELECT vorlesung.Name AS Vorlesung \n"
                 , "FROM    student , stud_vorl , vorlesung \n"
                 , "WHERE   student.MNr = \"" 
                 , filterQuots mnr , "\" "
                 , "AND stud_vorl.SNr = student.SNr \n"
                 , "AND stud_vorl.VNr = vorlesung.VNr "
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Vorlesung"
                            return a
                          ) stat
       disconnect conn 
       return inh


-- |
-- Login des Studenten Version 2
--
-- Input:   Matrikelnr., Passwort
-- Output:  IO Just SNr zurück, wenn (mnr,pass) in DB
--
loginDBx :: String -> String -> IO (Maybe String)
-- loginDB "" "" = return $ Nothing
loginDBx mnr pass =
    do
       conn <- myconnect
       stat <- query conn
               ( concat
                 [ "SELECT "
                 , "SNr, Passwort \n"
                 , "FROM    student \n"
                 , "WHERE   student.MNr = \"" ++ (filterQuots mnr)++ "\" "
                 -- , "AND student.Passwort = \"" ++ (quoteQuots pass)++ "\" "
                 , ";"
                 ] )
       inhs <- collectRows ( \ state -> do
                            a <- getFieldValue state "SNr"
                            p <- getFieldValue state "Passwort"
                            return (a :: String, read p)
                          ) stat
       disconnect conn

       return $ case inhs of
           [ (a, p) ] -> do
		 guard $  Inter.Crypt.compare p pass
		 return a
           _ -> Nothing

