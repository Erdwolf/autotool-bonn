module Control.Punkt where

-----------------------------------------------------------------------------------------

-- | fürs AUTOTOOL

--
-- erhöht von Student, für Aufgabe (Ok,Size) / No 
--
-- Input: (SNr,ANr,{No,Ok size}, {High,Low,Keine} )
-- Output: IO ()
--
-- bepunkteStudentDB :: String -> String -> ATBewertung -> ATHighLow -> IO ()
-- bepunkteStudentDB snr anr bewert highlow = return ()

bepunkteStudentDB :: SNr -> ANr -> Wert -> HiLo -> IO ()
bepunkteStudentDB snr anr bewert highlow = do
   conn <- myconnect
   stat <- query conn ("SELECT SNr FROM stud_aufg \n" ++ 
                        "WHERE SNr = \"" ++ filterQuots snr ++ "\" "++
                        "AND ANr = \"" ++ filterQuots anr ++ "\" " ++
                        ";"
                       )
   inh <- collectRows ( \ state -> do
                        b <- getFieldValue state "SNr"
                        return (b :: String)
                      ) stat

   --
   -- wenn (snr,anr) bereits in db -> update der Zeile sonst insert Zeile
   --
   let tim s = case highlow of 
			   Keine -> "0,\"0000-00-00 00:00:00\""
			   _     -> show s ++ ", NOW()" 
   let insertsql = 
		   concat [ "INSERT INTO stud_aufg (SNr,ANr,Ok,No,Size,Scoretime) VALUES \n" 
				  , "( \"" ++ filterQuots snr ++ "\" "
				  , ", \"" ++ filterQuots anr ++ "\""
				  , "," 
					-- 
				  , case bewert of 
					No    -> "0,1,NULL,\"0000-00-00 00:00:00\"" 
					Ok s  -> "1,0," ++ tim s
				  , " )"
				  , ";"
				  ] 
   if null inh  
      then  -- insert 

      query conn insertsql

                
      else  -- update
      query conn
                ( concat 
                  [ "UPDATE stud_aufg \n"
                  , "SET \n"
                  , case bewert of 
                    No  -> "No = No + 1 "
                    Ok s   -> "Ok = Ok + 1 " -- ++ sizetime s
                  , " \n"
                  , "WHERE SNr = \"" ++ filterQuots snr ++ "\" "
                  , "AND ANr = \"" ++ filterQuots anr ++ "\" "
                  , ";"
                  ] )
   disconnect conn 
   return ()

     where sizetime s =
             case highlow of 
             Keine   -> " " 
             High    -> " "
                        ++ ", Scoretime = IF( IFNULL(Size, "++ show s ++" - 1) < " ++ show s 
                       ++ " , Now(), Scoretime )"
                       ++ ", Size = GREATEST( Size," ++ show s ++ ")" 
             Low -> " "
                    ++ ", Scoretime = IF( IFNULL(Size, "++ show s ++" + 1 ) > " ++ show s
                   ++ " ,Now(), Scoretime )"
                   ++ ", Size = LEAST( Size," ++ show s ++ ")" 


