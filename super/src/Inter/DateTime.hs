-- | erzeugt nette default datumseinträge von morgen, 10:00 Uhr bis
-- morgen in einer Woche, 10:00 Uhr

module Inter.DateTime where

--  $Id$


import System.Time ( TimeDiff , noTimeDiff , tdDay , getClockTime 
		   , addToClockTime , toCalendarTime , formatCalendarTime 
		   )
import System.Locale ( defaultTimeLocale )

-------------------------------------------------------------------------------

type Format = String

date, time, full :: Format
time = "%H:%M:%S"
date = "%Y-%m-%d"
full = foldl1 (++) [ date , " " , time ]

-------------------------------------------------------------------------------

type Day = Int

calc :: Day -> Format -> IO String
calc d f = getClockTime
	   >>= return . addToClockTime (noTimeDiff { tdDay = d })
	   >>= toCalendarTime
	   >>= return . formatCalendarTime defaultTimeLocale f

yesterday, today, now, tomorrow, tomorrow_in_a_week :: IO String
yesterday          = calc (-1) date
today              = calc   0  date
now                = calc   0  full
tomorrow           = calc   1  date
tomorrow_in_a_week = calc   8  date

-------------------------------------------------------------------------------

defaults :: IO (String,String)
defaults = let ten = flip (++) " 10:00:00"
           in do start <- tomorrow
		 end <- tomorrow_in_a_week
		 return ( ten start , ten end )


