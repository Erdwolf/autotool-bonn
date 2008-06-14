{-# OPTIONS -cpp -fglasgow-exts -fallow-overlapping-instances #-}

module Control.Types 

( HiLo (..)
, Status (..)

, Wert ( Reset, Pending, No, Okay)
, size, is_okay
-- 
-- break abstraction because Inter.Action needs to see details.
-- Eigentlich sollte die instance XmlRpcType Wert hier in Inter.Types stehen,
-- aber das würde import import Network.XmlRpc.Internals erfordern,
-- d. h. jeder müßte erst haskell-XML-rpc installieren
, Wert ( Ok )

, ok, okay, size, punkte

-- , MNr, SNr, ANr, GNr, VNr -- abstrakt
, MNr(..), SNr(..), ANr(..), GNr(..), VNr(..) -- not abstrakt, see above
, UNr(..)
, ENr(..)

-- , ClockTime -- wer braucht das?

-- , Typ, Config, Remark, Name, File, Email
, Typ, Config, Remark, Name(..), File, Email(..)
, Oks (..), Nos (..)
, Time
, TimeStatus (..), timer
, FromCGI (..)
, ToString (..)
)

where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Hash

-- import System.Time
import Control.Time

import Control.SQL hiding ( equals )
import Inter.Crypt
import Data.Char ( toLower )
import Data.Maybe ( fromMaybe, listToMaybe )

import Database.HSQL.MySQL
import Database.HSQL.Types


import qualified Control.Exception
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml hiding ( Name )

-------------------------------------------------------------

class FromCGI a where fromCGI :: String -> a
class ToString a where toString :: a -> String

-------------------------------------------------------------

instance FromCGI Integer where fromCGI = read
instance ToString Integer where toString = show

-------------------------------------------------------------

instance ToEx Crypt where 
    toEx c = EString ( unCrypt c )
instance SqlBind Crypt where 
    fromSqlValue _ s = Just $ Crypt s
    toSqlValue c = unCrypt c

--------------------------------------------------------------

{-
data Time = Time String deriving ( Eq, Ord, Typeable )

instance SqlBind Time where 
    fromSqlValue _ s = Just $ Time s
    toSqlValue (Time s) = s

instance ToEx Time where
    toEx (Time x) = EString x
instance FromCGI Time where
    fromCGI s = Time s
instance ToString Time where
    toString (Time x) = x
-}

-- Zeit wird jetzt intern strukturiert:
instance SqlBind Time where 
    fromSqlValue _ s = listToMaybe $ do
        ( t, "" ) <- reads s
	return t
    toSqlValue s = show s

instance ToEx Time where
    toEx x = EString $ show x
instance FromCGI Time where
    fromCGI s = read s
instance ToString Time where
    toString x = show x


-------------------------------------------------------------

-- | bezeichnet eigenschaft der aktuellen zeit
-- mit dem in der DB angegebenen bereich
-- (es ist zu früh, richtig, zu spät)
-- bei jedem zugriff auf DB wird das gesetzt,
-- damit wir außerhalb der DB nie auf die Uhr sehen müssen
-- (die stimmen nämlich sowieso nicht überein)

data TimeStatus = Early | Current | Late deriving ( Eq, Ord, Typeable )

{-! for TimeStatus derive : Reader, ToDoc, Haskell2Xml !-}

timer :: Int -> Int -> TimeStatus
timer 1 _ = Early
timer _ 1 = Late
timer 0 0 = Current

-------------------------------------------------------------

data HiLo = Keine | High | Low 
    deriving ( Eq, Ord, Typeable, Bounded, Enum )

{-! for HiLo derive : Reader, ToDoc, Haskell2Xml !-}

instance SqlBind HiLo where 
    fromSqlValue _ s = Just 
        $ fromMaybe Keine 
	$ lookup (map toLower s)
        $ do h <- [ minBound .. maxBound ]
             return ( map toLower $ show h, h )
    toSqlValue w = show w

instance ToEx HiLo where
    toEx x = EString $ show x

instance ToString HiLo where
    toString = show 

-------------------------------------------------------------

data Status = Demo | Mandatory | Optional
    deriving ( Eq, Ord, Typeable, Bounded, Enum )

{-! for Status derive : Reader, ToDoc, Haskell2Xml !-}

instance SqlBind Status where 
    fromSqlValue _ s = Just 
        $ fromMaybe Demo
	$ lookup (map toLower s)
        $ do h <- [ minBound .. maxBound ]
             return ( map toLower $ show h, h )
    toSqlValue w = show w

instance ToEx Status where
    toEx x = EString $ show x

instance ToString Status where
    toString = show 

-------------------------------------------------------------

data Wert = Reset 
	  | Pending
	  | No 
	  | Ok Integer
          | Okay { punkte :: Integer, size_ :: Integer } 
    deriving ( Eq, Ord, Typeable )

okay p s = Okay { punkte = p, size_ = s }

size w = case w of 
    Ok p -> p
    Okay { } -> size_ w
    _ -> 0

is_okay w = case w of
    Ok _ -> True
    Okay {} -> True
    _ -> False

-- nice try, but constructors cannot be deprecated it seems
-- {-# DEPRECATED Ok "Use 'Okay' resp. smart constructor 'ok' instead" #-}

ok :: Integer -> Wert
ok s = Okay { punkte = 1, size_ = s }

-- | replace OK constructor by Okay
renovate :: Wert -> Wert
renovate w = case w of
    Ok s -> ok s
    _    -> w

{-! for Wert derive : Reader, ToDoc, Haskell2Xml !-}

instance SqlBind Wert where 
    fromSqlValue _ s = Just $ case reads s of
        [ (x, "") ] -> renovate x
	_           -> Pending
    toSqlValue w = show w

instance ToEx Wert where
    toEx x = EString $ show x

instance ToString Wert where
    toString = show 

-------------------------------------------------------------

-- | Matrikel-Nummer
data MNr = MNr String deriving ( Eq, Ord, Typeable )

{-! for MNr derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind MNr where 
    fromSqlValue _ s = Just $ MNr $ s
    toSqlValue (MNr i) = toSqlValue i
instance FromCGI MNr where
    fromCGI cs = MNr cs
instance ToString MNr where
    toString (MNr i) = i
instance ToEx MNr where
    toEx (MNr i) = EString i

-------------------------------------------------------------------

-- | Aufgaben-Typ
data Typ = Typ String deriving ( Eq, Ord, Typeable )

{-! for Typ derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind Typ where 
    fromSqlValue ty s = Just $ Typ s
    toSqlValue (Typ cs) = toSqlValue cs
instance ToEx Typ where
    toEx (Typ x) = EString x
instance FromCGI Typ where
    fromCGI cs = Typ cs
instance ToString Typ where 
    toString (Typ cs) = cs

------------------------------------------------------------

-- | Aufgaben-Email
data Email = Email String deriving ( Eq, Ord, Typeable )

{-! for Email derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind Email where 
    fromSqlValue ty s = Just $ Email s
    toSqlValue (Email cs) = toSqlValue cs
instance ToEx Email where
    toEx (Email x) = EString x
instance FromCGI Email where
    fromCGI cs = Email cs
instance ToString Email where 
    toString (Email cs) = cs

------------------------------------------------------------

-- | Aufgaben-Config
data Config = Config String deriving ( Eq, Ord, Typeable )
{-! for Config derive: Reader, ToDoc, Haskell2Xml !-}


instance SqlBind Config where 
    fromSqlValue ty s = Just $ Config s
    toSqlValue (Config cs) = toSqlValue cs
instance ToEx Config where
    toEx (Config x) = EString x
instance ToString Config where
    toString ( Config s ) = s
instance FromCGI Config where
    fromCGI s = Config s

-- | Aufgaben-Remark
data Remark = Remark String deriving ( Eq, Ord, Typeable )

{-! for Remark derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind Remark where 
    fromSqlValue ty s = Just $ Remark s
    toSqlValue (Remark cs) = toSqlValue cs
instance ToEx Remark where
    toEx (Remark x) = EString x
instance ToString Remark where
    toString ( Remark s ) = s
instance FromCGI Remark where
    fromCGI s = Remark s

-- | Aufgaben-Name
data Name = Name String deriving ( Eq, Ord, Typeable )

{-! for Name derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind Name where 
    fromSqlValue ty s = Just $ Name s
    toSqlValue (Name cs) = toSqlValue cs
instance ToEx Name where
    toEx (Name x) = EString x
instance ToString Name where 
    toString (Name cs) = cs
instance FromCGI Name where
    fromCGI cs = Name cs

-- | File-Name
data File = File String deriving ( Eq, Ord, Typeable )

{-! for File derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind File where 
    fromSqlValue ty s = Just $ File s
    toSqlValue (File cs) = toSqlValue cs
instance ToEx File where
    toEx (File x) = EString x
instance ToString File where 
    toString (File cs) = cs
instance FromCGI File where
    fromCGI cs = File cs

-------------------------------------------------------------

-- | Studenten-Nummer
data SNr = SNr Int deriving ( Eq, Ord, Typeable )

{-! for SNr derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind SNr where 
    fromSqlValue _ s = Just $ SNr $ read s -- FIXME: check SqlType
    toSqlValue (SNr i) = toSqlValue i

instance ToEx SNr where
    toEx (SNr x) = EString $ show x
instance ToString SNr where
    toString (SNr x) = show x

-- | Aufgaben-Nummer

data ANr = ANr Int deriving ( Eq, Ord, Typeable )

{-! for ANr derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind ANr where 
    fromSqlValue _ s = Just $ ANr $ read s -- FIXME: check SqlType
    toSqlValue (ANr i) = toSqlValue i
instance ToEx ANr where
    toEx (ANr x) = EString $ show x
instance ToString ANr where
    toString (ANr i) = show i
instance FromCGI ANr where
    fromCGI cs = ANr $ read cs
instance Hash ANr where
    hash (ANr x) = hash ( 25 :: Int , x)

-- | Schul-Nummer

data UNr = UNr Int deriving ( Eq, Ord, Typeable )

{-! for UNr derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind UNr where 
    fromSqlValue _ s = Just $ UNr $ read s -- FIXME: check SqlType
    toSqlValue (UNr i) = toSqlValue i
instance ToEx UNr where
    toEx (UNr x) = EString $ show x
instance ToString UNr where
    toString (UNr i) = show i
instance FromCGI UNr where
    fromCGI cs = UNr $ read cs


-- | Semester-Nummer

data ENr = ENr Int deriving ( Eq, Ord, Typeable )

{-! for ENr derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind ENr where 
    fromSqlValue _ s = Just $ ENr $ read s -- FIXME: check SqlType
    toSqlValue (ENr i) = toSqlValue i
instance ToEx ENr where
    toEx (ENr x) = EString $ show x
instance ToString ENr where
    toString (ENr i) = show i
instance FromCGI ENr where
    fromCGI cs = ENr $ read cs


-- | Gruppen-Nummer

data GNr = GNr Int deriving ( Eq, Ord, Typeable )

{-! for GNr derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind GNr where 
    fromSqlValue _ s = Just $ GNr $ read s -- FIXME: check SqlType
    toSqlValue (GNr i) = toSqlValue i
instance ToEx GNr where
    toEx (GNr x) = EString $ show x
instance FromCGI GNr where
    fromCGI cs = GNr $ read cs

-- | Vorlesungs-Nummer

data VNr = VNr Int deriving ( Eq, Ord, Typeable )

{-! for VNr derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind VNr where 
    fromSqlValue _ s = Just $ VNr $ read s -- FIXME: check SqlType
    toSqlValue (VNr i) = toSqlValue i
instance ToEx VNr where
    toEx (VNr x) = EString $ show x
instance ToString VNr where
    toString (VNr x) = show x
instance FromCGI VNr where
    fromCGI cs = VNr $ read cs
instance Hash VNr where
    hash ( VNr x ) = hash ( 17 :: Int , x )

-- | OK-Bewertung (Anzahl)

data Oks = Oks Int deriving ( Eq, Ord, Typeable )

{-! for Oks derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind Oks where 
    fromSqlValue ty s = Just $ Oks $ read s
    toSqlValue (Oks i) = toSqlValue i
instance ToEx Oks where
    toEx (Oks i) = EInteger $ fromIntegral i
instance ToString Oks where
    toString (Oks x) = show x

-- | No-Bewertung (Anzahl)

data Nos = Nos Int deriving ( Eq, Ord, Typeable )

{-! for Nos derive: ToDoc, Reader, Haskell2Xml !-}

instance SqlBind Nos where 
    fromSqlValue ty s = Just $ Nos $ read s
    toSqlValue (Nos i) = toSqlValue i
instance ToEx Nos where
    toEx (Nos i) = EInteger $ fromIntegral i
instance ToString Nos where
    toString (Nos x) = show x

-------------------------------------------------------------

-- Local Variables:
-- mode: haskell
-- End:


