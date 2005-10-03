{-# LINE 1 "Control/Types.hs.drift" #-}
module Control.Types 

( HiLo (..)
, Status (..)

, Wert ( Reset, Pending, No, Okay )
-- 
-- break abstraction because Inter.Action needs to see details.
-- Eigentlich sollte die instance XmlRpcType Wert hier in Inter.Types stehen,
-- aber das würde import import Network.XmlRpc.Internals erfordern,
-- d. h. jeder müßte erst haskell-XML-rpc installieren
, Wert ( Ok )

, ok, size, punkte

-- , MNr, SNr, ANr, GNr, VNr -- abstrakt
, MNr(..), SNr(..), ANr(..), GNr(..), VNr(..) -- not abstrakt, see above
, UNr(..)

, ClockTime
-- , Typ, Config, Remark, Name, File, Email
, Typ, Config, Remark, Name(..), File, Email(..)
, Oks (..), Nos (..)
, Time
, TimeStatus (..), timer
, FromCGI (..)
, ToString (..)
)

where

--  $Id$

import Autolib.Reader
import Autolib.ToDoc

import System.Time
import Control.SQL hiding ( equals )
import Inter.Crypt
import Data.Char ( toLower )
import Data.Maybe ( fromMaybe )

#ifdef HSQL16
import Database.HSQL.MySQL
import Database.HSQL.Types
#elif HSQL14
import Database.HSQL.MySQL
import Database.HSQL.Types
#elif HSQL12
import Database.MySQL.HSQL
#endif

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

data Time = Time String deriving ( Eq, Ord, Typeable )

{-! for Time derive : Reader, ToDoc, Haskell2Xml !-}

instance SqlBind Time where 
    fromSqlValue _ s = Just $ Time s
    toSqlValue (Time s) = s

instance ToEx Time where
    toEx (Time x) = EString x
instance FromCGI Time where
    fromCGI s = Time s
instance ToString Time where
    toString (Time x) = x

-------------------------------------------------------------

-- | bezeichnet eigenschaft der aktuellen zeit
-- mit dem in der DB angegebenen bereich
-- (es ist zu früh, richtig, zu spät)
-- bei jedem zugriff auf DB wird das gesetzt,
-- damit wir außerhalb der DB nie auf die Uhr sehen müssen
-- (die stimmen nämlich sowieso nicht überein)

data TimeStatus = Early | Current | Late deriving ( Eq, Ord, Typeable )

{-! for TimeStatus derive : Reader, ToDoc, Haskell2Xml !-}

#ifdef HSQL16
timer :: Int -> Int -> TimeStatus
timer 1 _ = Early
timer _ 1 = Late
timer 0 0 = Current
#elif HSQL14
timer :: Int -> Int -> TimeStatus
timer 1 _ = Early
timer _ 1 = Late
timer 0 0 = Current
#elif HSQL12
timer :: Bool -> Bool -> TimeStatus
timer True _ = Early
timer _ True = Late
timer False False = Current
#endif

-------------------------------------------------------------

#ifdef HSQL16
-- this HAS an Bool instance already
-- but we are not going to use it
#elif HSQL14
-- this HAS an Bool instance already
-- but we are not going to use it
#elif HSQL12
instance SqlBind Bool where
    fromSqlValue _ s = return $ "0" /= s
    toSqlValue f = show $ fromEnum f
#endif

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
          | Okay { punkte :: Integer, size :: Integer } 
    deriving ( Eq, Ord, Typeable )

-- nice try, but constructors cannot be deprecated it seems
-- {-# DEPRECATED Ok "Use 'Okay' resp. smart constructor 'ok' instead" #-}

ok :: Integer -> Wert
ok s = Okay { punkte = 1, size = s }

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


{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance ToDoc Nos where
    toDocPrec d (Nos aa) = docParen (d >= 10)
	      (text "Nos" <+> fsep [toDocPrec 10 aa])

instance Reader Nos where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Nos"
			   aa <- readerPrec 9
			   return (Nos aa)))

instance Haskell2Xml Nos where
    toHType v =
	Defined "Nos" [] [Constr "Nos" [] [toHType aa]]
      where
	(Nos aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Nos" `isPrefixOf` constr =
	    (\(aa,_)-> (Nos aa, etc)) (fromContents cs)
    toContents v@(Nos aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc Oks where
    toDocPrec d (Oks aa) = docParen (d >= 10)
	      (text "Oks" <+> fsep [toDocPrec 10 aa])

instance Reader Oks where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Oks"
			   aa <- readerPrec 9
			   return (Oks aa)))

instance Haskell2Xml Oks where
    toHType v =
	Defined "Oks" [] [Constr "Oks" [] [toHType aa]]
      where
	(Oks aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Oks" `isPrefixOf` constr =
	    (\(aa,_)-> (Oks aa, etc)) (fromContents cs)
    toContents v@(Oks aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc VNr where
    toDocPrec d (VNr aa) = docParen (d >= 10)
	      (text "VNr" <+> fsep [toDocPrec 10 aa])

instance Reader VNr where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "VNr"
			   aa <- readerPrec 9
			   return (VNr aa)))

instance Haskell2Xml VNr where
    toHType v =
	Defined "VNr" [] [Constr "VNr" [] [toHType aa]]
      where
	(VNr aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "VNr" `isPrefixOf` constr =
	    (\(aa,_)-> (VNr aa, etc)) (fromContents cs)
    toContents v@(VNr aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc GNr where
    toDocPrec d (GNr aa) = docParen (d >= 10)
	      (text "GNr" <+> fsep [toDocPrec 10 aa])

instance Reader GNr where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "GNr"
			   aa <- readerPrec 9
			   return (GNr aa)))

instance Haskell2Xml GNr where
    toHType v =
	Defined "GNr" [] [Constr "GNr" [] [toHType aa]]
      where
	(GNr aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "GNr" `isPrefixOf` constr =
	    (\(aa,_)-> (GNr aa, etc)) (fromContents cs)
    toContents v@(GNr aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc UNr where
    toDocPrec d (UNr aa) = docParen (d >= 10)
	      (text "UNr" <+> fsep [toDocPrec 10 aa])

instance Reader UNr where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "UNr"
			   aa <- readerPrec 9
			   return (UNr aa)))

instance Haskell2Xml UNr where
    toHType v =
	Defined "UNr" [] [Constr "UNr" [] [toHType aa]]
      where
	(UNr aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "UNr" `isPrefixOf` constr =
	    (\(aa,_)-> (UNr aa, etc)) (fromContents cs)
    toContents v@(UNr aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc ANr where
    toDocPrec d (ANr aa) = docParen (d >= 10)
	      (text "ANr" <+> fsep [toDocPrec 10 aa])

instance Reader ANr where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "ANr"
			   aa <- readerPrec 9
			   return (ANr aa)))

instance Haskell2Xml ANr where
    toHType v =
	Defined "ANr" [] [Constr "ANr" [] [toHType aa]]
      where
	(ANr aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "ANr" `isPrefixOf` constr =
	    (\(aa,_)-> (ANr aa, etc)) (fromContents cs)
    toContents v@(ANr aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc SNr where
    toDocPrec d (SNr aa) = docParen (d >= 10)
	      (text "SNr" <+> fsep [toDocPrec 10 aa])

instance Reader SNr where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "SNr"
			   aa <- readerPrec 9
			   return (SNr aa)))

instance Haskell2Xml SNr where
    toHType v =
	Defined "SNr" [] [Constr "SNr" [] [toHType aa]]
      where
	(SNr aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "SNr" `isPrefixOf` constr =
	    (\(aa,_)-> (SNr aa, etc)) (fromContents cs)
    toContents v@(SNr aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc File where
    toDocPrec d (File aa) = docParen (d >= 10)
	      (text "File" <+> fsep [toDocPrec 10 aa])

instance Reader File where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "File"
			   aa <- readerPrec 9
			   return (File aa)))

instance Haskell2Xml File where
    toHType v =
	Defined "File" [] [Constr "File" [] [toHType aa]]
      where
	(File aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "File" `isPrefixOf` constr =
	    (\(aa,_)-> (File aa, etc)) (fromContents cs)
    toContents v@(File aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc Name where
    toDocPrec d (Name aa) = docParen (d >= 10)
	      (text "Name" <+> fsep [toDocPrec 10 aa])

instance Reader Name where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Name"
			   aa <- readerPrec 9
			   return (Name aa)))

instance Haskell2Xml Name where
    toHType v =
	Defined "Name" [] [Constr "Name" [] [toHType aa]]
      where
	(Name aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Name" `isPrefixOf` constr =
	    (\(aa,_)-> (Name aa, etc)) (fromContents cs)
    toContents v@(Name aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc Remark where
    toDocPrec d (Remark aa) = docParen (d >= 10)
	      (text "Remark" <+> fsep [toDocPrec 10 aa])

instance Reader Remark where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Remark"
			   aa <- readerPrec 9
			   return (Remark aa)))

instance Haskell2Xml Remark where
    toHType v =
	Defined "Remark" [] [Constr "Remark" [] [toHType aa]]
      where
	(Remark aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Remark" `isPrefixOf` constr =
	    (\(aa,_)-> (Remark aa, etc)) (fromContents cs)
    toContents v@(Remark aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance Reader Config where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Config"
			   aa <- readerPrec 9
			   return (Config aa)))

instance ToDoc Config where
    toDocPrec d (Config aa) = docParen (d >= 10)
	      (text "Config" <+> fsep [toDocPrec 10 aa])

instance Haskell2Xml Config where
    toHType v =
	Defined "Config" [] [Constr "Config" [] [toHType aa]]
      where
	(Config aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Config" `isPrefixOf` constr =
	    (\(aa,_)-> (Config aa, etc)) (fromContents cs)
    toContents v@(Config aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc Email where
    toDocPrec d (Email aa) = docParen (d >= 10)
	      (text "Email" <+> fsep [toDocPrec 10 aa])

instance Reader Email where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Email"
			   aa <- readerPrec 9
			   return (Email aa)))

instance Haskell2Xml Email where
    toHType v =
	Defined "Email" [] [Constr "Email" [] [toHType aa]]
      where
	(Email aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Email" `isPrefixOf` constr =
	    (\(aa,_)-> (Email aa, etc)) (fromContents cs)
    toContents v@(Email aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc Typ where
    toDocPrec d (Typ aa) = docParen (d >= 10)
	      (text "Typ" <+> fsep [toDocPrec 10 aa])

instance Reader Typ where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Typ"
			   aa <- readerPrec 9
			   return (Typ aa)))

instance Haskell2Xml Typ where
    toHType v =
	Defined "Typ" [] [Constr "Typ" [] [toHType aa]]
      where
	(Typ aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Typ" `isPrefixOf` constr =
	    (\(aa,_)-> (Typ aa, etc)) (fromContents cs)
    toContents v@(Typ aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance ToDoc MNr where
    toDocPrec d (MNr aa) = docParen (d >= 10)
	      (text "MNr" <+> fsep [toDocPrec 10 aa])

instance Reader MNr where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "MNr"
			   aa <- readerPrec 9
			   return (MNr aa)))

instance Haskell2Xml MNr where
    toHType v =
	Defined "MNr" [] [Constr "MNr" [] [toHType aa]]
      where
	(MNr aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "MNr" `isPrefixOf` constr =
	    (\(aa,_)-> (MNr aa, etc)) (fromContents cs)
    toContents v@(MNr aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

instance Reader Wert where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do my_reserved "Reset"
			   return (Reset))
		       <|>
		       (do my_reserved "Pending"
			   return (Pending))
		       <|>
		       (do my_reserved "No"
			   return (No))
		       <|>
		       (do guard (d < 9)
			   my_reserved "Ok"
			   aa <- readerPrec 9
			   return (Ok aa))
		       <|>
		       (do guard (d < 9)
			   my_reserved "Okay"
			   my_braces ((do my_reserved "punkte"
					  my_equals
					  aa <- readerPrec 0
					  my_comma
					  my_reserved "size"
					  my_equals
					  ab <- readerPrec 0
					  return (Okay aa ab)))))

instance ToDoc Wert where
    toDocPrec d (Reset) = text "Reset"
    toDocPrec d (Pending) = text "Pending"
    toDocPrec d (No) = text "No"
    toDocPrec d (Ok aa) = docParen (d >= 10)
	      (text "Ok" <+> fsep [toDocPrec 10 aa])
    toDocPrec d (Okay aa ab) = docParen (d >= 10)
	      (text "Okay" <+> dutch_record
	       [text "punkte" <+> equals <+> toDocPrec 0 aa,
		text "size" <+> equals <+> toDocPrec 0 ab])

instance Haskell2Xml Wert where
    toHType v =
	Defined "Wert" []
		[Constr "Reset" [] [],Constr "Pending" [] [],Constr "No" [] [],
		 Constr "Ok" [] [toHType aa],
		 Constr "Okay" [] [toHType ab,toHType ac]]
      where
	(Ok aa) = v
	(Okay ab ac) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Reset" `isPrefixOf` constr =
	    (Reset,etc)
	| "Pending" `isPrefixOf` constr =
	    (Pending,etc)
	| "Okay" `isPrefixOf` constr =
	    (\(ab,cs00)-> (\(ac,_)-> (Okay ab ac, etc)) (fromContents cs00))
	    (fromContents cs)
	| "Ok" `isPrefixOf` constr =
	    (\(aa,_)-> (Ok aa, etc)) (fromContents cs)
	| "No" `isPrefixOf` constr =
	    (No,etc)
    toContents v@Reset =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@Pending =
	[mkElemC (showConstr 1 (toHType v)) []]
    toContents v@No =
	[mkElemC (showConstr 2 (toHType v)) []]
    toContents v@(Ok aa) =
	[mkElemC (showConstr 3 (toHType v)) (toContents aa)]
    toContents v@(Okay ab ac) =
	[mkElemC (showConstr 4 (toHType v)) (concat [toContents ab,
						     toContents ac])]

instance Reader Status where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do my_reserved "Demo"
			   return (Demo))
		       <|>
		       (do my_reserved "Mandatory"
			   return (Mandatory))
		       <|>
		       (do my_reserved "Optional"
			   return (Optional)))

instance ToDoc Status where
    toDocPrec d (Demo) = text "Demo"
    toDocPrec d (Mandatory) = text "Mandatory"
    toDocPrec d (Optional) = text "Optional"

instance Haskell2Xml Status where
    toHType v =
	Defined "Status" []
		[Constr "Demo" [] [],Constr "Mandatory" [] [],
		 Constr "Optional" [] []]
    fromContents (CElem (Elem constr [] cs):etc)
	| "Optional" `isPrefixOf` constr =
	    (Optional,etc)
	| "Mandatory" `isPrefixOf` constr =
	    (Mandatory,etc)
	| "Demo" `isPrefixOf` constr =
	    (Demo,etc)
    toContents v@Demo =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@Mandatory =
	[mkElemC (showConstr 1 (toHType v)) []]
    toContents v@Optional =
	[mkElemC (showConstr 2 (toHType v)) []]

instance Reader HiLo where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do my_reserved "Keine"
			   return (Keine))
		       <|>
		       (do my_reserved "High"
			   return (High))
		       <|>
		       (do my_reserved "Low"
			   return (Low)))

instance ToDoc HiLo where
    toDocPrec d (Keine) = text "Keine"
    toDocPrec d (High) = text "High"
    toDocPrec d (Low) = text "Low"

instance Haskell2Xml HiLo where
    toHType v =
	Defined "HiLo" []
		[Constr "Keine" [] [],Constr "High" [] [],Constr "Low" [] []]
    fromContents (CElem (Elem constr [] cs):etc)
	| "Low" `isPrefixOf` constr =
	    (Low,etc)
	| "Keine" `isPrefixOf` constr =
	    (Keine,etc)
	| "High" `isPrefixOf` constr =
	    (High,etc)
    toContents v@Keine =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@High =
	[mkElemC (showConstr 1 (toHType v)) []]
    toContents v@Low =
	[mkElemC (showConstr 2 (toHType v)) []]

instance Reader TimeStatus where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do my_reserved "Early"
			   return (Early))
		       <|>
		       (do my_reserved "Current"
			   return (Current))
		       <|>
		       (do my_reserved "Late"
			   return (Late)))

instance ToDoc TimeStatus where
    toDocPrec d (Early) = text "Early"
    toDocPrec d (Current) = text "Current"
    toDocPrec d (Late) = text "Late"

instance Haskell2Xml TimeStatus where
    toHType v =
	Defined "TimeStatus" []
		[Constr "Early" [] [],Constr "Current" [] [],Constr "Late" [] []]
    fromContents (CElem (Elem constr [] cs):etc)
	| "Late" `isPrefixOf` constr =
	    (Late,etc)
	| "Early" `isPrefixOf` constr =
	    (Early,etc)
	| "Current" `isPrefixOf` constr =
	    (Current,etc)
    toContents v@Early =
	[mkElemC (showConstr 0 (toHType v)) []]
    toContents v@Current =
	[mkElemC (showConstr 1 (toHType v)) []]
    toContents v@Late =
	[mkElemC (showConstr 2 (toHType v)) []]

instance Reader Time where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Time"
			   aa <- readerPrec 9
			   return (Time aa)))

instance ToDoc Time where
    toDocPrec d (Time aa) = docParen (d >= 10)
	      (text "Time" <+> fsep [toDocPrec 10 aa])

instance Haskell2Xml Time where
    toHType v =
	Defined "Time" [] [Constr "Time" [] [toHType aa]]
      where
	(Time aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Time" `isPrefixOf` constr =
	    (\(aa,_)-> (Time aa, etc)) (fromContents cs)
    toContents v@(Time aa) =
	[mkElemC (showConstr 0 (toHType v)) (toContents aa)]

--  Imported from other files :-
