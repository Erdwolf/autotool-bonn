-- © 2001 Peter Thiemann
module Wash.RawCGI where
-- (CGIParameter, CGIParameters, start, urlDecode, urlEncode, assocParm, assoc)

import Array
import Char
import IO
import Data.List
import Maybe
import Random
import System

import Wash.Auxiliary
import Wash.Unique
import Wash.Config
import Wash.Hex

data CGIOption =
  NoPort | AutoPort | Port Int
  deriving Eq
type CGIOptions = [CGIOption]

isPortOption NoPort = True
isPortOption AutoPort = True
isPortOption (Port _) = True
isPortOption _ = False

getPortOption options =
  fromJust (find isPortOption (reverse (AutoPort:options)))

type CGIParameter = (String, CGIValue)
type CGIParameters = [CGIParameter]
type URL = String

type CGIValue = String

fieldName = fst
fieldContents = snd

data FileReference =
     FileReference { fileReferenceName :: FilePath
     		   , fileReferenceContentType :: String
		   , fileReferenceExternalName :: String
		   }
     deriving (Show, Read)

data CGIRawValue =
     CGIRawString String
   | CGIRawFile String String String		    -- name, contentType, content

data CGIInfo = 
     CGIInfo    { cgiUrl :: URL
     		, cgiPathInfo :: String
	       	, cgiContentType :: String
		, cgiContents :: String
		, cgiCookies :: [(String, String)]
		}

start :: CGIOptions -> (CGIInfo -> CGIParameters -> IO a) -> IO a
start options f = 
  do server_name <- protectedGetEnv "SERVER_NAME" "localhost"
     server_port <- protectedGetEnv "SERVER_PORT" "80"
     script_name <- protectedGetEnv "SCRIPT_NAME" ""
     request_method <- protectedGetEnv "REQUEST_METHOD" "GET"
     content_length <- protectedGetEnv "CONTENT_LENGTH" "0"
     contentType <- protectedGetEnv "CONTENT_TYPE" "application/x-www-form-urlencoded"
     http_cookies <- protectedGetEnv "HTTP_COOKIE" ""
     rawContents <- getContents
     path_info <- protectedGetEnv "PATH_INFO" ""
     let portString = 
	   case getPortOption options of
	     AutoPort -> ':' : server_port
	     NoPort   -> ""
	     Port num -> ':' : show num
	 myurl = "http://" ++ server_name ++ portString ++
	 	 script_name ++ path_info
	 methodIsGet 
	   = case advanceIC "GET" request_method of {Just "" -> True; _ -> False}
	 contentIsURLEncoded
	   = case advanceIC "application/x-www-form-urlencoded" contentType of
	     {Just "" -> True; _ -> False}
	 contentLength = read content_length
	 contents = take contentLength rawContents
	 rawDecodedParameters 
	   | methodIsGet = []
	   | contentIsURLEncoded = map decodeLine $ parameterLines contents
	   | otherwise = decodeMultiPart contentType contents
	 parsed_cookies = parseCookies http_cookies
	 info = CGIInfo { cgiUrl = myurl
	 		, cgiPathInfo = path_info
	 		, cgiContentType = contentType
			, cgiContents = contents
			, cgiCookies = parsed_cookies
			}
     decodedParameters <- resolveFiles rawDecodedParameters
     -- writeFile "/tmp/CGIPATHINFO" path_info
     -- writeFile "/tmp/CGIRAWINPUT" contents
     -- writeFile "/tmp/CGIDECODED" (show decodedParameters)
     f info decodedParameters

argsToPath :: [String] -> String
argsToPath = concatMap ('/':)

resolveFiles :: [(String, CGIRawValue)] -> IO CGIParameters
resolveFiles =
  let resolveOne (key, rv) =
        case rv of
          CGIRawString s -> 
	    return (key, s)
	  CGIRawFile fileName contentType fileContents ->
	    do localName <- inventFilePath
	       writeFile localName fileContents
	       let fileRef = FileReference
		           { fileReferenceName = localName
			   , fileReferenceContentType = contentType
			   , fileReferenceExternalName = fileName
			   }
	       return (key, show fileRef)
  in mapM resolveOne

parseCookies :: String -> [(String, String)]
parseCookies str = 
  let s0 = dropWhile isSpace str in
  if null s0 then [] else
  let (item, rest) = span (/= ';') s0 in
  case span (/= '=') item of
    (key, '=':value) ->
      (key, value) : parseCookies (dropWhile (== ';') rest)
    _ -> 
      error ("Trying to parse cookie: " ++ str)

dropSpecialParameters :: [(String,a)] -> [(String,a)]
dropSpecialParameters = filter (f . fieldName)
	where f ('=':_) = False
	      f _ = True

decodeMultiPart :: String -> String -> [(String, CGIRawValue)]
decodeMultiPart contentType contents
  = let Just boundary = extractBoundary contentType 
	startBoundary = '-':'-':boundary
	g source = case 
	  (
	  advanceIC startBoundary source >>= \afterBoundary ->
	  case afterBoundary of 
	   '-':_ -> Nothing
	   '\13':'\10':body ->
		advanceIC "content-disposition: form-data; name=\"" body
		>>= \fieldNameRest ->
		let (fieldName, rest) = span (/= '\"') fieldNameRest in
		advanceIC "\"" rest
		>>= \mayBeFileNameRest ->
		(case mayBeFileNameRest of
		  ';':_ -> advanceIC "; filename=\"" mayBeFileNameRest
		           >>= \fileNameRest ->
		           let (fileName, rest) = span (/= '\"') fileNameRest in
		           advanceIC "\"" rest
		           >>= \mayBeContentTypeRest ->
		           return (fileName, mayBeContentTypeRest)
		  _ -> return ("", mayBeFileNameRest))
		>>= \ (fileName, fileNameRest) ->
		let (contentType, rest) =
		      case advanceIC "\ncontent-type: " fileNameRest of
		        Just contentTypeRest -> span (/= '\13') contentTypeRest
		        Nothing -> ("text/plain", fileNameRest) in
		advanceIC "\n\n" rest
		>>= \contentRest ->
		extractContents startBoundary contentRest
		>>= \ (fieldContents, rest) ->
		-- fieldName must be rfc1522decoded (?)
		let moreParms = g rest 
		    rawvalue = if null fileName 
		    	       then CGIRawString fieldContents
			       else CGIRawFile fileName contentType fieldContents
		in  return ((fieldName, rawvalue) : moreParms)
	   )
	  of
	    Just parameters -> parameters
	    Nothing -> []
    in g contents

extractContents :: String -> String -> Maybe (String, String)
extractContents boundary source
  = g "" source
  where g rev "" = Nothing
	g rev ('\r':'\n':xs) = case advanceIC boundary xs of
				Just _  -> Just (reverse rev, xs)
				Nothing -> g ('\n':'\r':rev) xs
	g rev (x:xs) = g (x:rev) xs


extractBoundary :: String -> Maybe String
extractBoundary contentType = advanceIC "multipart/form-data; boundary=" contentType 

advanceIC :: String -> String -> Maybe String
advanceIC [] ys = Just ys
advanceIC xs [] = Nothing
advanceIC (' ':xs) (y:ys)
	| isSpace y = advanceIC xs (dropWhile isSpace ys)
advanceIC ('\n':xs) ('\13':'\10':ys)
	= advanceIC xs ys
advanceIC (x:xs) (y:ys) 
	| toUpper x == toUpper y = advanceIC xs ys
	| otherwise = Nothing

parameterLines :: String -> [String]
parameterLines "" = []
parameterLines xs = let (firstPar, restPar) = span (/= '&') xs in
		    case restPar of
		      '&' : moreParameters -> firstPar : parameterLines moreParameters
		      _ -> [firstPar]

decodeLine :: String -> (String, CGIRawValue)
decodeLine str = 
  let (name, '=':value) = span (/= '=') str in
  (urlDecode name, CGIRawString $ urlDecode value)

urlEncode "" = ""
urlEncode (x:xs) | isAlphaNum x = x : urlEncode xs
		 | x == ' '     = '+' : urlEncode xs
		 | otherwise    = '%' : showHex2 (ord x) ++ urlEncode xs

urlDecode "" = ""
urlDecode ('+':xs) =
	' ' : urlDecode xs
urlDecode ('%':upper:lower:xs) =
	chr (16 * hexDigitVal upper + hexDigitVal lower) : urlDecode xs
urlDecode (x:xs) = 
	x : urlDecode xs

assocParm :: String -> CGIParameters -> Maybe CGIValue
assocParm key =
  listToMaybe . assocParmL key

assocParmL :: String -> CGIParameters -> [CGIValue]
assocParmL key =
  map fieldContents . assocParmR key

assocParmR :: String -> CGIParameters -> CGIParameters
assocParmR key =
  filter (\parm -> fieldName parm == key) 

assoc :: (Eq a) => a -> [(a,b)] -> Maybe b
assoc key alist =
  ass alist 
  where ass ((a,b):rest) = if a == key then Just b else ass rest
	ass [] = Nothing -- error ("assoc ("++show key++") "++show alist)

-- update :: [(a,b)] -> a -> b -> [(a,b)]
-- update alist a b = (a,b) : alist

fieldNames :: CGIParameters -> [String]
fieldNames = map fieldName

-- encryption

generateKey :: IO (Maybe (Integer, String, String))
generateKey =
  try (openFile keyFile ReadMode) >>= g
  where
    g (Left ioerror) =
      return Nothing
    g (Right h) =
      do size <- hFileSize h
	 let size2 = size `div` 2
	 pos <- randomRIO (0, size2)
	 if pos < 0
	    then return Nothing 
	    else do hSeek h AbsoluteSeek pos
		    xs <- hGetContents h
		    g <- getStdGen
		    return (Just (pos
		    		 ,randomRs (minBound, maxBound) g
				 ,extendRandomly xs))

extendRandomly :: [Char] -> [Char]
extendRandomly xs = h 0 xs
  where h n (x:xs) = x : h (n + ord x) xs
	h n []     = randoms (mkStdGen n)

nrNonces :: Int
nrNonces = 16

makeEncoder :: Maybe (Integer, String, String) -> String -> String
makeEncoder Nothing xs = xs
makeEncoder (Just (i, nonces, keys)) xs = 
  show i ++ ';' : encrypt1 (take nrNonces nonces ++ xs) keys

decode :: String -> IO String
decode inp = g (reads inp)
  where
    g :: [(Integer, String)] -> IO String
    g ((pos, ';':encrypted) : _) = 
      do h <- openFile keyFile ReadMode
	 hSeek h AbsoluteSeek pos
	 xs <- hGetContents h
	 return (drop nrNonces $ decrypt1 encrypted (extendRandomly xs))
    g _ = return inp

encrypt1 inp keys = map chr (enc 0 (map ord inp) (map ord keys))
  where enc acc [] okeys = []
	enc acc (oinp : oinps) (okey : okeys) =
	  let out = (oinp + okey + acc) `mod` 256 in
	  out : enc ((acc + oinp) `mod` 256) oinps okeys

decrypt1 einp keys = map chr (dec 0 (map ord einp) (map ord keys))
  where dec acc [] okeys = []
	dec acc (oeinp : oeinps) (okey : okeys) =
	  let oinp = (512 + oeinp - okey - acc) `mod` 256 in
	  oinp : dec ((acc + oinp) `mod` 256) oeinps okeys

encrypt, decrypt :: String -> String -> String
encrypt = zipWith cadd
cadd c1 c2 = chr (ord c1 + ord c2 `mod` 256)
decrypt = zipWith csub
csub c1 c2 = chr ((ord c1 + 256 - ord c2) `mod` 256)
{-- 
import Bits						    -- ghc specific
encrypt = zipWith cxor
decrypt = zipWith cxor
cxor c1 c2 = chr (ord c1 `xor` ord c2)
--}
