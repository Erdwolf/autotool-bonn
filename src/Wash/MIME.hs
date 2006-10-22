module Wash.MIME where
-- RFC 2045
-- RFC 2046

import Array
import Hex
import IO
import Random
import Char


-- 
newtype Header = Header (String, String)
newtype KV = KV (String, String)
newtype MediaType = MediaType (String, String)
-- 

instance Show Header where
  show (Header (key, value)) = 
    if null value then "" else
    key ++ ':' : ' ' : encodeValue value ++ "\r\n"

instance Show KV where
  show (KV (key, value)) =
    key ++ '=' : value

instance Show MediaType where
  show (MediaType (ty, subty)) =
    ty ++ '/' : subty

-- 

mimeHeader = 
  Header ("MIME-Version", "1.0")

identHeader =
  Header ("X-Mailer", "MIME.hs 0.1")

makeContentType mtype subtype parameters = 
  Header ("Content-Type", mtype ++ "/" ++ subtype ++ p parameters)
  where p = concat . map p1
	p1 parameter = ';' : show parameter

makeContentTransferEncoding enc =
  Header ("Content-Transfer-Encoding", enc)

makeContentDisposition name =
  Header ("Content-Disposition", name)

makeX what recipients = 
  Header (what, l recipients)
  where l [] = []
	l [xs] = xs
	l (xs:xss) = xs ++ ", " ++ l xss

makeTO = makeX "To"
makeCC = makeX "Cc"
makeBCC = makeX "Bcc"
makeSubject s = makeX "Subject" [s]

--  ------------------------------------------------------------------

base64_alphabet_list =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

encode_base64_alphabet_index =
  zip [0 .. (63::Int)] base64_alphabet_list

decode_base64_alphabet_index =
  zip base64_alphabet_list [0 .. (63::Int)]

encode_base64_alphabet =
  array (0 :: Int, 63 :: Int) encode_base64_alphabet_index

decode_base64_alphabet =
  array (' ','z') decode_base64_alphabet_index

base64_character =
  array (chr 0, chr 255) [(c, c `elem` base64_alphabet_list || c == '=') | c <- [chr 0 .. chr 255]]

encode_base64 = linebreak 76 . encode_base64'

linebreak n [] = "\r\n"
linebreak 0 xs = '\r':'\n': linebreak 76 xs
linebreak n (x:xs) = x: linebreak (n-1) xs

encode_base64' [] = []

encode_base64' [ch] = 
  encode_base64_alphabet!b1 : 
  encode_base64_alphabet!b2 :
  "=="
  where (b1, b2, _, _) = encode_base64_group (ch, chr 0, chr 0)

encode_base64' [ch1, ch2] =
  encode_base64_alphabet!b1 : 
  encode_base64_alphabet!b2 :
  encode_base64_alphabet!b3 :
  "="
  where (b1, b2, b3, _) = encode_base64_group (ch1, ch2, chr 0)

encode_base64' (ch1: ch2: ch3: rest) =
  encode_base64_alphabet!b1 : 
  encode_base64_alphabet!b2 :
  encode_base64_alphabet!b3 :
  encode_base64_alphabet!b4 :
  encode_base64' rest
  where (b1, b2, b3, b4) = encode_base64_group (ch1, ch2, ch3)

-- 111111 112222 222233 333333
encode_base64_group (ch1, ch2, ch3) = (b1, b2, b3, b4)
  where o1 = ord ch1
	o2 = ord ch2
	o3 = ord ch3
	b1 = o1 `div` 4
	b2 = (o1 `mod` 4) * 16 + o2 `div` 16
	b3 = (o2 `mod` 16) * 4 + o3 `div` 64
	b4 = o3 `mod` 64

decode_base64_group (b1, b2, b3, b4) = (ch1, ch2, ch3)
  where ch1 = chr (b1 * 4 + b2 `div` 16)
	ch2 = chr (b2 `mod` 16 * 16 + b3 `div` 4)
	ch3 = chr (b3 `mod` 4 * 64 + b4)

decode_base64' [] = []

decode_base64' [cin1, cin2, '=', '='] = [cout1]
  where (cout1, _, _) = 
          decode_base64_group (decode_base64_alphabet!cin1
	  		      ,decode_base64_alphabet!cin2
			      ,0
			      ,0)

decode_base64' [cin1, cin2, cin3, '='] = [cout1, cout2]
  where (cout1, cout2, _) = 
          decode_base64_group (decode_base64_alphabet!cin1
	  		      ,decode_base64_alphabet!cin2
			      ,decode_base64_alphabet!cin3
			      ,0)

decode_base64' (cin1: cin2: cin3: cin4: rest) = 
  cout1: cout2: cout3: decode_base64' rest
  where (cout1, cout2, cout3) = 
          decode_base64_group (decode_base64_alphabet!cin1
	  		      ,decode_base64_alphabet!cin2
			      ,decode_base64_alphabet!cin3
			      ,decode_base64_alphabet!cin4)

decode_base64 = decode_base64' . filter (base64_character!)

--  ------------------------------------------------------------------

encode_hexadecimal c = '=' : showHex2 c

quoted_printable x = 
  ox >= 33 && ox <= 126 && ox /= 61
  where ox = ord x

end_of_line [] = True
end_of_line ('\r':'\n':_) = True
end_of_line _ = False

encode_quoted' (x:xs) | x `elem` "\t " = 
  if end_of_line xs then encode_hexadecimal (ord x) ++ encode_quoted' xs
                    else x : encode_quoted' xs
encode_quoted' (x:xs) | quoted_printable x = x : encode_quoted' xs
encode_quoted' ('\r':'\n':xs) = '\r':'\n': encode_quoted' xs
encode_quoted' (x:xs) = encode_hexadecimal (ord x) ++ encode_quoted' xs
encode_quoted' [] = ""

encode_quoted = softLineBreak 76 . encode_quoted'

softLineBreak n [] = "\r\n"
softLineBreak 0 xs | not (end_of_line xs) = '=':'\r':'\n': softLineBreak 76 xs
softLineBreak n ('\r':'\n':xs) = '\r':'\n': softLineBreak 76 xs
softLineBreak n (xs@('=':_)) | n < 4 = '=':'\r':'\n': softLineBreak 76 xs
softLineBreak n (x:xs) = x : softLineBreak (n-1) xs

decode_quoted [] = []
decode_quoted ('=':'\r':'\n':xs) =
  decode_quoted xs
decode_quoted ('=':upper:lower:xs) = 
  chr (16 * hexDigitVal upper + hexDigitVal lower) : decode_quoted xs
decode_quoted (x:xs) = 
  x : decode_quoted xs

--  ------------------------------------------------------------------
-- RFC 2047: encoding of header fields

encodeWord w = "=?" ++ charset ++ "?" ++ encoding ++ "?" ++ encode_quoted' w ++ "?="
  where encoding = "q"
	charset  = "iso-8859-1"

encodeValue v = 
  case span (not . flip elem " ()<>@.!,") v of
    ([], []) -> []
    (word, []) -> maybeEncode word
    (word, x:rest) -> maybeEncode word ++ x : encodeValue rest

maybeEncode word | all p word = word
                 | otherwise = encodeWord word
  where p x = let ox = ord x in ox >= 33 && ox <= 126

--  ------------------------------------------------------------------

textDOC subty docLines =
  DOC {	mediatype= "text",
	subtype= subty,
	textLines= docLines,
	parameters= [],
	filename= "",
	messageData="",
	parts=[]
      }

binaryDOC ty subty bindata =
  DOC {	mediatype= ty,
	subtype= subty,
	messageData= bindata,
	textLines= [],
	parameters= [],
	filename= "",
	parts=[]
      }
  
multipartDOC subdocs =
  DOC {	mediatype= "multipart",
	subtype= "mixed",
	messageData= "",
	textLines= [],
	parameters= [],
	filename= "",
	parts= subdocs
      }

data DOC =
     DOC {
	mediatype :: String,			    -- type
	subtype :: String,			    -- subtype
	parameters  :: [KV],			    -- parameters
	filename :: String,			    -- suggested filename
	-- depending on mediatype only one of the following is relevant:
	messageData :: String,			    -- data
	textLines :: [String],			    -- lines
	parts :: [DOC]				    -- data
	}

recommend_cte h doc = 
  case mediatype doc of
    "text" -> 
       case sendMode h of
         SevenBit -> "quoted-printable"
	 EightBit -> "8bit"
    "multipart" -> "7bit"
    _ ->
      case sendMode h of 
	SevenBit -> "base64"
	EightBit -> "8bit"

inventBoundary =
  inventKey 60 (init base64_alphabet_list)
  where
    inventKey len chars =
      do g <- getStdGen
	 let candidate = take len $ map (chars !!) $ randomRs (0, length chars - 1) g
	 return candidate

data SendMode =
  EightBit | SevenBit
data SendControl = 
  SendControl {
    sendH :: Handle,
    sendMode :: SendMode
  }

smtpSendControl = 
  SendControl { sendH = stdout, sendMode = SevenBit }

httpSendControl =
  SendControl { sendH = stdout, sendMode = EightBit }

instance Send DOC where
  hSend h doc =
    let cte = recommend_cte h doc in
    do boundary <- inventBoundary
       let extraParameter = 
	     if mediatype doc == "multipart" 
	     then [KV ("boundary", boundary)]
	     else []
       hSend h (makeContentType (mediatype doc)
       			     (subtype doc) 
       			     (extraParameter ++ parameters doc))
       hSend h (makeContentTransferEncoding cte)
       hSend h (makeContentDisposition (filename doc))
       hSend h CRLF
       case mediatype doc of 
	 "text" -> hSendText h doc
	 "multipart" -> hSendMultipart h boundary doc
	 _ -> hSendBinary h doc

hSendText h doc =
  case sendMode h of
    EightBit -> 
      hPutStr hdl str
    SevenBit ->
      hPutStr hdl (encode_quoted str)
  where hdl = sendH h
	str = flat (textLines doc)
	flat [] = []
	flat (xs:xss) = xs ++ "\r\n" ++ flat xss

hSendBinary h doc =
  case sendMode h of
    SevenBit ->
      hPutStr (sendH h) (encode_base64 (messageData doc))
    EightBit ->
      hPutStr (sendH h) (messageData doc)

hSendMultipart h boundary doc =
  do hSendText h doc
     sendParts (parts doc)
  where hdl = sendH h
	sendParts [] = 
	  do hPutStr hdl "--"
	     hPutStr hdl boundary
	     hPutStr hdl "--"
	     hSend h CRLF
	sendParts (doc:docs) =
	  do hPutStr hdl "--"
	     hPutStr hdl boundary
	     hSend h CRLF
	     hSend h doc
	     sendParts docs

data CRLF = CRLF

instance Send CRLF where
  hSend h CRLF = hPutStr (sendH h) "\n"

data Mail =
     Mail {
	to :: [String],
	subject :: String,
	cc :: [String],
	bcc :: [String],
	headers :: [Header],
	contents :: DOC
        }

simpleMail recipients subj doc =
  Mail { to= recipients, subject= subj, cc=[], bcc=[], headers=[], contents=doc }

class Send m where
  send :: m -> IO ()
  hSend :: SendControl -> m -> IO ()
  send = hSend smtpSendControl

instance Send Header where
  hSend h header = hPutStr (sendH h) (show header)

instance Send Mail where
  hSend h mail =
    do hSend h (makeTO (to mail))
       hSend h (makeSubject (subject mail))
       hSend h (makeCC (cc mail))
       hSend h (makeBCC (bcc mail))
       hSend h mimeHeader
       hSend h identHeader
       sequence (map (hSend h) (headers mail))
       hSend h (contents mail)

--

example = 
  do bs <- readFile "/home/thiemann/lib/pictures/rijbewijs.jpg"
     let doc1 = DOC { mediatype="image",
	 	      subtype="jpeg",
		      parameters=[],
		      filename="rijbewijs.jpg",
		      messageData=bs }
	 doc2 = DOC { mediatype="text",
	 	      subtype="plain",
		      parameters=[],
		      filename="",
		      textLines=["Hallo, ihr daheim!   ", "diridiridideldö    ", "x=y und auch !\"§$%&/()§$%&/()=?"]}
	 doc = DOC {  mediatype="multipart",
	 	      subtype="mixed",
		      parameters=[],
		      filename="",
		      textLines=[],
		      parts=[doc1,doc2]}
     return Mail {    to=["thiemann"],
     		      subject="Halloween",
		      cc=[],
		      bcc=[],
		      headers=[], 
		      contents=doc }


example2 = 
  do bs <- readFile "/home/thiemann/lib/pictures/rijbewijs.jpg"
     let doc1 = (binaryDOC "image" "jpeg" bs) { filename="rijbewijs.jpg" }
	 doc2 = textDOC "plain" ["Hallo, ihr daheim!   "
	 			,"diridiridideldö    "
				,"x=y und auch !\"§$%&/()§$%&/()=?"]
	 doc = multipartDOC [doc2,doc1]
     return (simpleMail ["thiemann"] "Halloween" doc)

