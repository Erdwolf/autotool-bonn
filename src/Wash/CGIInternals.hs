-- © 2001 Peter Thiemann
module Wash.CGIInternals (module Wash.CGIInternals, getPathInfo)
{-- interface
  (ask		-- WithHTML CGI a -> CGI ()
  ,tell		-- CGIOutput a => a -> CGI ()
  ,htell 	-- WithHTML IO () -> CGI ()
  ,run		-- CGI () -> IO ()
  ,runWithHook 	-- ([String] -> CGI ()) -> CGI () -> IO ()
  )
--}
where

import Prelude hiding (head,div,span)
import qualified Prelude
import qualified List
import Maybe
import Control.Monad
import System

import Wash.HTMLWrapper hiding (map)
import Wash.CGIMonad
import Wash.CGIOutput
import Wash.RawCGI
import Wash.Fields
import IO

import Wash.Config

-- 
io :: (Read a, Show a) => IO a -> CGI a
io ioa =
  once (unsafe_io ioa)

once :: (Read a, Show a) => CGI a -> CGI a
once cgi = 
  CGI (\ cgistate ->
  case inparm cgistate of
    PAR_RESULT code : rest ->
      return (read code
	     ,cgistate { inparm = rest })
    [] ->
      unCGI cgi cgistate { outparm = PAR_MARK : outparm cgistate } >>= finish
    PAR_MARK : rest -> 
      unCGI cgi cgistate { inparm = rest } >>= finish
    _ -> 
      reportError "Out of sync" empty cgistate)
  where popToMark [] = []
	popToMark (PAR_MARK: rest) = rest
	popToMark (_: rest) = popToMark rest
	finish (v, cgistate') =
 	  let out = outparm cgistate' in
	  return (v
	      	 ,cgistate' { outparm = PAR_RESULT (show v) : popToMark out })

--
unsafe_io :: IO a -> CGI a
unsafe_io ioa =
  CGI $ \cgistate -> ioa >>= \a -> return (a, cgistate)

-- 
ask :: WithHTML CGI a -> CGI ()
ask ma =
  do CGI (\cgistate -> return ((), cgistate { pageInfo = initialPageInfo cgistate }))
     elem <- build_document ma 
     CGI $
       \cgistate ->
       unCGI (nextaction (pageInfo cgistate) elem) (nextCGIState cgistate)

-- 
askOffline :: WithHTML CGI a -> (Element -> IO ()) -> CGI ()
askOffline ma elementAction =
  do CGI (\cgistate -> return ((), cgistate { pageInfo = initialPageInfo cgistate }))
     elem <- build_document ma 
     CGI $
       \cgistate ->
       case bindings (pageInfo cgistate) of
         Nothing ->
	   elementAction elem >> return ((), cgistate)
	 Just _ ->
	   unCGI (nextaction (pageInfo cgistate) elem) (nextCGIState cgistate)

-- 
run :: CGIOptions -> CGI () -> IO ()
run options =
  runInternal options (fallbackTranslator Nothing)

runWithHook :: CGIOptions -> ([String] -> CGI ()) -> CGI () -> IO ()
runWithHook options cgigen =
  runInternal options (fallbackTranslator (Just cgigen))

runInternal options cgigen (CGI cgi) =
  start options $ \ info decoded_parameters ->
  let maybecgiparm = assocParm "=CGI=parm=" decoded_parameters in
  do oldparm <- case maybecgiparm of
       Just cgiparm ->
         liftM read $ decode (urlDecode cgiparm)
       Nothing -> 
         return []
     key <- generateKey
     let newparm 
          | null decoded_parameters = []
	  | otherwise = (PAR_VALUES $ dropSpecialParameters decoded_parameters)
	  		: oldparm
	 cgistate = CGIState { inparm = reverse newparm
		             , outparm = newparm
			     , cgiInfo = info
			     , pageInfo = initialPageInfo cgistate
			     , encoder = makeEncoder key
			     , cookieMap = map decodeCookie (cgiCookies info)
			     , cookiesToSend = []
			     }
     -- writeFile "/tmp/CGIOLDPARM" (show oldparm)
     -- writeFile "/tmp/CGINEWPARM" (show newparm)
     args <- getArgs
     if null args
	 then cgi cgistate >> return ()
	 else unCGI (cgigen args) cgistate >> return ()
     exitWith ExitSuccess

initialPageInfo cgistate =
  let bnds = do PAR_VALUES parms <- listToMaybe (inparm cgistate)
		return parms
  in  PageInfo
  	{ count = 0
	, vcount = 0
	, nextaction = case bnds of 
		       Nothing -> tell
		       Just _ -> tellError "Unspecified action"
	, bindings = bnds
	, enctype = contentType cgistate
	, faultyfields = []
	}

nextCGIState cgistate = cgistate'
  where cgistate' = cgistate { inparm = drop 1 (inparm cgistate)
      			     , pageInfo = initialPageInfo cgistate'
			     }

-- ======================================================================
-- internal references

makeRef :: Monad m => String -> WithHTML m () -> CGI (WithHTML m ())
makeRef fileName elems =
  CGI (\cgistate ->
  let fileURL = url cgistate ++ '?' : fileName in
  return (hlink fileURL elems, cgistate))

-- ======================================================================
-- input fields & forms

data InputField a x = 
     InputField { ifName :: String
     		, ifFty :: String
		, ifString :: Maybe String
		, ifValue :: Maybe a
		, ifRaw :: CGIParameters
		, ifBound :: Bool	    -- True if form submitted
		}

-- transition code
name = ifName
fty = ifFty
string = ifString
-- value = ifValue
raw = ifRaw
-- transition code end

data VALID = VALID
data INVALID = INVALID

class HasValue i where
  value :: i a VALID -> a

instance HasValue InputField where
  value inf = fromJust (ifValue inf)

feither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
feither f g (Left a) = Left (f a)
feither f g (Right b) = Right (g b)

propagate :: Either a [String] -> Either b [String] -> Either (a,b) [String]
propagate (Left a) (Left b) = Left (a, b)
propagate (Left a) (Right bss) = Right bss
propagate (Right ass) (Left b) = Right ass
propagate (Right ass) (Right bss) = Right (ass++bss)

class InputHandle h where
  validate :: h INVALID -> Either (h VALID) [String]

instance InputHandle (InputField a) where
  validate inf =
    case ifValue inf of 
      Nothing | ifBound inf -> 
        Right [ifName inf]
      _ ->
        Left InputField { ifName = ifName inf
			, ifFty  = ifFty inf
			, ifString = ifString inf
			, ifValue = ifValue inf
			, ifRaw = ifRaw inf
			, ifBound = ifBound inf
			}

data F0 x = F0

instance InputHandle F0 where
  validate F0 = Left F0

data F1 a x = F1 (a x)

instance InputHandle a => InputHandle (F1 a) where
  validate (F1 ainv) =
    feither F1 id (validate ainv)

data F2 a b x = F2 (a x) (b x)

instance (InputHandle a, InputHandle b) => InputHandle (F2 a b) where
  validate (F2 ainv binv) =
    feither (uncurry F2) id
    (propagate (validate ainv) (validate binv))

data F3 a b c x = F3 (a x) (b x) (c x)

instance (InputHandle a, InputHandle b, InputHandle c) => InputHandle (F3 a b c) where
  validate (F3 ainv binv cinv) =
    feither (\ (aval,(bval,cval)) -> F3 aval bval cval) id
    (propagate (validate ainv)
     (propagate (validate binv) (validate cinv)))

data F4 a b c d x = F4 (a x) (b x) (c x) (d x)

instance (InputHandle a, InputHandle b, InputHandle c, InputHandle d)
       => InputHandle (F4 a b c d) where
  validate (F4 ainv binv cinv dinv) =
    feither (\ (aval,(bval,(cval,dval))) -> F4 aval bval cval dval) id
    (propagate (validate ainv) 
     (propagate (validate binv)
      (propagate (validate cinv) (validate dinv))))

data F5 a b c d e x = F5 (a x) (b x) (c x) (d x) (e x)

instance (InputHandle a, InputHandle b, InputHandle c, InputHandle d, InputHandle e)
       => InputHandle (F5 a b c d e) where
  validate (F5 ainv binv cinv dinv einv) =
    feither (\ (aval,(bval,(cval,(dval,eval)))) -> F5 aval bval cval dval eval) id
    (propagate (validate ainv) 
     (propagate (validate binv)
      (propagate (validate cinv)
       (propagate (validate dinv) (validate einv)))))

data F6 a b c d e f x = F6 (a x) (b x) (c x) (d x) (e x) (f x)

instance (InputHandle a, InputHandle b, InputHandle c, InputHandle d, InputHandle e, InputHandle f)
       => InputHandle (F6 a b c d e f) where
  validate (F6 ainv binv cinv dinv einv finv) =
    feither (\ (aval,(bval,(cval,(dval,(eval, fval))))) -> F6 aval bval cval dval eval fval) id
    (propagate (validate ainv) 
     (propagate (validate binv)
      (propagate (validate cinv)
       (propagate (validate dinv) 
        (propagate (validate einv) (validate finv))))))

data F8 a b c d e f g h x = F8 (a x) (b x) (c x) (d x) (e x) (f x) (g x) (h x)

instance (InputHandle a, InputHandle b, InputHandle c, InputHandle d, InputHandle e, InputHandle f, InputHandle g, InputHandle h)
       => InputHandle (F8 a b c d e f g h) where
  validate (F8 ainv binv cinv dinv einv finv ginv hinv) =
    feither (\ (aval,(bval,(cval,(dval,(eval, (fval, (gval, hval))))))) -> F8 aval bval cval dval eval fval gval hval) id
    (propagate (validate ainv) 
     (propagate (validate binv)
      (propagate (validate cinv)
       (propagate (validate dinv) 
        (propagate (validate einv) 
	 (propagate (validate finv)
	  (propagate (validate ginv) (validate hinv))))))))

data FL a x = FL [a x]

instance InputHandle a => InputHandle (FL a) where
  validate (FL ainvs) =
    g (map validate ainvs)		 -- [Either (h VALID) [String]]
    where 
    g = foldr h (Left (FL []))
    h ev evs = feither (\ (v, FL vs) -> FL (v : vs)) id (propagate ev evs)
	
data FA a b x = FA a (b x)

instance InputHandle b => InputHandle (FA a b) where
  validate (FA a binv) =
    feither (FA a) id (validate binv)

submit :: InputHandle h => h INVALID -> (h VALID -> CGI ()) -> HTMLField ()
submit = submitInternal False

defaultSubmit :: InputHandle h => h INVALID -> (h VALID -> CGI ()) -> HTMLField ()
defaultSubmit = submitInternal True

submitInternal isDefault hinv g =
  case validate hinv of
    Left hval ->
      internalSubmitField isDefault (Left (g hval))
    Right ss ->
      internalSubmitField isDefault (Right ss)

-- internal
data InputType = TEXT | PASSWORD | CHECKBOX |
    RADIO | SUBMIT | RESET |
    FILE | HIDDEN | IMAGE | BUTTON
    deriving (Eq)

instance Show InputType where
  show TEXT = "text"
  show PASSWORD = "password"
  show CHECKBOX = "checkbox"
  show RADIO = "radio"
  show SUBMIT = "submit"
  show RESET = "reset"
  show FILE = "file"
  show HIDDEN = "hidden"
  show IMAGE = "image"
  show BUTTON = "button"

textual :: InputType -> Bool
textual TEXT = True
textual PASSWORD = True
textual _ = False

type HTMLField a = WithHTML CGI () -> WithHTML CGI a

resetField :: HTMLField (InputField () INVALID)
resetField = 
  genericField RESET (const (Just ()))

submitField :: CGI () -> HTMLField ()
submitField action =
  internalSubmitField False (Left action)

defaultSubmitField :: CGI () -> HTMLField ()
defaultSubmitField action =
  internalSubmitField True (Left action)

internalSubmitField isDefault what attrs =
  do sf <- genericField SUBMIT (const (Just ())) attrs
     pageInfo <- lift getInfo
     let mbnds = bindings pageInfo
	 localAction = 
	   case what of
	     Left action ->
	       lift (setAction (const action))
	     Right ss ->
	       do lift (setAction tell)			    -- CHANGE THIS
		  lift (setFaulty ss)
		  lift (CGI (\ cgistate -> 
		  	return ( ()
			       , cgistate { outparm = tail (outparm cgistate)})))
     case do bnds <- maybeToList mbnds
	     's' : fns <-  fieldNames bnds
	     return ()
          of
       [] | isDefault && isJust mbnds -> 
         localAction
       _ ->
         case mbnds >>= assocParm (name sf) of
           Nothing ->
             return ()
	   Just _ ->
             localAction

activeInputField :: (Reason a, Read a) => (a -> CGI ()) -> HTMLField ()
activeInputField actionFun attrs =
  activate actionFun inputField attrs

-- BUG:
-- (in-) validates input even if form was *not* submitted by changing this field
-- potential fix: check for an s## binding and don't action if present
activate :: (a -> CGI ()) -> HTMLField (InputField a INVALID) -> HTMLField ()
activate actionFun inputField attrs =
  do invalid_inf <- inputField (attrs ## attr "onchange" "this.form.submit();")
     pageInfo <- lift getInfo
     when (ifBound invalid_inf) $
       case validate invalid_inf of
         Left valid_inf ->
	   lift (setAction (const $ actionFun $ value valid_inf))
	 Right ss ->
	   do lift (setAction tell)		    -- CHANGE THIS
	      lift (setFaulty ss)
	      lift ((CGI (\ cgistate -> 
	             return ( ()
		            , cgistate { outparm = tail (outparm cgistate)}))))
     return ()


inputField :: (Reason a, Read a) => HTMLField (InputField a INVALID)
inputField = 
  genericField TEXT (maybeRead . fromMaybe "")

maybeRead :: Read a => String -> Maybe a
maybeRead s = g (reads s)
  where g ((a,""):_) = Just a
	g _ = Nothing

textInputField :: HTMLField (InputField String INVALID)
textInputField =
  genericField TEXT id

checkedTextInputField :: (Maybe String -> Maybe String) -> HTMLField (InputField String INVALID)
checkedTextInputField g attrs =
  genericField TEXT g attrs

passwordInputField :: (Reason a, Read a) => HTMLField (InputField a INVALID)
passwordInputField =
  genericField PASSWORD (maybeRead . fromMaybe "")

checkboxInputField :: HTMLField (InputField Bool INVALID)
checkboxInputField =
  genericField CHECKBOX g
  where g Nothing = Just False
	g (Just _) = Just True

fileInputField :: HTMLField (InputField FileReference INVALID)
fileInputField attrs =
  lift (setEnctype "multipart/form-data") >>
  genericField FILE (maybeRead . fromMaybe "") attrs

instance Reason FileReference where
  reason _ = "FileReference"

genericField :: Reason a => InputType -> (Maybe String -> Maybe a) -> HTMLField (InputField a INVALID)
genericField inputType decode fieldAttrs =
  do fieldName' <- lift nextName
     let fieldName | inputType == SUBMIT = 's' : tail fieldName' 
	 	   | otherwise = fieldName'
	 fieldType = show inputType
     info <- lift getInfo
     let bds =  bindings info
	 maybestring = bds >>= assocParm fieldName
	 rawvalues = maybeToList bds >>= assocParmR fieldName
	 decoded = decode maybestring
	 isBound = isJust bds
     input (fieldAttrs ##
	    attr "type" fieldType ##
            attr "name" fieldName ##
	    case maybestring of 
	      Nothing -> empty
	      Just str -> attr "value" str)
     baseUrl <- lift getUrl
     when (textual inputType) $
       makeImage nothing (reason (fromJust decoded) ++ " expected")
         (attr "align" "center" ## 
          attr "name" ('i' : fieldName))
     return $
       InputField { ifName = fieldName
     		  , ifFty = fieldType
		  , ifString = maybestring
		  , ifValue = decoded
		  , ifRaw = rawvalues
		  , ifBound = isBound
		  }

imageField :: URL -> HTMLField (InputField (Int, Int) INVALID)
imageField url fieldAttrs =
  do fieldName <- lift nextName
     input (attr "type" "image" ##
     	    attr "name" fieldName ##
	    attr "src" url ## 
	    fieldAttrs)
     info <- lift getInfo
     return $
       let maybe_xy = 
	    do bds <- bindings info
	       x <- assocParm (fieldName ++ ".x") bds
	       y <- assocParm (fieldName ++ ".y") bds
	       return (x, y)
       in
       InputField { ifName = fieldName
       		  , ifFty = "image"
		  , ifString = do (x, y) <- maybe_xy
				  return ("(" ++ x ++ "," ++ y ++ ")")
		  , ifValue  = do (x, y) <- maybe_xy
				  return (read x, read y)
     		  , ifRaw = []
		  , ifBound = isJust (bindings info)
		  }

-- a virtual field that never appears on the screen
data RadioGroup a x =
     RadioGroup { radioName   :: String
     		, radioAttrs   :: WithHTML CGI ()
                , radioString :: Maybe String
		, radioValue  :: Maybe a
		, radioBound  :: Bool
		}

instance HasValue RadioGroup where
  value rg = fromJust (radioValue rg)

instance InputHandle (RadioGroup a) where
  validate rg =
    case radioValue rg of 
      Nothing | radioBound rg -> 
        Right [radioName rg]
      _ ->
        Left RadioGroup { radioName = radioName rg
			, radioString = radioString rg
			, radioValue = radioValue rg
			, radioAttrs = radioAttrs rg
			, radioBound = radioBound rg
			}

radioGroup :: Read a => HTMLField (RadioGroup a INVALID)
radioGroup attrs =
  do fieldName <- lift nextName
     info <- lift getInfo
     let bds = bindings info
	 maybeString = bds >>= assocParm fieldName
	 maybeVal = maybeString >>= (g . reads . urlDecode)
	 g ((a,""):_) = Just a
	 g _ = Nothing
     return $
       RadioGroup { radioName = fieldName
       		  , radioAttrs = attrs
		  , radioString = maybeString
		  , radioValue = maybeVal
		  , radioBound = isJust bds
		  }

radioButton :: Show a => RadioGroup a INVALID -> a -> HTMLField ()
radioButton rg val fieldAttrs =
  input (do attr "type" "radio"
            attr "name" (radioName rg)
	    attr "value" (urlEncode (show val)) 
	    fieldAttrs
	    radioAttrs rg)

radioError :: RadioGroup a INVALID -> WithHTML CGI ()
radioError rg = 
  let name = radioName rg in
  do baseUrl <- lift getUrl
     img (attr "align" "center" ## 
          attr "name" ('i' : name) ##
     	  attr "src" (baseUrl ++ "?" ++ ffName nothing) ##
	  attr "alt" "Select exactly one button")

-- buttons

makeButton :: HTMLField (InputField Bool INVALID)
makeButton fieldAttrs =
  let fieldType = "button" in
  do fieldName <- lift nextName
     input (do attr "type" fieldType
	       attr "name" fieldName 
	       fieldAttrs)
     info <- lift getInfo
     let bds = bindings info
	 maybeString = bds >>= assocParm fieldName
	 maybeVal = 
	   case bds of
	     Nothing -> Nothing
	     Just parms ->
	       case maybeString of
	         Nothing -> Just False
		 Just _  -> Just True
     return $
       InputField { ifName = fieldName
     		  , ifFty = fieldType
		  , ifString = maybeString
		  , ifValue = maybeVal
		  , ifRaw = []
		  , ifBound = isJust bds
		  }

-- form

makeForm :: WithHTML CGI a -> WithHTML CGI ()
makeForm attrs_elems =
  lift getUrl >>= \url ->
  form $
  do name0 <- lift nextName
     attr "action" url
     attr "method" "POST"
     attrs_elems
     name <- lift nextName
     info <- lift getInfo
     parm <- lift getParm
     encoder <- lift getEncoder
     input (attr "type" "hidden" ##
            attr "name" "=CGI=parm=" ##
	    attr "value" (urlEncode $ encoder $ show $ parm))
     let name' = 'f' : show ((read (tail name0) :: Int) + 1)
	 hilight | name' == name = []
	   | otherwise = ["document." ++ name ++ "." ++ name' ++ ".focus();"
	 		 ,"document." ++ name ++ "." ++ name' ++ ".select();"]
	 jsprog = map (\fname -> 
		       "document.i" ++ fname ++ ".src = '" ++ url ++ "?" ++ ffName question ++ "';")
	 	      (faultyfields info)
		++ hilight
     script (attr "language" "JavaScript" ##
             rawtext ("<!-- Faultyfields: " ++ show (faultyfields info)) ##
             rawtext ('\n' : unlines jsprog) ##
	     rawtext "// -->")
     attr "name" name
     attr "enctype" (enctype info)

-- textarea

makeTextarea :: String -> HTMLField (InputField String INVALID)
makeTextarea fieldValue fieldAttrs =
  do name <- lift nextName
     textarea (attr "name" name ## text fieldValue ## fieldAttrs)
     info <- lift getInfo
     let bds = bindings info
	 mvalue = bds >>= assocParm name
     return $
       InputField { ifName = name
       		  , ifFty = "textarea"
		  , ifString = mvalue
		  , ifValue = mvalue
		  , ifRaw = maybeToList bds >>= assocParmR name
		  , ifBound = isJust bds
		  }

-- select

selectTags = map (('o':) . show) [(1::Int)..] 

selectMultiple :: Eq a => (a -> String) -> [a] -> [a] -> (Int, Int)
	       -> HTMLField (InputField [a] INVALID)
selectMultiple shw defs opts (minSel, maxSel) attrs =
  do name <- lift nextName
     info <- lift getInfo
     let bds = bindings info
	 rawvalues = maybeToList bds >>= assocParmR name
	 g ('o':i:rest) = i /= '0' 
	 g _ = False
	 inputs = filter g $ map fieldContents rawvalues
	 values = inputs >>=
		  (maybeToList . flip List.elemIndex selectTags) >>=
		  (\i -> [opts !! i])
	 len = length inputs
	 mvalue | minSel <= len && len <= maxSel = Just values
	        | otherwise = Nothing
	 revisedDefaults | isJust bds && not (null inputs) = values
	 		 | otherwise = defs
     let makeoption (opt, tag) = 
	   option (do text (shw opt)
		      attr "value" tag
		      when (opt `elem` revisedDefaults) $
		        attr "selected" "selected")
	 makeChoice :: Int -> String
	 makeChoice n | n == 0 = "no choice"
                   | n == 1 = "1 choice"
		   | n == maxBound = "arbitrary many choices"
		   | otherwise = show n ++ " choices"
	 makeRange :: Int -> Int -> String
	 makeRange lo hi | lo == maxBound = "Arbitrary many choices"
                      | lo == hi = "Exactly " ++ makeChoice lo
                      | otherwise = "Minimum " ++ makeChoice lo ++
		      		    "; maximum " ++ makeChoice hi
     select (do attrs
		attr "name" name
		when (maxSel > 1) $
		  attr "multiple" "multiple"
		when (null defs && minSel > 0) $
		  option (text "--" ## attr "value" "o0")
		mapM_ makeoption (zip opts selectTags))
     baseUrl <- lift getUrl
     img (attr "align" "center" ## 
          attr "name" ('i' : name) ##
     	  attr "src" (baseUrl ++ "?" ++ ffName nothing) ##
	  attr "alt" (makeRange minSel maxSel))
     return $ 
       InputField { ifName = name
       		  , ifFty = "select"
		  , ifString = Nothing -- fmap show value
		  , ifValue = mvalue
		  , ifRaw = rawvalues
		  , ifBound = isJust bds
		  }

selectSingle :: Eq a => (a -> String) -> Maybe a -> [a]
	       -> HTMLField (InputField a INVALID)
selectSingle shw mdef opts attrs =
  do inf <- selectMultiple shw (maybeToList mdef) opts (1,1) attrs
     return $
       InputField { ifName = ifName inf
       		  , ifFty = "select"
		  , ifString = ifString inf
		  , ifValue = fmap Prelude.head (ifValue inf)
		  , ifRaw = ifRaw inf
		  , ifBound = ifBound inf
		  }

selectBounded :: (Enum a, Bounded a, Read a, Show a, Eq a) =>
		 Maybe a -> HTMLField (InputField a INVALID)
selectBounded def =
  selectSingle show def [minBound..maxBound]
     
-- ======================================================================
-- attributes


fieldSIZE :: Monad m => Int -> WithHTML m ()
fieldSIZE i = attr "size" (show i)

fieldMAXLENGTH :: Monad m => Int -> WithHTML m ()
fieldMAXLENGTH i = attr "maxlength" (show i)
 

fieldVALUE :: (Monad m, Show a) => a -> WithHTML m ()
fieldVALUE a =
  attr "value" (show a)

-- ======================================================================
-- output routines

decodeCookie :: (String, String) -> (String, (Maybe String, Maybe String))
encodeCookie :: (String, (Maybe String, Maybe String)) -> (String, String)
encodeCookie (k, (v, mexp)) =
  (urlEncode k, 
   case v of 
     Just v' -> urlEncode v' ++
       (case mexp of
	  Nothing -> ""
	  Just exp -> "; expires=" ++ exp)
     Nothing -> "deleted; expires=Thu, 01-Jan-1970 00:00:00 GMT")
decodeCookie (k, v) =  (urlDecode k, (Just (urlDecode v), Nothing))

putCookies :: CGIState -> IO ()
putCookies cgistate =
  let cookies = cookiesToSend cgistate
      cm = cookieMap cgistate
      sendCookie name = case lookup name cm of
        Nothing -> 
	  return ()
	Just value -> 
	  let (encName, encValue) = encodeCookie (name, value) in
	  do putStr "Set-Cookie: "
	     putStr encName
	     putStr "="
	     putStr encValue
	     putStrLn ";"
  in mapM_ sendCookie cookies

tell :: CGIOutput a => a -> CGI ()
tell a =
  CGI (\cgistate -> 
  do putCookies cgistate
     cgiPut a
     exitWith ExitSuccess)

-- 
htell :: WithHTML IO () -> CGI a
htell hma =
  CGI (\cgistate ->
  do putCookies cgistate
     itell hma)
     -- never reached


-- Images

-- image from FreeForm data
makeImage ff alttext attrs = 
  do baseUrl <- lift getUrl
     img (attr "src" (baseUrl ++ '?' : ffName ff)
       ## attr "alt" alttext
       ## attrs)

rawSmallNothing = "GIF89a\SOH\NUL\SOH\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\216!\249\EOT\SOH\NUL\NUL\NUL\NUL,\NUL\NUL\NUL\NUL\SOH\NUL\SOH\NUL\NUL\STX\STXD\SOH\NUL;"
rawNothing = "GIF89a\DLE\NUL\DLE\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL!\249\EOT\SOH\NUL\NUL\NUL\NUL,\NUL\NUL\NUL\NUL\DLE\NUL\DLE\NUL\NUL\STX\SO\132\143\169\203\237\SI\163\156\180\218\139\179>\ENQ\NUL;"

rawQuestion = "GIF89a\DLE\NUL\DLE\NUL\128\NUL\NUL\NUL\NUL\NUL\255\NUL\NUL!\249\EOT\SOH\NUL\NUL\NUL\NUL,\NUL\NUL\NUL\NUL\DLE\NUL\DLE\NUL\NUL\STX#\132\GS\EM\199\167\191\SUBP\140\182$\177\204\250\194\238\129\155\181Ub9E\136\SUB~\229\199\162\242\140\146\178}\DC4\NUL;"

nothing  = FreeForm "nothing.gif"  "image/gif" rawNothing
question = FreeForm "question.gif" "image/gif" rawQuestion
ffName (FreeForm name _ _) = name

fallbackTranslator mCgigen =
  docTranslator [nothing, question] (nextTranslator mCgigen)

docTranslator docs next [name] =
  let f (doc@(FreeForm myname ct str) : rest) =
        if name == myname then tell doc else f rest
      f [] = next [name]
  in  f docs
docTranslator docs next strs =
  next strs

lastTranslator =
  nextTranslator Nothing

nextTranslator Nothing _ =
  tell (Status 404 "Not Found" Nothing)
nextTranslator (Just cgigen) strs =
  cgigen strs

-- 
reportError :: String -> WithHTML IO () -> CGIState -> IO (a, CGIState)
reportError ttl elems cgistate =
  unCGI (htell message) cgistate
  -- never reached
  where message = 
	  standardPage ttl (elems ## backLink)

backLink :: Monad m => WithHTML m ()
backLink =
  hlink "javascript:back()" (text "Try again...")

hlink :: Monad m => URL -> HTMLCons m ()
hlink url subs = a (attr "href" url ## subs)

standardQuery ttl elems =
  ask (standardPage ttl (makeForm elems))

tellError :: String -> Element -> CGI a
tellError str elems = 
  htell message 
  where message = 
	  standardPage str backLink
-- 
debug message = unsafe_io $
  do putStrLn "content-type: text/plain"
     putStrLn ""
     putStrLn message
     putStrLn "------------------------------------------------------------"
