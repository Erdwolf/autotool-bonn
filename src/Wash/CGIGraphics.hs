-- © 2001 Peter Thiemann
module Wash.CGIGraphics 
  (activeImage
  ,newImage
  ,drawOval, fillOval
  ,drawRectangle, fillRectangle
  ,drawLine, drawPoint
  ,makeText
  ,gifImage, xwdImage, xpmImage, bmpImage
  ,activateImage, activateXY, activateColor
  ,overlay
  ,storeDirectory
  ,Pixel, CGIImage, ActionFun) 
  where

import Wash.Hex
import Wash.RawCGI
import Wash.CGIInternals
import Wash.CGIMonad
import Wash.PPM
import Wash.HTMLMonad
import Wash.Unique
import Data.Maybe
import System

import Wash.Config

storeDirectory = imageDir

ppmtogif = pbmPath ++ "ppmtogif "
pbmtext  = pbmPath ++ "pbmtext "
pnmcrop  = pbmPath ++ "pnmcrop "
pbmtopgm = pbmPath ++ "pbmtopgm 1 1 "
pgmtoppm = pbmPath ++ "pgmtoppm "


type CGIImage  = IO CGIImage'
type ActionFun = Int -> Int -> Maybe (CGI ())
data CGIImage' = 
     CGIImage'{	pixmap    :: Pixmap
              , actionFun :: ActionFun
	      }


activeImage :: CGIImage -> WithHTML CGI ()
activeImage img =
  do fieldName <- lift nextName
     (url, actFun) <- lift (flushImage img)
     input (attr "type" "image" ##
            attr "name" fieldName ##
	    attr "src" url)
     info <- lift getInfo
     let maybeSetAction = 
	    do bds <- bindings info
	       x <- assocParm (fieldName ++ ".x") bds
	       y <- assocParm (fieldName ++ ".y") bds
	       act <- actFun (read x) (read y)
	       return (lift $ setAction (const act))
     fromMaybe (return ()) maybeSetAction

-- internal
flushImage :: CGIImage -> CGI (URL, ActionFun)
flushImage img = 
  do cgi_image <- unsafe_io img
     pageInfo <- getInfo
     baseUrl <- getUrl
     case bindings pageInfo of
       Nothing ->
	 unsafe_io $ do
	   key <- inventStdKey
           let path = storeDirectory ++ key
	       ppmPath = path ++ ".ppm"
	       gifPath = path ++ ".gif"
           writeFile ppmPath (show $ pixmap cgi_image)
           system (ppmtogif ++ ppmPath ++ " > " ++ gifPath)
           return (baseUrl ++ '?' : key ++ ".gif", actionFun cgi_image)
       Just _ ->
	 return ("", actionFun cgi_image)

newImage :: (Int, Int) -> Pixel -> CGIImage
newImage (w, h) bg =
  return $ CGIImage' 
  	{ pixmap = create w h 8 bg
	, actionFun = const (const Nothing)
	}

makeOval :: Bool -> CGIImage -> (Int, Int) -> (Int, Int) -> Pixel -> CGIImage
drawOval :: CGIImage -> (Int, Int) -> (Int, Int) -> Pixel -> CGIImage
fillOval :: CGIImage -> (Int, Int) -> (Int, Int) -> Pixel -> CGIImage
drawOval = makeOval False
fillOval = makeOval True
makeOval fill img ul lr p =
  do cgi_image <- img
     return $ cgi_image { pixmap = oval (pixmap cgi_image) ul lr p fill }

makeRectangle :: Bool -> CGIImage -> (Int, Int) -> (Int, Int) -> Pixel -> CGIImage
drawRectangle :: CGIImage -> (Int, Int) -> (Int, Int) -> Pixel -> CGIImage
fillRectangle :: CGIImage -> (Int, Int) -> (Int, Int) -> Pixel -> CGIImage
drawRectangle = makeRectangle False
fillRectangle = makeRectangle True
makeRectangle fill img ul lr p =
  do cgi_image <- img
     return $ cgi_image { pixmap = rectangle (pixmap cgi_image) ul lr p fill }

drawLine :: CGIImage -> (Int, Int) -> (Int, Int) -> Pixel -> CGIImage
drawLine img ul lr p =
  do cgi_image <- img
     return $ cgi_image { pixmap = line (pixmap cgi_image) ul lr p }

drawPoint :: CGIImage -> (Int, Int) -> Pixel -> CGIImage
drawPoint img xy p =
  do cgi_image <- img
     return $ cgi_image { pixmap = point (pixmap cgi_image) xy p }

makeText :: String -> Pixel -> CGIImage
makeText str fg =
  do key <- inventStdKey
     let path = storeDirectory ++ key
	 ppmPath = path ++ ".ppm"
	 gifPath = path ++ ".gif"
     system (pbmtext ++ "'" ++ str ++ "' | "
          ++ pnmcrop ++ "| "
	  ++ pbmtopgm ++ "| "
	  ++ pgmtoppm ++ rgb fg ++ "-black > " ++ ppmPath)
     pixmapStr <- readFile ppmPath
     return $ CGIImage'
        { pixmap = read pixmapStr
	, actionFun = const (const Nothing)
	}

rgb (r, g, b) =
  "rgb:" ++ showHex2 r ++ '/' : showHex2 g ++ '/' : showHex2 b

gifImage, xwdImage, xpmImage, bmpImage :: String -> CGIImage
gifImage gifPath =
  anyImage "gif" gifPath
xwdImage xwdPath =
  anyImage "xwd" xwdPath
xpmImage xpmPath =
  anyImage "xpm" xpmPath
bmpImage bmpPath =
  anyImage "bmp" bmpPath

-- internal
anyImage :: String -> String -> CGIImage
anyImage format anyPath =
  do key <- inventStdKey
     let path = storeDirectory ++ key
	 ppmPath = path ++ ".ppm"
     system (pbmPath ++ format ++ "toppm " ++ anyPath ++ " > " ++ ppmPath)
     pixmapStr <- readFile ppmPath
     return $ CGIImage'
     	{ pixmap = read pixmapStr
	, actionFun = const (const Nothing)
	}

activateImage :: CGIImage -> CGI () -> CGIImage
activateImage img act =
  activateXY img (const (const (Just act)))

activateXY :: CGIImage -> ActionFun -> CGIImage
activateXY img actFun =
  do cgi_image <- img
     return $ cgi_image { actionFun = actFun }

activateColor :: CGIImage -> Pixel -> CGI () -> CGIImage
activateColor img p act =
  do cgi_image <- img
     let fun x y = if pixelAt (pixmap cgi_image) x y == p 
	 	   then Just act
		   else actionFun cgi_image x y
     return $ cgi_image { actionFun = fun }

overlay :: CGIImage -> CGIImage -> (Int, Int) -> Pixel -> CGIImage
overlay img1 img2 (xul, yul) p =
  do cgi1 <- img1
     cgi2 <- img2
     return $ 
       cgi1 { pixmap = comp (pixmap cgi1) (pixmap cgi2) (xul, yul) p
       	    , actionFun = fun cgi1 cgi2
	    }
  where fun cgi1 cgi2 x1 y1 = 
	  let x2 = x1 - xul
	      y2 = y1 - yul
	      w2 = width $ pixmap cgi2
	      h2 = height $ pixmap cgi2
	      maybeAction2 = actionFun cgi2 x2 y2
	      maybeAction1 = actionFun cgi1 x1 y1
	  in  if x2 >= 0 && y2 >= 0 && x2 < w2 && y2 < w2 && isJust maybeAction2 
	      then maybeAction2
	      else maybeAction1
