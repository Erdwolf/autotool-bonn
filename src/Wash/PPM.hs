-- © 2001 Peter Thiemann
module Wash.PPM where

import Char

data Pixmap =
     Pixmap	{ width :: Int
     		, height :: Int
		, maximumColorValue :: Int
		, pixelFun :: Int -> Int -> Pixel
		}

type Pixel = (Int, Int, Int)

instance Show Pixmap where
  show pm = if maximumColorValue pm > 255
  	    then showPixmap pm "P3" showAsciiPixel
	    else showPixmap pm "P6" showBinaryPixel
	    
showPixmap pm code showPixel =
  let w = width pm
      h = height pm
  in
  code ++
  '\n' : (show w) ++
  '\n' : (show h) ++
  '\n' : (show $ maximumColorValue pm) ++
  '\n' : [ ch | y <- [1..h], x <- [1..w], ch <- showPixel (pixelAt pm x y)]

showAsciiPixel (r, g, b) =
  show r ++ ' ' : show g ++ ' ' : show b ++ "\n"

showBinaryPixel (r, g, b) =
  [chr r, chr g, chr b]

instance Read Pixmap where
  readsPrec i = readsPixmap

readsPixmap ('P':'3':rest) = readsPixmap1 readsAsciiPixel (dropWhile isSpace rest)
readsPixmap ('P':'6':rest) = readsPixmap1 readsBinaryPixel (dropWhile isSpace rest)
readsPixmap str = []

readsPixmap1 readsPixel str = 
  do (w, rest1) <- reads str
     (h, rest2) <- reads (dropWhile isSpace rest1)
     (m, rest3) <- reads (dropWhile isSpace rest2)
     (pixs, rest4) <- readsPixels w h readsPixel (dropWhile isSpace rest3)
     return (Pixmap { width = w, 
                      height = h, 
		      maximumColorValue = m, 
		      pixelFun = \x y -> (pixs !! y) !! x }
     	    ,dropWhile isSpace rest4)

readsPixels w h readsPixel str =
  if h > 0 
  then do (sl1, rest1) <- readsScanline w readsPixel str
	  (sls, rest2) <- readsPixels w (h-1) readsPixel rest1
	  return (sl1 : sls, rest2)
  else return ([], str)

readsScanline w readsPixel str =
  if w > 0 
  then do (pix1, rest1) <- readsPixel str
	  (pixs, rest2) <- readsScanline (w-1) readsPixel rest1
	  return (pix1 : pixs, rest2)
  else return ([], str)

readsAsciiPixel str =
  do (r, rest1) <- reads (dropWhile isSpace str)
     (g, rest2) <- reads (dropWhile isSpace rest1)
     (b, rest3) <- reads (dropWhile isSpace rest2)
     return ((r,g,b), rest3)

readsBinaryPixel (cr:cg:cb:rest) =
  return ((ord cr, ord cg, ord cb), rest)
readsBinaryPixel _ =
  []
  
  


-- ======================================================================

create :: Int -> Int -> Int -> Pixel -> Pixmap
create w h m p = 
  Pixmap w h m (const (const p))

oval :: Pixmap -> (Int, Int) -> (Int, Int) -> Pixel -> Bool -> Pixmap
line :: Pixmap -> (Int, Int) -> (Int, Int) -> Pixel -> Pixmap
rectangle :: Pixmap -> (Int, Int) -> (Int, Int) -> Pixel -> Bool -> Pixmap
-- arc
-- poly
-- text
-- bitmap
-- image 

oval pm (xul,yul) (xlr,ylr) p fill =
  pm { pixelFun = fun }
  where w2 = (xlr - xul) `div` 2
	h2 = (ylr - yul) `div` 2
	xm = xul + w2
	ym = yul + h2
	lastfun = pixelFun pm
	fun x y | y < yul || y > ylr || x < xul || x > xlr = lastfun x y
	        | d <= 1.0 && (fill || d >= 0.9) = p
	        | otherwise = lastfun x y
	  where d = fromIntegral ((x - xm) ^ 2) / fromIntegral (w2 ^ 2) +
		    fromIntegral ((y - ym) ^ 2) / fromIntegral (h2 ^ 2)

rectangle pm (xul,yul) (xlr,ylr) p fill =
  pm { pixelFun = fun }
  where lastfun = pixelFun pm
	fun x y | y < yul || y > ylr || x < xul || x > xlr = lastfun x y
	        | x == xul || x == xlr || y == yul || y == ylr || fill = p
		| otherwise = lastfun x y
-- 
line = line2

line1 pm (xul,yul) (xlr,ylr) p =
  pm { pixelFun = fun }
  where lastfun = pixelFun pm
	fun x y | x < xul && x < xlr || x > xul && x > xlr || y < yul && y < ylr || y > yul && y > ylr = lastfun x y
	        | (x,y) `elem` points = p
	        | otherwise = lastfun x y
	-- suppose abs dx >= abs dy
	dx = xlr - xul
	dy = ylr - yul
	adx = abs dx
	ady = abs dy
	sdx = signum dx
	sdy = signum dy
	points | adx >= ady = k adx ady sdx sdy xlr ylr (adx `div` 2) (xul,yul)
	       | otherwise  = 
       		 [(x,y) | (y,x) <- k ady adx sdy sdx ylr xlr (ady `div` 2) (yul,xul)]
	k adx ady sdx sdy xlr ylr = m
	  where m v (x, y) | x == xlr && y == ylr = [(x,y)]
		           | otherwise = (x, y) : m nv' (nx, ny)
		  where
			nv = v - ady
			nv' | nv > 0 = nv
			    | otherwise = nv + adx
			nx  = x + sdx
			ny  | nv > 0 = y
			    | otherwise = y + sdy

line2 pm (xul,yul) (xlr,ylr) p =
  pm { pixelFun = fun }
  where lastfun = pixelFun pm
	dx = fromIntegral (xlr - xul)
	dy = fromIntegral (ylr - yul)
	xul' = fromIntegral xul
	yul' = fromIntegral yul
-- solve: dx1 * dx + dy1 * dy = 0
	dx1 = - dy
	dy1 = dx
	divisor = dy1 * dx - dy
		
	fun x y | x < xul && x < xlr ||
	          x > xul && x > xlr || 
		  y < yul && y < ylr ||
		  y > yul && y > ylr = lastfun x y
		| dx == 0 && x == xul ||
		  dy == 0 && y == yul = p
		| d <= lineWidth = p
		| otherwise = lastfun x y
	  where x' = fromIntegral x
		y' = fromIntegral y
-- solve: x0 = x + t * dx1 
--     && y0 = y + t * dy1
--     && x0 = xul + s * dx 
--     && y0 = yul + s * dy
-- for x0, y0, s, t
		s = ((x' - xul') * dy1 + yul' - y') / divisor
		x0 = xul' + s * dx
		y0 = yul' + s * dy
		d = (x0 - x')^2 + (y0 - y')^2
		lineWidth = 1.0

point pm (x0,y0) p =
  pm { pixelFun = fun }
  where lastfun = pixelFun pm
	fun x y | x == x0 && y == y0 = p
	        | otherwise = lastfun x y

comp pm1 pm2 (xul,yul) p =
  pm1 { pixelFun = fun }
  where 
    w2 = width pm2
    h2 = height pm2
    lastfun = pixelFun pm1
    fun x y | x2 >= 0 && y2 >= 0 && x2 < w2 && y2 < h2 && p2 /= p = p2
            | otherwise = lastfun x y
      where x2 = x - xul
	    y2 = y - yul
	    p2 = pixelFun pm2 x2 y2

-- for efficiency and versatility rely on external programs:

-- ppmmake : create a canvas
-- pbmtext : create a text image (pbmtopgm; pgmtoppm)
-- pnmcomp : compose two images
-- giftopnm : image to portable anymap

-- missing: ppmline ppmoval

pixelAt :: Pixmap -> Int -> Int -> Pixel
pixelAt =
  pixelFun

-- ======================================================================
-- a picture data type
-- ======================================================================
type Color = (Int, Int, Int)
data Picture = 
    Circle Bool		-- radius 1 around origin (filled?)
  | Square Bool		-- unit square, origin= lower left (filled?)
  | Line	-- from origin to (1,0)
  | Colored Color Picture
  | Translate (Double, Double) Picture
  | Rotate Double Picture	-- around origin
  | Scale (Double, Double) Picture
  | Invert Picture
  | And [Picture]
  | Or [Picture]

maxcv (Circle _) = 0
maxcv (Square _) = 0
maxcv (Line) = 0
maxcv (Colored (r,g,b) pic) = maximum [r,g,b,maxcv pic]
maxcv (Translate _ pic) = maxcv pic
maxcv (Rotate _ pic) = maxcv pic
maxcv (Scale _ pic) = maxcv pic
maxcv (Invert pic) = maxcv pic
maxcv (And pics) = maximum (0 : map maxcv pics)
maxcv (Or pics) = maximum (0 : map maxcv pics)

render :: Picture -> Int -> Int -> Color -> Pixmap
render pic w h bg =
  Pixmap { width = w, height = h, maximumColorValue = maxcv pic, pixelFun = pixelAt }
  where pixelAt x y =
	  case renderPix pic bg (fromIntegral x) (fromIntegral y) (sqrt 2) of
	    Nothing -> (0,0,0)
	    Just cl -> cl

renderPix pic bg fx fy fr =
  case pic of
    Circle filled ->		-- radius 1 around origin (filled?)
      let ra = fx * fx + fy * fy in
      if filled then
        if ra <= 1 + fr then Just bg else Nothing
      else
        if abs (ra - 1) <= fr then Just bg else Nothing
    Square filled ->		-- unit square, origin= lower left (filled?)
      if filled then
        if fx + fr >= 0 && fx - fr <= 1 && fy + fr >= 0 && fy - fr <= 1 
	then Just bg else Nothing
      else
        if abs fx <= fr && fy + fr >= 0 && fy - fr <= 1 
	|| abs fy <= fr && fx + fr >= 0 && fx - fr <= 1 
        || abs (fx - 1) <= fr && fy + fr >= 0 && fy - fr <= 1 
	|| abs (fy - 1) <= fr && fx + fr >= 0 && fx - fr <= 1 
	then Just bg else Nothing
    Line ->	-- from origin
      if abs fx <= fr && fy + fr >= 0 && fy - fr <= 1 
      then Just bg else Nothing      
    Colored clr pic ->
      renderPix pic bg fx fy fr >> Just clr
    Translate (dx, dy) pic ->
      renderPix pic bg (fx - dx) (fy - dy) fr
    Rotate phi pic ->	-- around origin
      renderPix pic bg (fx * cos (- phi) - fy * sin (- phi)) 
                       (fx * sin (- phi) + fy * cos (- phi)) fr
    Scale (sx, sy) pic ->
      renderPix pic bg (fx / sx) (fy / sy) (max (abs (fr / sx)) (abs (fr / sy)))
    Invert pic ->
      case renderPix pic bg fx fy fr of
        Just _ -> Nothing
	Nothing -> Just bg
    And pics ->
      foldl (\ j pic -> j >> renderPix pic bg fx fy fr) (Just bg) pics
    Or pics ->
      foldl (\ j pic -> case j of 
			  Nothing -> renderPix pic bg fx fy fr
			  Just clr -> Just clr
	    ) Nothing pics
