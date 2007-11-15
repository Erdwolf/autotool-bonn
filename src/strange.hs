import Control.Concurrent
import Control.Monad
import Data.IORef
import System.IO
import System.Random

data Config = 
     Config { input :: MVar [Integer]
            , outputs :: IORef [ MVar [Integer] ]
            , count :: Int
            }

make :: IO Config
make = do
    inp <- newEmptyMVar 
    out <- newIORef []
    return $ Config { input = inp, outputs = out, count = 0 }

step :: Config -> IO Config
step c = do
    mv <- tryTakeMVar $ input c
    xs <- sequence $ replicate 1000 $ randomRIO ( 0, 2 :: Int )
    os <- readIORef $ outputs c
    sequence $ do 
        o <- os
        return $ hPutStrLn stderr "huh"
    hPutStrLn stderr $ show $ sum xs
    return $ c { count = count c + 1 }

action :: Config -> IO ()
action c = do
    c' <- step c
    action c'

main = do
    let n = 5
    c : cs <- sequence $ replicate n $ make
    mapM ( forkIO . action ) cs
    action c

    