import Network.XmlRpc.Server
import Network.XmlRpc.Internals


instance XmlRpcType Integer where
    toValue i = toValue $ show i
    fromValue v = do s <- fromValue v ; return $ read s
    getType _ = TString

main :: IO ()
main =  cgiXmlRpcServer [ ("copy", fun copy ) ]

copy :: Integer -> IO Integer
copy x = return x

