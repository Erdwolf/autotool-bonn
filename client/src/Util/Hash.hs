-- Note: we shouldn't use Autolib.Hash because the latter isn't secure
module Util.Hash (
    Digest,
    Hash (..)
) where

import qualified Data.Digest.Pure.SHA as S
import qualified Data.ByteString.Lazy as BL

hashBase :: BL.ByteString -> S.Digest
hashBase = S.sha1

type Digest = String

class Hash a where
    hash :: a -> Digest
    hashList :: [a] -> Digest

    hashList = hashList0

hashList0 :: Hash a => [a] -> Digest
hashList0 = hashString . ("List" ++) . concatMap hash

instance Hash a => Hash [a] where
    hash = hashList

instance Hash Char where
    hash = hashList . (:[])
    hashList = hashString

instance Hash Int where
    hash = hashString . show

instance (Hash a, Hash b) => Hash (a, b) where
    hash (a, b) = hashString $ "Pair" ++ hash a ++ hash b

hashString :: String -> Digest
hashString = S.showDigest . hashBase . BL.pack . map (fromIntegral . fromEnum)
