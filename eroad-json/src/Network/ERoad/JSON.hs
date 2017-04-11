module Network.ERoad.JSON (parse, parseEither, serialize) where

import qualified Network.ERoad.Types as ET

import Control.Monad
import Data.Aeson
import Data.Either (either)
import Data.Scientific (scientific, toBoundedInteger)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as MS
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.Word (Word64)
import Data.ByteString.Lazy (ByteString)


toLazy :: (Eq k, Hashable k) => MS.HashMap k v -> M.HashMap k v
toLazy = flip MS.foldlWithKey' M.empty $ \ m k v -> M.insert k v m

fromLazy :: (Eq k, Hashable k) => M.HashMap k v -> MS.HashMap k v
fromLazy = flip M.foldlWithKey' MS.empty $ \ m k v -> MS.insert k v m


instance ToJSON ET.Value where
    toJSON (ET.Integer i) = Number . fromInteger . ET.nonNegativeToInteger $ i
    toJSON (ET.String  s) = String s
    toJSON (ET.Boolean b) = Bool b
    toJSON (ET.List    l) = Array $ V.map toJSON l
    toJSON (ET.Dict    d) = Object $ M.map toJSON (toLazy d)


instance FromJSON ET.Value where
    parseJSON (Number n) =
        case i of Just i'  -> return $ ET.Integer i'
                  Nothing -> fail "Number, but not a non negative 64 bits integer"
            where i = do i' <- toBoundedInteger n :: Maybe Word64
                         ET.nonNegativeInteger $ toInteger i'

    parseJSON (Bool b)   = return $ ET.Boolean b
    parseJSON (String s) = return $ ET.String s
    parseJSON (Array a)  = ET.List <$> mapM parseJSON a

    parseJSON (Object o) = ET.Dict . fromLazy <$> mapM parseJSON o


serialize :: ET.Value -> ByteString
serialize = encode


parse :: ByteString -> Maybe ET.Value
parse = either (const Nothing) Just . parseEither


parseEither :: ByteString -> Either String ET.Value
parseEither = eitherDecode
