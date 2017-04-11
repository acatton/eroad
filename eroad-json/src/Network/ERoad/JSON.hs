module Network.ERoad.JSON () where

import qualified Network.ERoad.Types as ET

import Data.Aeson
import Data.Scientific (scientific)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as MS
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)


toLazy :: (Eq k, Hashable k) => MS.HashMap k v -> M.HashMap k v
toLazy = flip MS.foldlWithKey' M.empty $ \ m k v -> M.insert k v m


instance ToJSON ET.Value where
    toJSON (ET.Integer i) = Number . fromInteger . ET.nonNegativeToInteger $ i
    toJSON (ET.String  s) = String s
    toJSON (ET.Boolean b) = Bool b
    toJSON (ET.List    l) = Array $ V.map toJSON l
    toJSON (ET.Dict    d) = Object $ M.map toJSON (toLazy d)
