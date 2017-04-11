{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ERoad.JSONSpec where

import Test.Hspec
import qualified Network.ERoad.Messages as EM
import qualified Network.ERoad.Types as ET
import qualified Network.ERoad.JSON
import qualified Data.HashMap.Strict as M
import Data.Aeson (encode)
import Data.Maybe (fromJust)


integer' :: Integer -> ET.Value
{-# INLINE integer' #-}
integer' = ET.Integer . fromJust . ET.nonNegativeInteger


spec :: Spec
spec = do
    describe "JSON serialization" $ do
        it "Can serialize integer" $ do
            encode (integer' 10) `shouldBe` "10"
            encode (integer' 1337) `shouldBe` "1337"

        it "Can serialize strings" $
            encode (ET.String "Hello") `shouldBe` "\"Hello\""

        it "Can serialize booleans" $ do
            encode (ET.Boolean True) `shouldBe` "true"
            encode (ET.Boolean False) `shouldBe` "false"

        it "Can serialize arrays" $
            encode (ET.List [integer' 1, ET.Boolean True])
                `shouldBe` "[1,true]"

        it "Can serialize dicts" $ do
            encode (ET.Dict M.empty) `shouldBe` "{}"
            encode (ET.Dict $ M.fromList [("hello world", ET.Boolean True)])
                `shouldBe` "{\"hello world\":true}"
