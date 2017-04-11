{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ERoad.JSONSpec where

import Test.Hspec
import qualified Network.ERoad.Types as ET
import Network.ERoad.JSON
import qualified Data.HashMap.Strict as M
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)


integer' :: Integer -> ET.Value
{-# INLINE integer' #-}
integer' = ET.Integer . fromJust . ET.nonNegativeInteger


spec :: Spec
spec = do
    describe "JSON deserialization" $ do
        it "Can deserializes integer" $ do
            parse "0" `shouldBe` Just (integer' 0)
            parse "1" `shouldBe` Just (integer' 1)
            parse "2.0" `shouldBe` Just (integer' 2)
            parse "100" `shouldBe` Just (integer' 100)
            parse "1.2e5" `shouldBe` Just (integer' 120000)

        it "Can deserializes big number up to google" $ do
            pending
            parse "1.0e100" `shouldBe` Just (integer' $ 1^100)

        it "Safely rejects when numbers are too big" $
            parse "1.0e100000000000000000000000000" `shouldBe` Nothing

        it "Safely rejects floats" $ do
            parse "1.1234" `shouldBe` Nothing
            parse "1.1234e1" `shouldBe` Nothing

        it "Safely rejects negative numbers" $ do
            parse "-1" `shouldBe` Nothing
            parse "-10" `shouldBe` Nothing
            parse "-1.0e200" `shouldBe` Nothing
            parse "-1.12341" `shouldBe` Nothing

        it "Can deserialize booleans" $ do
            parse "true" `shouldBe` Just (ET.Boolean True)
            parse "false" `shouldBe` Just (ET.Boolean False)

        it "Can deserialize strings" $ do
            parse "\"Hello world\"" `shouldBe` Just (ET.String "Hello world")

        it "Can deserialize arrays" $
            parse "[1.0, true]" `shouldBe`
                Just (ET.List [integer' 1, ET.Boolean True])

        it "Can deserialize dicts" $ do
            parse "{}" `shouldBe` Just (ET.Dict M.empty)
            parse "{\"hello world\": true}" `shouldBe`
                Just (ET.Dict $ M.fromList [("hello world", ET.Boolean True)])


    describe "JSON serialization" $ do
        it "Can serialize integer" $ do
            serialize (integer' 10) `shouldBe` "10"
            serialize (integer' 1337) `shouldBe` "1337"

        it "Can serialize strings" $
            serialize (ET.String "Hello") `shouldBe` "\"Hello\""

        it "Can serialize booleans" $ do
            serialize (ET.Boolean True) `shouldBe` "true"
            serialize (ET.Boolean False) `shouldBe` "false"

        it "Can serialize arrays" $
            serialize (ET.List [integer' 1, ET.Boolean True])
                `shouldBe` "[1,true]"

        it "Can serialize dicts" $ do
            serialize (ET.Dict M.empty) `shouldBe` "{}"
            serialize (ET.Dict $ M.fromList [("hello world", ET.Boolean True)])
                `shouldBe` "{\"hello world\":true}"
