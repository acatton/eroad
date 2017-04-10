{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ERoad.MessagesSpec where

import Test.Hspec
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

import Network.ERoad.Messages
import Network.ERoad.Types
import qualified Network.ERoad.Constants as C

parse' :: V.Vector Value -> Maybe Message
parse' = parse . List

nonEmptyDict :: Dict
nonEmptyDict = M.fromList [ ("hello", String "world"), ("HELLO", String "WORLD") ]

spec :: Spec
spec = do
    describe "parse" $ do
        it "Can't parse empty message" $ do
            parse' [] `shouldBe` Nothing

        it "Can't parse invalid type" $ do
            parse' [ Integer 255, String "Invalid" ] `shouldBe` Nothing

        describe "HELLO" $ do
            it "Parses simple message" $ do
                parse' [ Integer C.hello, String "realm", Dict M.empty ]
                    `shouldBe` Just Hello { realm = URI "realm", details = M.empty }

            it "Fails when there are too fews arguments" $ do
                parse' [ Integer C.hello ] `shouldBe` Nothing
                parse' [ Integer C.hello, String "realm" ] `shouldBe` Nothing

            it "Refuses extra arguments" $ do
                let msg = [ Integer C.hello, String "realm", Dict M.empty, String "extra" ]
                parse' msg `shouldBe` Nothing

            it "Checks URI" $ pending

        describe "WELCOME" $ do
            it "Parses simple message" $ do
                parse' [ Integer C.welcome, Integer 0, Dict M.empty ]
                    `shouldBe` Just Welcome { session = 0, details = M.empty }

            it "Refuses invalid ID" $ pending

        describe "ABORT" $ do
            it "Parses simple message" $ do
                parse' [ Integer C.abort, Dict M.empty, String "com.example" ]
                    `shouldBe` Just Abort { details = M.empty
                                          , reason  = URI "com.example"
                                          }

            it "Validates URI" $ do
                pending
                parse' [ Integer C.abort, Dict M.empty, String "invalid uri" ]
                    `shouldBe` Nothing

        describe "GOODBYE" $ do
            it "Parses simple message" $
                parse' [ Integer C.goodbye, Dict M.empty, String "com.example" ]
                    `shouldBe` Just Goodbye { details = M.empty
                                            , reason  = URI "com.example"
                                            }

        describe "ERROR" $ do
            it "Parses simple message" $
                parse' [ Integer C.error, Integer 0, Integer 1337
                       , Dict M.empty, String "com.example.error" ]
                    `shouldBe` Just Error { type_   = 0
                                          , request = 1337
                                          , details = M.empty
                                          , error_  = URI "com.example.error"
                                          , args    = []
                                          , kwargs  = M.empty
                                          }

            it "Parses optional arguments in message" $
                parse' [ Integer C.error, Integer 0, Integer 1337
                       , Dict M.empty, String "com.example.error"
                       , List [String "a"], Dict nonEmptyDict ]
                    `shouldBe` Just Error { type_   = 0
                                          , request = 1337
                                          , details = M.empty
                                          , error_  = URI "com.example.error"
                                          , args    = [String "a"]
                                          , kwargs  = nonEmptyDict
                                          }

            it "Fails when there is too many" $
                parse' [ Integer C.error, Integer 0, Integer 0
                       , Dict M.empty, String "com.example.error"
                       , List [], Dict M.empty, Integer 0]
                    `shouldBe` Nothing


        describe "PUBLISH" $ do
            it "Parses simple messages" $
                parse' [ Integer C.publish, Integer 0, Dict M.empty
                       , String "com.example.topic" ]
                    `shouldBe` Just Publish { request = 0
                                            , options = M.empty
                                            , topic   = URI "com.example.topic"
                                            , args    = []
                                            , kwargs  = M.empty
                                            }


        describe "PUBLISHED" $ do
            it "Parses simple messages" $
                parse' [ Integer C.published, Integer 0, Integer 0 ]
                    `shouldBe` Just Published { request     = 0
                                              , publication = 0
                                              }


        describe "SUBSCRIBE" $ do
            it "Parses simple messages" $
                parse' [ Integer C.subscribe, Integer 0, Dict M.empty
                       , String "com.example.topic" ]
                    `shouldBe` Just Subscribe { request = 0
                                              , options = M.empty
                                              , topic   = URI "com.example.topic"
                                              }

        describe "SUBSCRIBED" $ do
            it "Parses simple messages" $
                parse' [ Integer C.subscribed, Integer 0, Integer 1337 ]
                    `shouldBe` Just Subscribed { request      = 0
                                               , subscription = 1337
                                               }

        describe "UNSUBSCRIBE" $ do
            it "Parses simple messages" $
                parse' [ Integer C.unsubscribe, Integer 0, Integer 1337 ]
                    `shouldBe` Just Unsubscribe { request      = 0
                                                , subscription = 1337
                                                }

        describe "UNSUBSCRIBED" $ do
            it "Parses simple messages" $
                parse' [ Integer C.unsubscribed, Integer 0 ]
                    `shouldBe` Just Unsubscribed { request = 0 }

        describe "EVENT" $ do
            it "Parses simple messages" $
                parse' [ Integer C.event, Integer 0, Integer 1337
                       , Dict nonEmptyDict ]
                    `shouldBe` Just Event { subscription = 0
                                          , publication  = 1337
                                          , details      = nonEmptyDict
                                          , args         = []
                                          , kwargs       = M.empty
                                          }

    describe "serialize" $ do
        it "Never sends empty args & kwargs" $
            serialize Error { type_ = 0, request = 1337, details = nonEmptyDict
                            , error_ = URI "com.example.error", args = []
                            , kwargs = M.empty }
                `shouldBe` List [ Integer C.error, Integer 0, Integer 1337
                                , Dict nonEmptyDict, String "com.example.error" ]

        it "Serialize args when they're not empty" $
            serialize Error { type_ = 0, request = 1337, details = nonEmptyDict
                            , error_ = URI "com.example.error", args = [Integer 1]
                            , kwargs = M.empty }
                `shouldBe` List [ Integer C.error, Integer 0, Integer 1337
                                , Dict nonEmptyDict, String "com.example.error"
                                , List [Integer 1] ]

        it "Don't miss args when sending kwargs" $
            serialize Error { type_ = 0, request = 1337, details = M.empty
                            , error_ = URI "com.example.error", args = []
                            , kwargs = nonEmptyDict }
                `shouldBe` List [ Integer C.error, Integer 0, Integer 1337
                                , Dict M.empty, String "com.example.error"
                                , List [], Dict nonEmptyDict ]

        it "Sends args and kwargs" $
            serialize Error { type_ = 0, request = 1337, details = M.empty
                            , error_ = URI "com.example.error", args = [Integer 1]
                            , kwargs = nonEmptyDict }
                `shouldBe` List [ Integer C.error, Integer 0, Integer 1337
                                , Dict M.empty, String "com.example.error"
                                , List [Integer 1], Dict nonEmptyDict ]
