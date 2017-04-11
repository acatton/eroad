{-# LANGUAGE OverloadedStrings #-}

module Network.ERoad.Types ( URI(..)
                           , ID
                           , NonNegativeInteger
                           , nonNegativeInteger
                           , nonNegativeToInteger
                           , Value(..)
                           , List
                           , Dict
                           , Message(..)
                           ) where

import Data.HashMap.Strict (HashMap)
import Data.Text           (Text, intercalate)
import Data.Vector         (Vector)


data URI = URI Text
               deriving (Show, Eq)

type ID = NonNegativeInteger -- TODO: Fix type

data NonNegativeInteger = NonNegativeInteger Integer
                              deriving (Show, Eq, Ord)



nonNegativeInteger :: Integer -> Maybe NonNegativeInteger
nonNegativeInteger i | i < 0     = Nothing
                     | otherwise = Just $ NonNegativeInteger i


nonNegativeToInteger :: NonNegativeInteger -> Integer
nonNegativeToInteger (NonNegativeInteger i) = i


data Value = Integer !NonNegativeInteger
           | String  !Text
           | Boolean !Bool
           | Dict    !Dict
           | List    !List
               deriving (Show, Eq)


type List = Vector Value

type Dict = HashMap Text Value


data Message = Hello        { realm   :: !URI, details :: !Dict }
             | Welcome      { session :: !ID, details :: !Dict }
             | Abort        { details :: !Dict, reason  :: !URI }
             | Goodbye      { details :: !Dict, reason  :: !URI }
             | Error        { type_ :: !NonNegativeInteger, request :: !ID , details :: !Dict
                            , error_ :: !URI, args :: !List, kwargs :: !Dict }
             | Publish      { request :: !ID, options :: !Dict, topic :: !URI, args :: !List
                            , kwargs :: !Dict }
             | Published    { request :: !ID, publication :: !ID }
             | Subscribe    { request :: !ID, options :: !Dict, topic :: !URI }
             | Subscribed   { request :: !ID, subscription :: !ID }
             | Unsubscribe  { request :: !ID, subscription :: !ID }
             | Unsubscribed { request :: !ID }
             | Event        { subscription :: !ID, publication :: !ID, details :: !Dict
                            , args :: !List, kwargs :: !Dict }
--             | Call         {
--             | Result       {
--             | Register     {
--             | Registered   {
--             | Unregister   {
--             | Unregistered {
--             | Invocation   {
--             | Yield        {
                 deriving (Show, Eq)
