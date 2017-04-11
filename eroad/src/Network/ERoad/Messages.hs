{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Network.ERoad.Messages (parse, parseEither, serialize) where

import Data.Either (either)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import Control.Monad

import Network.ERoad.Types
import qualified Network.ERoad.Constants as C


type Result a = Either String a

type Parser = Value -> Result Message


idToValue :: ID -> Value
{-# INLINE idToValue #-}
idToValue = Integer . idToNonNegativeInteger


withList :: String -> (List -> Result Message) -> Parser
withList _ f (List l) = f l
withList e _ _        = Left e


getIndex :: String -> Int -> List -> Result Value
getIndex msg idx l =
    case l V.!? idx of
        Just x  -> return x
        Nothing -> Left msg


getIndexDefault :: Value -> Int -> List -> Result Value
getIndexDefault def idx l =
    case l V.!? idx of
        Just x  -> return x
        Nothing -> return def


getHead :: String -> List -> Result Value
getHead = flip getIndex 0


getTail :: String -> List -> Result List
getTail msg l =
    if V.null l
        then Left msg
        else return $ V.drop 1 l


getHeadAndTail :: String -> List -> Result (Value, List)
getHeadAndTail msg l = do
    h <- getHead msg l
    t <- getTail msg l
    return (h, t)


withMessageType :: (NonNegativeInteger -> List -> Result Message) -> Parser
withMessageType f = withList "Messages are supposed to be a WAMP list" $ \ l -> do
    let nullMessage = "Message must have at least one element (the message type)"
    (type_, data_) <- getHeadAndTail nullMessage l
    case type_ of
        Integer id -> f id data_
        _          -> Left "WAMP messages are supposed to start with an ID"


parse :: Value -> Maybe Message
parse = either (const Nothing) Just . parseEither
{-# INLINE parse #-}


-- XXX: get2/3/4Elems is not that elegant

get2Elems :: List -> Result (Value, Value, List)
get2Elems l = do
    first  <- getIndex msg 0 l
    second <- getIndex msg 1 l
    return (first, second, V.drop 2 l)
        where msg = "requires at least two elements"


get3Elems :: List -> Result (Value, Value, Value, List)
get3Elems l = do
    first  <- getIndex msg 0 l
    second <- getIndex msg 1 l
    third  <- getIndex msg 2 l
    return (first, second, third, V.drop 3 l)
        where msg = "requires at least three elements"


get4Elems :: List -> Result (Value, Value, Value, Value, List)
get4Elems l = do
    first  <- getIndex msg 0 l
    second <- getIndex msg 1 l
    third  <- getIndex msg 2 l
    fourth <- getIndex msg 3 l
    return (first, second, third, fourth, V.drop 4 l)
        where msg = "requires at least four elements"


castURI :: String -> Value -> Result URI
castURI _ (String uri) = return $ URI uri  -- TODO: Check URI
castURI e _            = Left e


castID :: String -> Value -> Result ID
castID e (Integer i) = case idFromNonNegativeInteger i  of
                           Just i' -> return i'
                           Nothing -> Left e
castID e _           = Left e


castDict :: String -> Value -> Result Dict
castDict _ (Dict d) = return d
castDict e _        = Left e


castList :: String -> Value -> Result List
castList _ (List l) = return l
castList e _        = Left e


castInt :: String -> Value -> Result NonNegativeInteger
castInt _ (Integer i) = return i -- TODO: Check non negativity
castInt e _           = Left e


errorContext :: String -> Result a -> Result a
errorContext ctx (Left err)  = Left $ ctx ++ ": " ++ err
errorContext _   (Right val) = Right val

parseArgsKwargs :: List -> Result (List, Dict)
parseArgsKwargs l = do
    args    <- getIndexDefault (List V.empty) 0 l
    kwargs  <- getIndexDefault (Dict M.empty) 1 l
    when (V.length l > 2) $ Left "too many elements"
    args    <- castList "Arguments must be a List" args
    kwargs  <- castDict "ArgumentsKw must be a Dict" kwargs
    return (args, kwargs)


get1ElemStrict :: List -> Result Value
get1ElemStrict l = do
    (elem, tail) <- getHeadAndTail "requires at least 1 element" l
    unless (V.null tail) $ Left "requires at most 1 element"
    return elem


get2ElemsStrict :: List -> Result (Value, Value)
get2ElemsStrict l = do
    (first, second, tail) <- get2Elems l
    unless (V.null tail) $ Left "requires at most 2 elements"
    return (first, second)


get3ElemsStrict :: List -> Result (Value, Value, Value)
get3ElemsStrict l = do
    (first, second, third, tail) <- get3Elems l
    unless (V.null tail) $ Left "requires at most 3 elements"
    return (first, second, third)


parseHello :: List -> Result Message
parseHello elems = errorContext "HELLO message" $ do
    (realm, details) <- get2ElemsStrict elems
    realm   <- castURI "Realm must be an URI" realm
    details <- castDict "Details must be a Dict" details
    return $ Hello {realm, details}


parseWelcome :: List -> Result Message
parseWelcome elems = errorContext "WELCOME message" $ do
    (session, details) <- get2ElemsStrict elems
    session <- castID "Session must be an ID" session
    details <- castDict "Details must be a Dict" details
    return $ Welcome {session, details}


parseAbort :: List -> Result Message
parseAbort elems = errorContext "ABORT message" $ do
    (details, reason) <- get2ElemsStrict elems
    details <- castDict "Details must be a Dict" details
    reason  <- castURI  "Reason must be a URI" reason
    return $ Abort {details, reason}


-- XXX: This is a copy of parseAbort. Maybe this should be factored
parseGoodbye :: List -> Result Message
parseGoodbye elems = errorContext "GOODBYE message" $ do
    (details, reason) <- get2ElemsStrict elems
    details <- castDict "Details must be a Dict" details
    reason  <- castURI  "Reason must be a URI" reason
    return $ Goodbye {details, reason}


parseError :: List -> Result Message
parseError elems = errorContext "ERROR message" $ do
    (type_, request, details, error_, tail) <- get4Elems elems
    (args, kwargs) <- parseArgsKwargs tail
    type_   <- castInt "Type must be an integer" type_
    request <- castID "Request must be an ID" request
    details <- castDict "Details must be a Dict" details
    error_  <- castURI "Error must be a URI" error_
    return $ Error {type_, request, details, error_, args, kwargs}


parsePublish :: List -> Result Message
parsePublish elems = errorContext "PUBLISH message" $ do
    (request, options, topic, tail) <- get3Elems elems
    (args, kwargs) <- parseArgsKwargs tail
    request <- castID "Request must be an ID" request
    options <- castDict "Options must be a Dict" options
    topic   <- castURI "Topic must be a URI" topic
    return $ Publish {request, options, topic, args, kwargs}


parsePublished :: List -> Result Message
parsePublished elems = errorContext "PUBLISHED message" $ do
    (request, publication) <- get2ElemsStrict elems
    request     <- castID "Request must be an ID" request
    publication <- castID "Publication must be an ID" publication
    return $ Published {request, publication}


parseSubscribe :: List -> Result Message
parseSubscribe elems = errorContext "SUBSCRIBE message" $ do
    (request, options, topic) <- get3ElemsStrict elems
    request <- castID "Request must be an ID" request
    options <- castDict "Options must be a Dict" options
    topic   <- castURI "Topic must be an URI" topic
    return $ Subscribe {request, options, topic}


parseSubscribed :: List -> Result Message
parseSubscribed elems = errorContext "SUBSCRIBED message" $ do
    (request, subscription) <- get2ElemsStrict elems
    request      <- castID "Request must be an ID" request
    subscription <- castID "Subscription must be an ID" subscription
    return $ Subscribed {request, subscription}


-- XXX: This is copy/pasted from parseSubscribed. This should be factored
parseUnsubscribe :: List -> Result Message
parseUnsubscribe elems = errorContext "UNSUBSCRIBE message" $ do
    (request, subscription) <- get2ElemsStrict elems
    request      <- castID "Request must be an ID" request
    subscription <- castID "Subscription must be an ID" subscription
    return $ Unsubscribe {request, subscription}


parseUnsubscribed :: List -> Result Message
parseUnsubscribed elems = errorContext "UNSUBSCRIBED message" $ do
    request <- get1ElemStrict elems >>= castID "Requset must be an ID"
    return $ Unsubscribed {request}


parseEvent :: List -> Result Message
parseEvent elems = errorContext "EVENT message" $ do
    (subscription, publication, details, tail) <- get3Elems elems
    (args, kwargs) <- parseArgsKwargs tail
    subscription <- castID "Subscription must be an ID" subscription
    publication  <- castID "Publication must be an ID" publication
    details      <- castDict "Details must be a Dict" details
    return Event {subscription, publication, details, args, kwargs}


-- TODO: Do prodecure calls
-- <https://tools.ietf.org/html/draft-oberstet-hybi-tavendo-wamp-02#section-6.4.3>
parseEither :: Value -> Result Message
parseEither = withMessageType $ \ t ->
    case t of _ | t == C.hello        -> parseHello
                | t == C.welcome      -> parseWelcome
                | t == C.abort        -> parseAbort
                | t == C.goodbye      -> parseGoodbye
                | t == C.error        -> parseError
                | t == C.publish      -> parsePublish
                | t == C.published    -> parsePublished
                | t == C.subscribe    -> parseSubscribe
                | t == C.subscribed   -> parseSubscribed
                | t == C.unsubscribe  -> parseUnsubscribe
                | t == C.unsubscribed -> parseUnsubscribed
                | t == C.event        -> parseEvent
                | otherwise           -> const $ Left "Invalid message type"


uriToString :: URI -> Value
uriToString (URI s) = String s


addArgsKwargs :: V.Vector Value -> List -> Dict -> V.Vector Value
addArgsKwargs vec args kwargs =
    let withArgs   = if V.null args && M.null kwargs
                         then []
                         else [ List args ]
        withKwargs = if M.null kwargs
                         then []
                         else [ Dict kwargs ]
    in V.concat [vec, withArgs, withKwargs]


serialize :: Message -> Value
serialize Hello{realm, details} =
    List [ Integer C.hello, String realm', Dict details ]
        where URI realm' = realm

serialize Welcome{session, details} =
    List [ Integer C.welcome, idToValue session, Dict details ]

serialize Abort{details, reason} =
    List [ Integer C.abort, Dict details, uriToString reason ]

serialize Goodbye{details, reason} =
    List [ Integer C.goodbye, Dict details, uriToString reason ]

serialize Error{type_,request,details,error_,args,kwargs} =
    List $ addArgsKwargs [ Integer C.error, Integer type_, idToValue request
                         , Dict details, uriToString error_ ] args kwargs

serialize Publish{request,options,topic,args,kwargs} =
    List $ addArgsKwargs [ Integer C.publish, idToValue request, Dict options
                         , uriToString topic ] args kwargs

serialize Published{request,publication} =
    List [ Integer C.published, idToValue request, idToValue publication ]

serialize Subscribe{request,options,topic} =
    List [ Integer C.subscribe, idToValue request, Dict options, uriToString topic ]

serialize Subscribed{request,subscription} =
    List [ Integer C.subscribed, idToValue request, idToValue subscription ]

serialize Unsubscribe{request,subscription} =
    List [ Integer C.unsubscribe, idToValue request, idToValue subscription ]

serialize Unsubscribed{request} =
    List [ Integer C.unsubscribed, idToValue request ]

serialize Event{subscription,publication,details,args,kwargs} =
    List $ addArgsKwargs [ Integer C.event, idToValue subscription
                         , idToValue publication, Dict details ] args kwargs
