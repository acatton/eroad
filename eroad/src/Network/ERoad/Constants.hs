module Network.ERoad.Constants ( abort
                               , call
                               , error
                               , event
                               , goodbye
                               , hello
                               , invocation
                               , publish
                               , published
                               , register
                               , registered
                               , result
                               , subscribe
                               , subscribed
                               , unregister
                               , unregistered
                               , unsubscribe
                               , unsubscribed
                               , welcome
                               , yield
                               ) where

import Prelude hiding (error)
import Network.ERoad.Types (NonNegativeInteger, nonNegativeInteger)
import Data.Maybe (fromJust)

nni :: Integer -> NonNegativeInteger
{-# INLINE nni #-}
nni = fromJust . nonNegativeInteger

hello :: NonNegativeInteger
hello = nni 1

welcome :: NonNegativeInteger
welcome = nni 2

abort :: NonNegativeInteger
abort = nni 3

goodbye :: NonNegativeInteger
goodbye = nni 6

error :: NonNegativeInteger
error = nni 8

publish :: NonNegativeInteger
publish = nni 16

published :: NonNegativeInteger
published = nni 17

subscribe :: NonNegativeInteger
subscribe = nni 32

subscribed :: NonNegativeInteger
subscribed = nni 33

unsubscribe :: NonNegativeInteger
unsubscribe = nni 34

unsubscribed :: NonNegativeInteger
unsubscribed = nni 35

event :: NonNegativeInteger
event = nni 36

call :: NonNegativeInteger
call = nni 48

result :: NonNegativeInteger
result = nni 50

register :: NonNegativeInteger
register = nni 64

registered :: NonNegativeInteger
registered = nni 65

unregister :: NonNegativeInteger
unregister = nni 66

unregistered :: NonNegativeInteger
unregistered = nni 67

invocation :: NonNegativeInteger
invocation = nni 68

yield :: NonNegativeInteger
yield = nni 70
