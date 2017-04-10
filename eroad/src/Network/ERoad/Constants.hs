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
import Network.ERoad.Types (NonNegativeInteger)

hello :: NonNegativeInteger
hello = 1

welcome :: NonNegativeInteger
welcome = 2

abort :: NonNegativeInteger
abort = 3

goodbye :: NonNegativeInteger
goodbye = 6

error :: NonNegativeInteger
error = 8

publish :: NonNegativeInteger
publish = 16

published :: NonNegativeInteger
published = 17

subscribe :: NonNegativeInteger
subscribe = 32

subscribed :: NonNegativeInteger
subscribed = 33

unsubscribe :: NonNegativeInteger
unsubscribe = 34

unsubscribed :: NonNegativeInteger
unsubscribed = 35

event :: NonNegativeInteger
event = 36

call :: NonNegativeInteger
call = 48

result :: NonNegativeInteger
result = 50

register :: NonNegativeInteger
register = 64

registered :: NonNegativeInteger
registered = 65

unregister :: NonNegativeInteger
unregister = 66

unregistered :: NonNegativeInteger
unregistered = 67

invocation :: NonNegativeInteger
invocation = 68

yield :: NonNegativeInteger
yield = 70
