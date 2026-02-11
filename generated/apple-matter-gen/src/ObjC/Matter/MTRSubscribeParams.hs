{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTRSubscribeParams    This is used to control the behavior of attribute/event subscribes.  If not    provided (i.e. nil passed for the MTRSubscribeParams argument), will be    treated as if a default-initialized object was passed in.
--
-- Generated bindings for @MTRSubscribeParams@.
module ObjC.Matter.MTRSubscribeParams
  ( MTRSubscribeParams
  , IsMTRSubscribeParams(..)
  , initWithMinInterval_maxInterval
  , init_
  , new
  , replaceExistingSubscriptions
  , setReplaceExistingSubscriptions
  , resubscribeAutomatically
  , setResubscribeAutomatically
  , minInterval
  , setMinInterval
  , maxInterval
  , setMaxInterval
  , reportEventsUrgently
  , setReportEventsUrgently
  , keepPreviousSubscriptions
  , setKeepPreviousSubscriptions
  , autoResubscribe
  , setAutoResubscribe
  , initWithMinInterval_maxIntervalSelector
  , initSelector
  , newSelector
  , replaceExistingSubscriptionsSelector
  , setReplaceExistingSubscriptionsSelector
  , resubscribeAutomaticallySelector
  , setResubscribeAutomaticallySelector
  , minIntervalSelector
  , setMinIntervalSelector
  , maxIntervalSelector
  , setMaxIntervalSelector
  , reportEventsUrgentlySelector
  , setReportEventsUrgentlySelector
  , keepPreviousSubscriptionsSelector
  , setKeepPreviousSubscriptionsSelector
  , autoResubscribeSelector
  , setAutoResubscribeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRSubscribeParams.  Must provide a minInterval and maxInterval; there are no default values for those.
--
-- ObjC selector: @- initWithMinInterval:maxInterval:@
initWithMinInterval_maxInterval :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber minInterval, IsNSNumber maxInterval) => mtrSubscribeParams -> minInterval -> maxInterval -> IO (Id MTRSubscribeParams)
initWithMinInterval_maxInterval mtrSubscribeParams  minInterval maxInterval =
  withObjCPtr minInterval $ \raw_minInterval ->
    withObjCPtr maxInterval $ \raw_maxInterval ->
        sendMsg mtrSubscribeParams (mkSelector "initWithMinInterval:maxInterval:") (retPtr retVoid) [argPtr (castPtr raw_minInterval :: Ptr ()), argPtr (castPtr raw_maxInterval :: Ptr ())] >>= ownedObject . castPtr

-- | init and new exist for now, for backwards compatibility, and initialize with minInterval set to 1 and maxInterval set to 0, which will not work on its own.  Uses of MTRSubscribeParams that rely on init must all be using (deprecated) APIs that pass in a separate minInterval and maxInterval.
--
-- ObjC selector: @- init@
init_ :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id MTRSubscribeParams)
init_ mtrSubscribeParams  =
    sendMsg mtrSubscribeParams (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRSubscribeParams)
new  =
  do
    cls' <- getRequiredClass "MTRSubscribeParams"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Whether the subscribe should replace already-existing subscriptions.  The default value is YES.
--
-- If YES, the subscribe will cancel any existing subscriptions to the target node when it sets up the new one.
--
-- If NO, the subscribe will allow any previous subscriptions to remain.
--
-- ObjC selector: @- replaceExistingSubscriptions@
replaceExistingSubscriptions :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO Bool
replaceExistingSubscriptions mtrSubscribeParams  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrSubscribeParams (mkSelector "replaceExistingSubscriptions") retCULong []

-- | Whether the subscribe should replace already-existing subscriptions.  The default value is YES.
--
-- If YES, the subscribe will cancel any existing subscriptions to the target node when it sets up the new one.
--
-- If NO, the subscribe will allow any previous subscriptions to remain.
--
-- ObjC selector: @- setReplaceExistingSubscriptions:@
setReplaceExistingSubscriptions :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> Bool -> IO ()
setReplaceExistingSubscriptions mtrSubscribeParams  value =
    sendMsg mtrSubscribeParams (mkSelector "setReplaceExistingSubscriptions:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether the subscription should automatically try to re-establish if it drops.  The default value is YES.
--
-- If NO, loss of subscription will simply lead to an error report.  Some subscription APIs do not support this value.
--
-- If YES, loss of subscription will lead to an automatic resubscription attempt.  If this succeeds, the subscriptionEstablished callback will be called again.
--
-- ObjC selector: @- resubscribeAutomatically@
resubscribeAutomatically :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO Bool
resubscribeAutomatically mtrSubscribeParams  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrSubscribeParams (mkSelector "resubscribeAutomatically") retCULong []

-- | Whether the subscription should automatically try to re-establish if it drops.  The default value is YES.
--
-- If NO, loss of subscription will simply lead to an error report.  Some subscription APIs do not support this value.
--
-- If YES, loss of subscription will lead to an automatic resubscription attempt.  If this succeeds, the subscriptionEstablished callback will be called again.
--
-- ObjC selector: @- setResubscribeAutomatically:@
setResubscribeAutomatically :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> Bool -> IO ()
setResubscribeAutomatically mtrSubscribeParams  value =
    sendMsg mtrSubscribeParams (mkSelector "setResubscribeAutomatically:") retVoid [argCULong (if value then 1 else 0)]

-- | The minimum time, in seconds, between consecutive reports a server will send for this subscription.  This can be used to rate-limit the subscription traffic.  Any non-negative value is allowed, including 0.
--
-- ObjC selector: @- minInterval@
minInterval :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id NSNumber)
minInterval mtrSubscribeParams  =
    sendMsg mtrSubscribeParams (mkSelector "minInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The minimum time, in seconds, between consecutive reports a server will send for this subscription.  This can be used to rate-limit the subscription traffic.  Any non-negative value is allowed, including 0.
--
-- ObjC selector: @- setMinInterval:@
setMinInterval :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber value) => mtrSubscribeParams -> value -> IO ()
setMinInterval mtrSubscribeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSubscribeParams (mkSelector "setMinInterval:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The suggested maximum time, in seconds, during which the server is allowed to send no reports at all for this subscription.  Must be at least as large as minInterval.  The server is allowed to use a larger time than this as the maxInterval it selects if it needs to (e.g. to meet its power budget).
--
-- ObjC selector: @- maxInterval@
maxInterval :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id NSNumber)
maxInterval mtrSubscribeParams  =
    sendMsg mtrSubscribeParams (mkSelector "maxInterval") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The suggested maximum time, in seconds, during which the server is allowed to send no reports at all for this subscription.  Must be at least as large as minInterval.  The server is allowed to use a larger time than this as the maxInterval it selects if it needs to (e.g. to meet its power budget).
--
-- ObjC selector: @- setMaxInterval:@
setMaxInterval :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber value) => mtrSubscribeParams -> value -> IO ()
setMaxInterval mtrSubscribeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSubscribeParams (mkSelector "setMaxInterval:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether events will be reported urgently. The default value is YES.
--
-- If YES, the events will be reported as soon as the minInterval does not prevent it.
--
-- If NO, the events will be reported at the maximum interval.
--
-- ObjC selector: @- reportEventsUrgently@
reportEventsUrgently :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO Bool
reportEventsUrgently mtrSubscribeParams  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrSubscribeParams (mkSelector "reportEventsUrgently") retCULong []

-- | Controls whether events will be reported urgently. The default value is YES.
--
-- If YES, the events will be reported as soon as the minInterval does not prevent it.
--
-- If NO, the events will be reported at the maximum interval.
--
-- ObjC selector: @- setReportEventsUrgently:@
setReportEventsUrgently :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> Bool -> IO ()
setReportEventsUrgently mtrSubscribeParams  value =
    sendMsg mtrSubscribeParams (mkSelector "setReportEventsUrgently:") retVoid [argCULong (if value then 1 else 0)]

-- | @- keepPreviousSubscriptions@
keepPreviousSubscriptions :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id NSNumber)
keepPreviousSubscriptions mtrSubscribeParams  =
    sendMsg mtrSubscribeParams (mkSelector "keepPreviousSubscriptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeepPreviousSubscriptions:@
setKeepPreviousSubscriptions :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber value) => mtrSubscribeParams -> value -> IO ()
setKeepPreviousSubscriptions mtrSubscribeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSubscribeParams (mkSelector "setKeepPreviousSubscriptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- autoResubscribe@
autoResubscribe :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id NSNumber)
autoResubscribe mtrSubscribeParams  =
    sendMsg mtrSubscribeParams (mkSelector "autoResubscribe") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAutoResubscribe:@
setAutoResubscribe :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber value) => mtrSubscribeParams -> value -> IO ()
setAutoResubscribe mtrSubscribeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSubscribeParams (mkSelector "setAutoResubscribe:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMinInterval:maxInterval:@
initWithMinInterval_maxIntervalSelector :: Selector
initWithMinInterval_maxIntervalSelector = mkSelector "initWithMinInterval:maxInterval:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @replaceExistingSubscriptions@
replaceExistingSubscriptionsSelector :: Selector
replaceExistingSubscriptionsSelector = mkSelector "replaceExistingSubscriptions"

-- | @Selector@ for @setReplaceExistingSubscriptions:@
setReplaceExistingSubscriptionsSelector :: Selector
setReplaceExistingSubscriptionsSelector = mkSelector "setReplaceExistingSubscriptions:"

-- | @Selector@ for @resubscribeAutomatically@
resubscribeAutomaticallySelector :: Selector
resubscribeAutomaticallySelector = mkSelector "resubscribeAutomatically"

-- | @Selector@ for @setResubscribeAutomatically:@
setResubscribeAutomaticallySelector :: Selector
setResubscribeAutomaticallySelector = mkSelector "setResubscribeAutomatically:"

-- | @Selector@ for @minInterval@
minIntervalSelector :: Selector
minIntervalSelector = mkSelector "minInterval"

-- | @Selector@ for @setMinInterval:@
setMinIntervalSelector :: Selector
setMinIntervalSelector = mkSelector "setMinInterval:"

-- | @Selector@ for @maxInterval@
maxIntervalSelector :: Selector
maxIntervalSelector = mkSelector "maxInterval"

-- | @Selector@ for @setMaxInterval:@
setMaxIntervalSelector :: Selector
setMaxIntervalSelector = mkSelector "setMaxInterval:"

-- | @Selector@ for @reportEventsUrgently@
reportEventsUrgentlySelector :: Selector
reportEventsUrgentlySelector = mkSelector "reportEventsUrgently"

-- | @Selector@ for @setReportEventsUrgently:@
setReportEventsUrgentlySelector :: Selector
setReportEventsUrgentlySelector = mkSelector "setReportEventsUrgently:"

-- | @Selector@ for @keepPreviousSubscriptions@
keepPreviousSubscriptionsSelector :: Selector
keepPreviousSubscriptionsSelector = mkSelector "keepPreviousSubscriptions"

-- | @Selector@ for @setKeepPreviousSubscriptions:@
setKeepPreviousSubscriptionsSelector :: Selector
setKeepPreviousSubscriptionsSelector = mkSelector "setKeepPreviousSubscriptions:"

-- | @Selector@ for @autoResubscribe@
autoResubscribeSelector :: Selector
autoResubscribeSelector = mkSelector "autoResubscribe"

-- | @Selector@ for @setAutoResubscribe:@
setAutoResubscribeSelector :: Selector
setAutoResubscribeSelector = mkSelector "setAutoResubscribe:"

