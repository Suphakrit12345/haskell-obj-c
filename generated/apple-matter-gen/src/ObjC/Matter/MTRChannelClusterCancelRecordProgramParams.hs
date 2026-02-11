{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterCancelRecordProgramParams@.
module ObjC.Matter.MTRChannelClusterCancelRecordProgramParams
  ( MTRChannelClusterCancelRecordProgramParams
  , IsMTRChannelClusterCancelRecordProgramParams(..)
  , programIdentifier
  , setProgramIdentifier
  , shouldRecordSeries
  , setShouldRecordSeries
  , externalIDList
  , setExternalIDList
  , data_
  , setData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , programIdentifierSelector
  , setProgramIdentifierSelector
  , shouldRecordSeriesSelector
  , setShouldRecordSeriesSelector
  , externalIDListSelector
  , setExternalIDListSelector
  , dataSelector
  , setDataSelector
  , timedInvokeTimeoutMsSelector
  , setTimedInvokeTimeoutMsSelector
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector


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

-- | @- programIdentifier@
programIdentifier :: IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams => mtrChannelClusterCancelRecordProgramParams -> IO (Id NSString)
programIdentifier mtrChannelClusterCancelRecordProgramParams  =
    sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "programIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProgramIdentifier:@
setProgramIdentifier :: (IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams, IsNSString value) => mtrChannelClusterCancelRecordProgramParams -> value -> IO ()
setProgramIdentifier mtrChannelClusterCancelRecordProgramParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "setProgramIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shouldRecordSeries@
shouldRecordSeries :: IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams => mtrChannelClusterCancelRecordProgramParams -> IO (Id NSNumber)
shouldRecordSeries mtrChannelClusterCancelRecordProgramParams  =
    sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "shouldRecordSeries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShouldRecordSeries:@
setShouldRecordSeries :: (IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams, IsNSNumber value) => mtrChannelClusterCancelRecordProgramParams -> value -> IO ()
setShouldRecordSeries mtrChannelClusterCancelRecordProgramParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "setShouldRecordSeries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- externalIDList@
externalIDList :: IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams => mtrChannelClusterCancelRecordProgramParams -> IO (Id NSArray)
externalIDList mtrChannelClusterCancelRecordProgramParams  =
    sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "externalIDList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams, IsNSArray value) => mtrChannelClusterCancelRecordProgramParams -> value -> IO ()
setExternalIDList mtrChannelClusterCancelRecordProgramParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "setExternalIDList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams => mtrChannelClusterCancelRecordProgramParams -> IO (Id NSData)
data_ mtrChannelClusterCancelRecordProgramParams  =
    sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams, IsNSData value) => mtrChannelClusterCancelRecordProgramParams -> value -> IO ()
setData mtrChannelClusterCancelRecordProgramParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams => mtrChannelClusterCancelRecordProgramParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrChannelClusterCancelRecordProgramParams  =
    sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams, IsNSNumber value) => mtrChannelClusterCancelRecordProgramParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrChannelClusterCancelRecordProgramParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams => mtrChannelClusterCancelRecordProgramParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrChannelClusterCancelRecordProgramParams  =
    sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRChannelClusterCancelRecordProgramParams mtrChannelClusterCancelRecordProgramParams, IsNSNumber value) => mtrChannelClusterCancelRecordProgramParams -> value -> IO ()
setServerSideProcessingTimeout mtrChannelClusterCancelRecordProgramParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterCancelRecordProgramParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @programIdentifier@
programIdentifierSelector :: Selector
programIdentifierSelector = mkSelector "programIdentifier"

-- | @Selector@ for @setProgramIdentifier:@
setProgramIdentifierSelector :: Selector
setProgramIdentifierSelector = mkSelector "setProgramIdentifier:"

-- | @Selector@ for @shouldRecordSeries@
shouldRecordSeriesSelector :: Selector
shouldRecordSeriesSelector = mkSelector "shouldRecordSeries"

-- | @Selector@ for @setShouldRecordSeries:@
setShouldRecordSeriesSelector :: Selector
setShouldRecordSeriesSelector = mkSelector "setShouldRecordSeries:"

-- | @Selector@ for @externalIDList@
externalIDListSelector :: Selector
externalIDListSelector = mkSelector "externalIDList"

-- | @Selector@ for @setExternalIDList:@
setExternalIDListSelector :: Selector
setExternalIDListSelector = mkSelector "setExternalIDList:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

