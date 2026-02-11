{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterGetProgramGuideParams@.
module ObjC.Matter.MTRChannelClusterGetProgramGuideParams
  ( MTRChannelClusterGetProgramGuideParams
  , IsMTRChannelClusterGetProgramGuideParams(..)
  , startTime
  , setStartTime
  , endTime
  , setEndTime
  , channelList
  , setChannelList
  , pageToken
  , setPageToken
  , recordingFlag
  , setRecordingFlag
  , externalIDList
  , setExternalIDList
  , data_
  , setData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , startTimeSelector
  , setStartTimeSelector
  , endTimeSelector
  , setEndTimeSelector
  , channelListSelector
  , setChannelListSelector
  , pageTokenSelector
  , setPageTokenSelector
  , recordingFlagSelector
  , setRecordingFlagSelector
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

-- | @- startTime@
startTime :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
startTime mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "startTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartTime:@
setStartTime :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setStartTime mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setStartTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endTime@
endTime :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
endTime mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "endTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndTime:@
setEndTime :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setEndTime mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setEndTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channelList@
channelList :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSArray)
channelList mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "channelList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannelList:@
setChannelList :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSArray value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setChannelList mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setChannelList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pageToken@
pageToken :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id MTRChannelClusterPageTokenStruct)
pageToken mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "pageToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPageToken:@
setPageToken :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsMTRChannelClusterPageTokenStruct value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setPageToken mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setPageToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recordingFlag@
recordingFlag :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
recordingFlag mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "recordingFlag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordingFlag:@
setRecordingFlag :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setRecordingFlag mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setRecordingFlag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- externalIDList@
externalIDList :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSArray)
externalIDList mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "externalIDList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSArray value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setExternalIDList mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setExternalIDList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSData)
data_ mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSData value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setData mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrChannelClusterGetProgramGuideParams  =
    sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setServerSideProcessingTimeout mtrChannelClusterGetProgramGuideParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterGetProgramGuideParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @setEndTime:@
setEndTimeSelector :: Selector
setEndTimeSelector = mkSelector "setEndTime:"

-- | @Selector@ for @channelList@
channelListSelector :: Selector
channelListSelector = mkSelector "channelList"

-- | @Selector@ for @setChannelList:@
setChannelListSelector :: Selector
setChannelListSelector = mkSelector "setChannelList:"

-- | @Selector@ for @pageToken@
pageTokenSelector :: Selector
pageTokenSelector = mkSelector "pageToken"

-- | @Selector@ for @setPageToken:@
setPageTokenSelector :: Selector
setPageTokenSelector = mkSelector "setPageToken:"

-- | @Selector@ for @recordingFlag@
recordingFlagSelector :: Selector
recordingFlagSelector = mkSelector "recordingFlag"

-- | @Selector@ for @setRecordingFlag:@
setRecordingFlagSelector :: Selector
setRecordingFlagSelector = mkSelector "setRecordingFlag:"

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

