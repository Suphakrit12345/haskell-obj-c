{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterActivateAudioTrackParams@.
module ObjC.Matter.MTRMediaPlaybackClusterActivateAudioTrackParams
  ( MTRMediaPlaybackClusterActivateAudioTrackParams
  , IsMTRMediaPlaybackClusterActivateAudioTrackParams(..)
  , trackID
  , setTrackID
  , audioOutputIndex
  , setAudioOutputIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , trackIDSelector
  , setTrackIDSelector
  , audioOutputIndexSelector
  , setAudioOutputIndexSelector
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

-- | @- trackID@
trackID :: IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams => mtrMediaPlaybackClusterActivateAudioTrackParams -> IO (Id NSString)
trackID mtrMediaPlaybackClusterActivateAudioTrackParams  =
    sendMsg mtrMediaPlaybackClusterActivateAudioTrackParams (mkSelector "trackID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTrackID:@
setTrackID :: (IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams, IsNSString value) => mtrMediaPlaybackClusterActivateAudioTrackParams -> value -> IO ()
setTrackID mtrMediaPlaybackClusterActivateAudioTrackParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterActivateAudioTrackParams (mkSelector "setTrackID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioOutputIndex@
audioOutputIndex :: IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams => mtrMediaPlaybackClusterActivateAudioTrackParams -> IO (Id NSNumber)
audioOutputIndex mtrMediaPlaybackClusterActivateAudioTrackParams  =
    sendMsg mtrMediaPlaybackClusterActivateAudioTrackParams (mkSelector "audioOutputIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAudioOutputIndex:@
setAudioOutputIndex :: (IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams, IsNSNumber value) => mtrMediaPlaybackClusterActivateAudioTrackParams -> value -> IO ()
setAudioOutputIndex mtrMediaPlaybackClusterActivateAudioTrackParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterActivateAudioTrackParams (mkSelector "setAudioOutputIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams => mtrMediaPlaybackClusterActivateAudioTrackParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMediaPlaybackClusterActivateAudioTrackParams  =
    sendMsg mtrMediaPlaybackClusterActivateAudioTrackParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams, IsNSNumber value) => mtrMediaPlaybackClusterActivateAudioTrackParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMediaPlaybackClusterActivateAudioTrackParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterActivateAudioTrackParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams => mtrMediaPlaybackClusterActivateAudioTrackParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMediaPlaybackClusterActivateAudioTrackParams  =
    sendMsg mtrMediaPlaybackClusterActivateAudioTrackParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams, IsNSNumber value) => mtrMediaPlaybackClusterActivateAudioTrackParams -> value -> IO ()
setServerSideProcessingTimeout mtrMediaPlaybackClusterActivateAudioTrackParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterActivateAudioTrackParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trackID@
trackIDSelector :: Selector
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @setTrackID:@
setTrackIDSelector :: Selector
setTrackIDSelector = mkSelector "setTrackID:"

-- | @Selector@ for @audioOutputIndex@
audioOutputIndexSelector :: Selector
audioOutputIndexSelector = mkSelector "audioOutputIndex"

-- | @Selector@ for @setAudioOutputIndex:@
setAudioOutputIndexSelector :: Selector
setAudioOutputIndexSelector = mkSelector "setAudioOutputIndex:"

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

