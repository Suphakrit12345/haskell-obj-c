{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterSeekParams@.
module ObjC.Matter.MTRMediaPlaybackClusterSeekParams
  ( MTRMediaPlaybackClusterSeekParams
  , IsMTRMediaPlaybackClusterSeekParams(..)
  , position
  , setPosition
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , positionSelector
  , setPositionSelector
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

-- | @- position@
position :: IsMTRMediaPlaybackClusterSeekParams mtrMediaPlaybackClusterSeekParams => mtrMediaPlaybackClusterSeekParams -> IO (Id NSNumber)
position mtrMediaPlaybackClusterSeekParams  =
    sendMsg mtrMediaPlaybackClusterSeekParams (mkSelector "position") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosition:@
setPosition :: (IsMTRMediaPlaybackClusterSeekParams mtrMediaPlaybackClusterSeekParams, IsNSNumber value) => mtrMediaPlaybackClusterSeekParams -> value -> IO ()
setPosition mtrMediaPlaybackClusterSeekParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterSeekParams (mkSelector "setPosition:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMediaPlaybackClusterSeekParams mtrMediaPlaybackClusterSeekParams => mtrMediaPlaybackClusterSeekParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMediaPlaybackClusterSeekParams  =
    sendMsg mtrMediaPlaybackClusterSeekParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMediaPlaybackClusterSeekParams mtrMediaPlaybackClusterSeekParams, IsNSNumber value) => mtrMediaPlaybackClusterSeekParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMediaPlaybackClusterSeekParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterSeekParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMediaPlaybackClusterSeekParams mtrMediaPlaybackClusterSeekParams => mtrMediaPlaybackClusterSeekParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMediaPlaybackClusterSeekParams  =
    sendMsg mtrMediaPlaybackClusterSeekParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMediaPlaybackClusterSeekParams mtrMediaPlaybackClusterSeekParams, IsNSNumber value) => mtrMediaPlaybackClusterSeekParams -> value -> IO ()
setServerSideProcessingTimeout mtrMediaPlaybackClusterSeekParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMediaPlaybackClusterSeekParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @position@
positionSelector :: Selector
positionSelector = mkSelector "position"

-- | @Selector@ for @setPosition:@
setPositionSelector :: Selector
setPositionSelector = mkSelector "setPosition:"

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

