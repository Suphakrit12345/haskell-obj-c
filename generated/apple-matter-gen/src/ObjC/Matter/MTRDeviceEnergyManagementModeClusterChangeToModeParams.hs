{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementModeClusterChangeToModeParams@.
module ObjC.Matter.MTRDeviceEnergyManagementModeClusterChangeToModeParams
  ( MTRDeviceEnergyManagementModeClusterChangeToModeParams
  , IsMTRDeviceEnergyManagementModeClusterChangeToModeParams(..)
  , newMode
  , setNewMode
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , newModeSelector
  , setNewModeSelector
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

-- | @- newMode@
newMode :: IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> IO (Id NSNumber)
newMode mtrDeviceEnergyManagementModeClusterChangeToModeParams  =
    sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeParams (mkSelector "newMode") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewMode:@
setNewMode :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> value -> IO ()
setNewMode mtrDeviceEnergyManagementModeClusterChangeToModeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeParams (mkSelector "setNewMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementModeClusterChangeToModeParams  =
    sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementModeClusterChangeToModeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementModeClusterChangeToModeParams  =
    sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementModeClusterChangeToModeParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDeviceEnergyManagementModeClusterChangeToModeParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newMode@
newModeSelector :: Selector
newModeSelector = mkSelector "newMode"

-- | @Selector@ for @setNewMode:@
setNewModeSelector :: Selector
setNewModeSelector = mkSelector "setNewMode:"

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

