{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterUpdateTwoDCartesianZoneParams@.
module ObjC.Matter.MTRZoneManagementClusterUpdateTwoDCartesianZoneParams
  ( MTRZoneManagementClusterUpdateTwoDCartesianZoneParams
  , IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams(..)
  , zoneID
  , setZoneID
  , zone
  , setZone
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , zoneIDSelector
  , setZoneIDSelector
  , zoneSelector
  , setZoneSelector
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

-- | @- zoneID@
zoneID :: IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> IO (Id NSNumber)
zoneID mtrZoneManagementClusterUpdateTwoDCartesianZoneParams  =
    sendMsg mtrZoneManagementClusterUpdateTwoDCartesianZoneParams (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoneID:@
setZoneID :: (IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams, IsNSNumber value) => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> value -> IO ()
setZoneID mtrZoneManagementClusterUpdateTwoDCartesianZoneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterUpdateTwoDCartesianZoneParams (mkSelector "setZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- zone@
zone :: IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> IO (Id MTRZoneManagementClusterTwoDCartesianZoneStruct)
zone mtrZoneManagementClusterUpdateTwoDCartesianZoneParams  =
    sendMsg mtrZoneManagementClusterUpdateTwoDCartesianZoneParams (mkSelector "zone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZone:@
setZone :: (IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams, IsMTRZoneManagementClusterTwoDCartesianZoneStruct value) => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> value -> IO ()
setZone mtrZoneManagementClusterUpdateTwoDCartesianZoneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterUpdateTwoDCartesianZoneParams (mkSelector "setZone:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrZoneManagementClusterUpdateTwoDCartesianZoneParams  =
    sendMsg mtrZoneManagementClusterUpdateTwoDCartesianZoneParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams, IsNSNumber value) => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrZoneManagementClusterUpdateTwoDCartesianZoneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterUpdateTwoDCartesianZoneParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrZoneManagementClusterUpdateTwoDCartesianZoneParams  =
    sendMsg mtrZoneManagementClusterUpdateTwoDCartesianZoneParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams mtrZoneManagementClusterUpdateTwoDCartesianZoneParams, IsNSNumber value) => mtrZoneManagementClusterUpdateTwoDCartesianZoneParams -> value -> IO ()
setServerSideProcessingTimeout mtrZoneManagementClusterUpdateTwoDCartesianZoneParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterUpdateTwoDCartesianZoneParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @zone@
zoneSelector :: Selector
zoneSelector = mkSelector "zone"

-- | @Selector@ for @setZone:@
setZoneSelector :: Selector
setZoneSelector = mkSelector "setZone:"

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

