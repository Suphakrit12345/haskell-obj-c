{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterConnectNetworkParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterConnectNetworkParams
  ( MTRNetworkCommissioningClusterConnectNetworkParams
  , IsMTRNetworkCommissioningClusterConnectNetworkParams(..)
  , networkID
  , setNetworkID
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , networkIDSelector
  , setNetworkIDSelector
  , breadcrumbSelector
  , setBreadcrumbSelector
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

-- | @- networkID@
networkID :: IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams => mtrNetworkCommissioningClusterConnectNetworkParams -> IO (Id NSData)
networkID mtrNetworkCommissioningClusterConnectNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterConnectNetworkParams (mkSelector "networkID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkID:@
setNetworkID :: (IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterConnectNetworkParams -> value -> IO ()
setNetworkID mtrNetworkCommissioningClusterConnectNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterConnectNetworkParams (mkSelector "setNetworkID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams => mtrNetworkCommissioningClusterConnectNetworkParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterConnectNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterConnectNetworkParams (mkSelector "breadcrumb") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterConnectNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterConnectNetworkParams (mkSelector "setBreadcrumb:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams => mtrNetworkCommissioningClusterConnectNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterConnectNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterConnectNetworkParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterConnectNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterConnectNetworkParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams => mtrNetworkCommissioningClusterConnectNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterConnectNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterConnectNetworkParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterConnectNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterConnectNetworkParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkID@
networkIDSelector :: Selector
networkIDSelector = mkSelector "networkID"

-- | @Selector@ for @setNetworkID:@
setNetworkIDSelector :: Selector
setNetworkIDSelector = mkSelector "setNetworkID:"

-- | @Selector@ for @breadcrumb@
breadcrumbSelector :: Selector
breadcrumbSelector = mkSelector "breadcrumb"

-- | @Selector@ for @setBreadcrumb:@
setBreadcrumbSelector :: Selector
setBreadcrumbSelector = mkSelector "setBreadcrumb:"

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

