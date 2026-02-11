{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams@.
module ObjC.Matter.MTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams
  ( MTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams
  , IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams(..)
  , activeDataset
  , setActiveDataset
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , activeDatasetSelector
  , setActiveDatasetSelector
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

-- | @- activeDataset@
activeDataset :: IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> IO (Id NSData)
activeDataset mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams  =
    sendMsg mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams (mkSelector "activeDataset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActiveDataset:@
setActiveDataset :: (IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, IsNSData value) => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> value -> IO ()
setActiveDataset mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams (mkSelector "setActiveDataset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- breadcrumb@
breadcrumb :: IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> IO (Id NSNumber)
breadcrumb mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams  =
    sendMsg mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams (mkSelector "breadcrumb") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, IsNSNumber value) => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> value -> IO ()
setBreadcrumb mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams (mkSelector "setBreadcrumb:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams  =
    sendMsg mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, IsNSNumber value) => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams  =
    sendMsg mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, IsNSNumber value) => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activeDataset@
activeDatasetSelector :: Selector
activeDatasetSelector = mkSelector "activeDataset"

-- | @Selector@ for @setActiveDataset:@
setActiveDatasetSelector :: Selector
setActiveDatasetSelector = mkSelector "setActiveDataset:"

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

