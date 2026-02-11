{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams
  ( MTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams
  , IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams(..)
  , operationalDataset
  , setOperationalDataset
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , operationalDatasetSelector
  , setOperationalDatasetSelector
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

-- | @- operationalDataset@
operationalDataset :: IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> IO (Id NSData)
operationalDataset mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams (mkSelector "operationalDataset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationalDataset:@
setOperationalDataset :: (IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> value -> IO ()
setOperationalDataset mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams (mkSelector "setOperationalDataset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams (mkSelector "breadcrumb") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams (mkSelector "setBreadcrumb:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationalDataset@
operationalDatasetSelector :: Selector
operationalDatasetSelector = mkSelector "operationalDataset"

-- | @Selector@ for @setOperationalDataset:@
setOperationalDatasetSelector :: Selector
setOperationalDatasetSelector = mkSelector "setOperationalDataset:"

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

