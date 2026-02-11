{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterReorderNetworkParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterReorderNetworkParams
  ( MTRNetworkCommissioningClusterReorderNetworkParams
  , IsMTRNetworkCommissioningClusterReorderNetworkParams(..)
  , networkID
  , setNetworkID
  , networkIndex
  , setNetworkIndex
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , networkIDSelector
  , setNetworkIDSelector
  , networkIndexSelector
  , setNetworkIndexSelector
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
networkID :: IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams => mtrNetworkCommissioningClusterReorderNetworkParams -> IO (Id NSData)
networkID mtrNetworkCommissioningClusterReorderNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "networkID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkID:@
setNetworkID :: (IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterReorderNetworkParams -> value -> IO ()
setNetworkID mtrNetworkCommissioningClusterReorderNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "setNetworkID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- networkIndex@
networkIndex :: IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams => mtrNetworkCommissioningClusterReorderNetworkParams -> IO (Id NSNumber)
networkIndex mtrNetworkCommissioningClusterReorderNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "networkIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkIndex:@
setNetworkIndex :: (IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterReorderNetworkParams -> value -> IO ()
setNetworkIndex mtrNetworkCommissioningClusterReorderNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "setNetworkIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams => mtrNetworkCommissioningClusterReorderNetworkParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterReorderNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "breadcrumb") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterReorderNetworkParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterReorderNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "setBreadcrumb:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams => mtrNetworkCommissioningClusterReorderNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterReorderNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterReorderNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterReorderNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams => mtrNetworkCommissioningClusterReorderNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterReorderNetworkParams  =
    sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterReorderNetworkParams mtrNetworkCommissioningClusterReorderNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterReorderNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterReorderNetworkParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterReorderNetworkParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkID@
networkIDSelector :: Selector
networkIDSelector = mkSelector "networkID"

-- | @Selector@ for @setNetworkID:@
setNetworkIDSelector :: Selector
setNetworkIDSelector = mkSelector "setNetworkID:"

-- | @Selector@ for @networkIndex@
networkIndexSelector :: Selector
networkIndexSelector = mkSelector "networkIndex"

-- | @Selector@ for @setNetworkIndex:@
setNetworkIndexSelector :: Selector
setNetworkIndexSelector = mkSelector "setNetworkIndex:"

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

