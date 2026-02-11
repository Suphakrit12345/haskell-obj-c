{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterSetRegulatoryConfigParams@.
module ObjC.Matter.MTRGeneralCommissioningClusterSetRegulatoryConfigParams
  ( MTRGeneralCommissioningClusterSetRegulatoryConfigParams
  , IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams(..)
  , newRegulatoryConfig
  , setNewRegulatoryConfig
  , countryCode
  , setCountryCode
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , newRegulatoryConfigSelector
  , setNewRegulatoryConfigSelector
  , countryCodeSelector
  , setCountryCodeSelector
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

-- | @- newRegulatoryConfig@
newRegulatoryConfig :: IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> IO (Id NSNumber)
newRegulatoryConfig mtrGeneralCommissioningClusterSetRegulatoryConfigParams  =
    sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "newRegulatoryConfig") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setNewRegulatoryConfig:@
setNewRegulatoryConfig :: (IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> value -> IO ()
setNewRegulatoryConfig mtrGeneralCommissioningClusterSetRegulatoryConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "setNewRegulatoryConfig:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- countryCode@
countryCode :: IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> IO (Id NSString)
countryCode mtrGeneralCommissioningClusterSetRegulatoryConfigParams  =
    sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCountryCode:@
setCountryCode :: (IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams, IsNSString value) => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> value -> IO ()
setCountryCode mtrGeneralCommissioningClusterSetRegulatoryConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "setCountryCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- breadcrumb@
breadcrumb :: IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> IO (Id NSNumber)
breadcrumb mtrGeneralCommissioningClusterSetRegulatoryConfigParams  =
    sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "breadcrumb") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> value -> IO ()
setBreadcrumb mtrGeneralCommissioningClusterSetRegulatoryConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "setBreadcrumb:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGeneralCommissioningClusterSetRegulatoryConfigParams  =
    sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGeneralCommissioningClusterSetRegulatoryConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGeneralCommissioningClusterSetRegulatoryConfigParams  =
    sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams mtrGeneralCommissioningClusterSetRegulatoryConfigParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetRegulatoryConfigParams -> value -> IO ()
setServerSideProcessingTimeout mtrGeneralCommissioningClusterSetRegulatoryConfigParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralCommissioningClusterSetRegulatoryConfigParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newRegulatoryConfig@
newRegulatoryConfigSelector :: Selector
newRegulatoryConfigSelector = mkSelector "newRegulatoryConfig"

-- | @Selector@ for @setNewRegulatoryConfig:@
setNewRegulatoryConfigSelector :: Selector
setNewRegulatoryConfigSelector = mkSelector "setNewRegulatoryConfig:"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @setCountryCode:@
setCountryCodeSelector :: Selector
setCountryCodeSelector = mkSelector "setCountryCode:"

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

