{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterAddNOCParams@.
module ObjC.Matter.MTROperationalCredentialsClusterAddNOCParams
  ( MTROperationalCredentialsClusterAddNOCParams
  , IsMTROperationalCredentialsClusterAddNOCParams(..)
  , nocValue
  , setNocValue
  , icacValue
  , setIcacValue
  , ipkValue
  , setIpkValue
  , caseAdminSubject
  , setCaseAdminSubject
  , adminVendorId
  , setAdminVendorId
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , nocValueSelector
  , setNocValueSelector
  , icacValueSelector
  , setIcacValueSelector
  , ipkValueSelector
  , setIpkValueSelector
  , caseAdminSubjectSelector
  , setCaseAdminSubjectSelector
  , adminVendorIdSelector
  , setAdminVendorIdSelector
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

-- | @- nocValue@
nocValue :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSData)
nocValue mtrOperationalCredentialsClusterAddNOCParams  =
    sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "nocValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNocValue:@
setNocValue :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSData value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setNocValue mtrOperationalCredentialsClusterAddNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "setNocValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- icacValue@
icacValue :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSData)
icacValue mtrOperationalCredentialsClusterAddNOCParams  =
    sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "icacValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcacValue:@
setIcacValue :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSData value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setIcacValue mtrOperationalCredentialsClusterAddNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "setIcacValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ipkValue@
ipkValue :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSData)
ipkValue mtrOperationalCredentialsClusterAddNOCParams  =
    sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "ipkValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIpkValue:@
setIpkValue :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSData value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setIpkValue mtrOperationalCredentialsClusterAddNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "setIpkValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- caseAdminSubject@
caseAdminSubject :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSNumber)
caseAdminSubject mtrOperationalCredentialsClusterAddNOCParams  =
    sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "caseAdminSubject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaseAdminSubject:@
setCaseAdminSubject :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setCaseAdminSubject mtrOperationalCredentialsClusterAddNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "setCaseAdminSubject:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- adminVendorId@
adminVendorId :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSNumber)
adminVendorId mtrOperationalCredentialsClusterAddNOCParams  =
    sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "adminVendorId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdminVendorId:@
setAdminVendorId :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setAdminVendorId mtrOperationalCredentialsClusterAddNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "setAdminVendorId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterAddNOCParams  =
    sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "timedInvokeTimeoutMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterAddNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "setTimedInvokeTimeoutMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterAddNOCParams  =
    sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "serverSideProcessingTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterAddNOCParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterAddNOCParams (mkSelector "setServerSideProcessingTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nocValue@
nocValueSelector :: Selector
nocValueSelector = mkSelector "nocValue"

-- | @Selector@ for @setNocValue:@
setNocValueSelector :: Selector
setNocValueSelector = mkSelector "setNocValue:"

-- | @Selector@ for @icacValue@
icacValueSelector :: Selector
icacValueSelector = mkSelector "icacValue"

-- | @Selector@ for @setIcacValue:@
setIcacValueSelector :: Selector
setIcacValueSelector = mkSelector "setIcacValue:"

-- | @Selector@ for @ipkValue@
ipkValueSelector :: Selector
ipkValueSelector = mkSelector "ipkValue"

-- | @Selector@ for @setIpkValue:@
setIpkValueSelector :: Selector
setIpkValueSelector = mkSelector "setIpkValue:"

-- | @Selector@ for @caseAdminSubject@
caseAdminSubjectSelector :: Selector
caseAdminSubjectSelector = mkSelector "caseAdminSubject"

-- | @Selector@ for @setCaseAdminSubject:@
setCaseAdminSubjectSelector :: Selector
setCaseAdminSubjectSelector = mkSelector "setCaseAdminSubject:"

-- | @Selector@ for @adminVendorId@
adminVendorIdSelector :: Selector
adminVendorIdSelector = mkSelector "adminVendorId"

-- | @Selector@ for @setAdminVendorId:@
setAdminVendorIdSelector :: Selector
setAdminVendorIdSelector = mkSelector "setAdminVendorId:"

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

