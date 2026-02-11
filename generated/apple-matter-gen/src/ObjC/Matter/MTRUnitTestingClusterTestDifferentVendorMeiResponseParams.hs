{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestDifferentVendorMeiResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestDifferentVendorMeiResponseParams
  ( MTRUnitTestingClusterTestDifferentVendorMeiResponseParams
  , IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams(..)
  , initWithResponseValue_error
  , arg1
  , setArg1
  , eventNumber
  , setEventNumber
  , initWithResponseValue_errorSelector
  , arg1Selector
  , setArg1Selector
  , eventNumberSelector
  , setEventNumberSelector


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

-- | Initialize an MTRUnitTestingClusterTestDifferentVendorMeiResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestDifferentVendorMeiResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestDifferentVendorMeiResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrUnitTestingClusterTestDifferentVendorMeiResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- arg1@
arg1 :: IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> IO (Id NSNumber)
arg1 mtrUnitTestingClusterTestDifferentVendorMeiResponseParams  =
    sendMsg mtrUnitTestingClusterTestDifferentVendorMeiResponseParams (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> value -> IO ()
setArg1 mtrUnitTestingClusterTestDifferentVendorMeiResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestDifferentVendorMeiResponseParams (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- eventNumber@
eventNumber :: IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> IO (Id NSNumber)
eventNumber mtrUnitTestingClusterTestDifferentVendorMeiResponseParams  =
    sendMsg mtrUnitTestingClusterTestDifferentVendorMeiResponseParams (mkSelector "eventNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEventNumber:@
setEventNumber :: (IsMTRUnitTestingClusterTestDifferentVendorMeiResponseParams mtrUnitTestingClusterTestDifferentVendorMeiResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestDifferentVendorMeiResponseParams -> value -> IO ()
setEventNumber mtrUnitTestingClusterTestDifferentVendorMeiResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestDifferentVendorMeiResponseParams (mkSelector "setEventNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @arg1@
arg1Selector :: Selector
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @eventNumber@
eventNumberSelector :: Selector
eventNumberSelector = mkSelector "eventNumber"

-- | @Selector@ for @setEventNumber:@
setEventNumberSelector :: Selector
setEventNumberSelector = mkSelector "setEventNumber:"

