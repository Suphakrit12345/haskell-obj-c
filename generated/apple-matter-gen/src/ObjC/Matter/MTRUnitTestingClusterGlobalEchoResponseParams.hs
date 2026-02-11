{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterGlobalEchoResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterGlobalEchoResponseParams
  ( MTRUnitTestingClusterGlobalEchoResponseParams
  , IsMTRUnitTestingClusterGlobalEchoResponseParams(..)
  , initWithResponseValue_error
  , field1
  , setField1
  , field2
  , setField2
  , initWithResponseValue_errorSelector
  , field1Selector
  , setField1Selector
  , field2Selector
  , setField2Selector


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

-- | Initialize an MTRUnitTestingClusterGlobalEchoResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterGlobalEchoResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterGlobalEchoResponseParams)
initWithResponseValue_error mtrUnitTestingClusterGlobalEchoResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrUnitTestingClusterGlobalEchoResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- field1@
field1 :: IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams => mtrUnitTestingClusterGlobalEchoResponseParams -> IO (Id MTRDataTypeTestGlobalStruct)
field1 mtrUnitTestingClusterGlobalEchoResponseParams  =
    sendMsg mtrUnitTestingClusterGlobalEchoResponseParams (mkSelector "field1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setField1:@
setField1 :: (IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams, IsMTRDataTypeTestGlobalStruct value) => mtrUnitTestingClusterGlobalEchoResponseParams -> value -> IO ()
setField1 mtrUnitTestingClusterGlobalEchoResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterGlobalEchoResponseParams (mkSelector "setField1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- field2@
field2 :: IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams => mtrUnitTestingClusterGlobalEchoResponseParams -> IO (Id NSNumber)
field2 mtrUnitTestingClusterGlobalEchoResponseParams  =
    sendMsg mtrUnitTestingClusterGlobalEchoResponseParams (mkSelector "field2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setField2:@
setField2 :: (IsMTRUnitTestingClusterGlobalEchoResponseParams mtrUnitTestingClusterGlobalEchoResponseParams, IsNSNumber value) => mtrUnitTestingClusterGlobalEchoResponseParams -> value -> IO ()
setField2 mtrUnitTestingClusterGlobalEchoResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterGlobalEchoResponseParams (mkSelector "setField2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @field1@
field1Selector :: Selector
field1Selector = mkSelector "field1"

-- | @Selector@ for @setField1:@
setField1Selector :: Selector
setField1Selector = mkSelector "setField1:"

-- | @Selector@ for @field2@
field2Selector :: Selector
field2Selector = mkSelector "field2"

-- | @Selector@ for @setField2:@
setField2Selector :: Selector
setField2Selector = mkSelector "setField2:"

