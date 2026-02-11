{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams
  ( MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams
  , IsMTRPushAVStreamTransportClusterAllocatePushTransportResponseParams(..)
  , initWithResponseValue_error
  , transportConfiguration
  , setTransportConfiguration
  , initWithResponseValue_errorSelector
  , transportConfigurationSelector
  , setTransportConfigurationSelector


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

-- | Initialize an MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRPushAVStreamTransportClusterAllocatePushTransportResponseParams mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams -> responseValue -> error_ -> IO (Id MTRPushAVStreamTransportClusterAllocatePushTransportResponseParams)
initWithResponseValue_error mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- transportConfiguration@
transportConfiguration :: IsMTRPushAVStreamTransportClusterAllocatePushTransportResponseParams mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams => mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams -> IO (Id MTRPushAVStreamTransportClusterTransportConfigurationStruct)
transportConfiguration mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams  =
    sendMsg mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams (mkSelector "transportConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransportConfiguration:@
setTransportConfiguration :: (IsMTRPushAVStreamTransportClusterAllocatePushTransportResponseParams mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams, IsMTRPushAVStreamTransportClusterTransportConfigurationStruct value) => mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams -> value -> IO ()
setTransportConfiguration mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterAllocatePushTransportResponseParams (mkSelector "setTransportConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @transportConfiguration@
transportConfigurationSelector :: Selector
transportConfigurationSelector = mkSelector "transportConfiguration"

-- | @Selector@ for @setTransportConfiguration:@
setTransportConfigurationSelector :: Selector
setTransportConfigurationSelector = mkSelector "setTransportConfiguration:"

