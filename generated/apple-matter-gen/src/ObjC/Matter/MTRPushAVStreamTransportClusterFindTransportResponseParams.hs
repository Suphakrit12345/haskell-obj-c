{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterFindTransportResponseParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterFindTransportResponseParams
  ( MTRPushAVStreamTransportClusterFindTransportResponseParams
  , IsMTRPushAVStreamTransportClusterFindTransportResponseParams(..)
  , initWithResponseValue_error
  , transportConfigurations
  , setTransportConfigurations
  , initWithResponseValue_errorSelector
  , transportConfigurationsSelector
  , setTransportConfigurationsSelector


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

-- | Initialize an MTRPushAVStreamTransportClusterFindTransportResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRPushAVStreamTransportClusterFindTransportResponseParams mtrPushAVStreamTransportClusterFindTransportResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrPushAVStreamTransportClusterFindTransportResponseParams -> responseValue -> error_ -> IO (Id MTRPushAVStreamTransportClusterFindTransportResponseParams)
initWithResponseValue_error mtrPushAVStreamTransportClusterFindTransportResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrPushAVStreamTransportClusterFindTransportResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- transportConfigurations@
transportConfigurations :: IsMTRPushAVStreamTransportClusterFindTransportResponseParams mtrPushAVStreamTransportClusterFindTransportResponseParams => mtrPushAVStreamTransportClusterFindTransportResponseParams -> IO (Id NSArray)
transportConfigurations mtrPushAVStreamTransportClusterFindTransportResponseParams  =
    sendMsg mtrPushAVStreamTransportClusterFindTransportResponseParams (mkSelector "transportConfigurations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransportConfigurations:@
setTransportConfigurations :: (IsMTRPushAVStreamTransportClusterFindTransportResponseParams mtrPushAVStreamTransportClusterFindTransportResponseParams, IsNSArray value) => mtrPushAVStreamTransportClusterFindTransportResponseParams -> value -> IO ()
setTransportConfigurations mtrPushAVStreamTransportClusterFindTransportResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterFindTransportResponseParams (mkSelector "setTransportConfigurations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @transportConfigurations@
transportConfigurationsSelector :: Selector
transportConfigurationsSelector = mkSelector "transportConfigurations"

-- | @Selector@ for @setTransportConfigurations:@
setTransportConfigurationsSelector :: Selector
setTransportConfigurationsSelector = mkSelector "setTransportConfigurations:"

