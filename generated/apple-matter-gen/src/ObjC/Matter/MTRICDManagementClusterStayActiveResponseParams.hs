{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterStayActiveResponseParams@.
module ObjC.Matter.MTRICDManagementClusterStayActiveResponseParams
  ( MTRICDManagementClusterStayActiveResponseParams
  , IsMTRICDManagementClusterStayActiveResponseParams(..)
  , initWithResponseValue_error
  , promisedActiveDuration
  , setPromisedActiveDuration
  , initWithResponseValue_errorSelector
  , promisedActiveDurationSelector
  , setPromisedActiveDurationSelector


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

-- | Initialize an MTRICDManagementClusterStayActiveResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRICDManagementClusterStayActiveResponseParams mtricdManagementClusterStayActiveResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtricdManagementClusterStayActiveResponseParams -> responseValue -> error_ -> IO (Id MTRICDManagementClusterStayActiveResponseParams)
initWithResponseValue_error mtricdManagementClusterStayActiveResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtricdManagementClusterStayActiveResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- promisedActiveDuration@
promisedActiveDuration :: IsMTRICDManagementClusterStayActiveResponseParams mtricdManagementClusterStayActiveResponseParams => mtricdManagementClusterStayActiveResponseParams -> IO (Id NSNumber)
promisedActiveDuration mtricdManagementClusterStayActiveResponseParams  =
    sendMsg mtricdManagementClusterStayActiveResponseParams (mkSelector "promisedActiveDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPromisedActiveDuration:@
setPromisedActiveDuration :: (IsMTRICDManagementClusterStayActiveResponseParams mtricdManagementClusterStayActiveResponseParams, IsNSNumber value) => mtricdManagementClusterStayActiveResponseParams -> value -> IO ()
setPromisedActiveDuration mtricdManagementClusterStayActiveResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterStayActiveResponseParams (mkSelector "setPromisedActiveDuration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @promisedActiveDuration@
promisedActiveDurationSelector :: Selector
promisedActiveDurationSelector = mkSelector "promisedActiveDuration"

-- | @Selector@ for @setPromisedActiveDuration:@
setPromisedActiveDurationSelector :: Selector
setPromisedActiveDurationSelector = mkSelector "setPromisedActiveDuration:"

