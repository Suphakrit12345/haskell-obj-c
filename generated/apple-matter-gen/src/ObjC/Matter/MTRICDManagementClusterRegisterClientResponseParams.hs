{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterRegisterClientResponseParams@.
module ObjC.Matter.MTRICDManagementClusterRegisterClientResponseParams
  ( MTRICDManagementClusterRegisterClientResponseParams
  , IsMTRICDManagementClusterRegisterClientResponseParams(..)
  , initWithResponseValue_error
  , icdCounter
  , setIcdCounter
  , initWithResponseValue_errorSelector
  , icdCounterSelector
  , setIcdCounterSelector


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

-- | Initialize an MTRICDManagementClusterRegisterClientResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRICDManagementClusterRegisterClientResponseParams mtricdManagementClusterRegisterClientResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtricdManagementClusterRegisterClientResponseParams -> responseValue -> error_ -> IO (Id MTRICDManagementClusterRegisterClientResponseParams)
initWithResponseValue_error mtricdManagementClusterRegisterClientResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtricdManagementClusterRegisterClientResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- icdCounter@
icdCounter :: IsMTRICDManagementClusterRegisterClientResponseParams mtricdManagementClusterRegisterClientResponseParams => mtricdManagementClusterRegisterClientResponseParams -> IO (Id NSNumber)
icdCounter mtricdManagementClusterRegisterClientResponseParams  =
    sendMsg mtricdManagementClusterRegisterClientResponseParams (mkSelector "icdCounter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIcdCounter:@
setIcdCounter :: (IsMTRICDManagementClusterRegisterClientResponseParams mtricdManagementClusterRegisterClientResponseParams, IsNSNumber value) => mtricdManagementClusterRegisterClientResponseParams -> value -> IO ()
setIcdCounter mtricdManagementClusterRegisterClientResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtricdManagementClusterRegisterClientResponseParams (mkSelector "setIcdCounter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @icdCounter@
icdCounterSelector :: Selector
icdCounterSelector = mkSelector "icdCounter"

-- | @Selector@ for @setIcdCounter:@
setIcdCounterSelector :: Selector
setIcdCounterSelector = mkSelector "setIcdCounter:"

