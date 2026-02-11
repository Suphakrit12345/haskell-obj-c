{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentControlClusterResetPINResponseParams@.
module ObjC.Matter.MTRContentControlClusterResetPINResponseParams
  ( MTRContentControlClusterResetPINResponseParams
  , IsMTRContentControlClusterResetPINResponseParams(..)
  , initWithResponseValue_error
  , pinCode
  , setPinCode
  , initWithResponseValue_errorSelector
  , pinCodeSelector
  , setPinCodeSelector


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

-- | Initialize an MTRContentControlClusterResetPINResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRContentControlClusterResetPINResponseParams mtrContentControlClusterResetPINResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrContentControlClusterResetPINResponseParams -> responseValue -> error_ -> IO (Id MTRContentControlClusterResetPINResponseParams)
initWithResponseValue_error mtrContentControlClusterResetPINResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrContentControlClusterResetPINResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- pinCode@
pinCode :: IsMTRContentControlClusterResetPINResponseParams mtrContentControlClusterResetPINResponseParams => mtrContentControlClusterResetPINResponseParams -> IO (Id NSString)
pinCode mtrContentControlClusterResetPINResponseParams  =
    sendMsg mtrContentControlClusterResetPINResponseParams (mkSelector "pinCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPinCode:@
setPinCode :: (IsMTRContentControlClusterResetPINResponseParams mtrContentControlClusterResetPINResponseParams, IsNSString value) => mtrContentControlClusterResetPINResponseParams -> value -> IO ()
setPinCode mtrContentControlClusterResetPINResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentControlClusterResetPINResponseParams (mkSelector "setPinCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @pinCode@
pinCodeSelector :: Selector
pinCodeSelector = mkSelector "pinCode"

-- | @Selector@ for @setPinCode:@
setPinCodeSelector :: Selector
setPinCodeSelector = mkSelector "setPinCode:"

