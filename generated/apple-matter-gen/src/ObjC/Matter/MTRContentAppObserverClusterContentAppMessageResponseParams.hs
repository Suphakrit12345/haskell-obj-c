{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentAppObserverClusterContentAppMessageResponseParams@.
module ObjC.Matter.MTRContentAppObserverClusterContentAppMessageResponseParams
  ( MTRContentAppObserverClusterContentAppMessageResponseParams
  , IsMTRContentAppObserverClusterContentAppMessageResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , data_
  , setData
  , encodingHint
  , setEncodingHint
  , initWithResponseValue_errorSelector
  , statusSelector
  , setStatusSelector
  , dataSelector
  , setDataSelector
  , encodingHintSelector
  , setEncodingHintSelector


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

-- | Initialize an MTRContentAppObserverClusterContentAppMessageResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrContentAppObserverClusterContentAppMessageResponseParams -> responseValue -> error_ -> IO (Id MTRContentAppObserverClusterContentAppMessageResponseParams)
initWithResponseValue_error mtrContentAppObserverClusterContentAppMessageResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrContentAppObserverClusterContentAppMessageResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- status@
status :: IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams => mtrContentAppObserverClusterContentAppMessageResponseParams -> IO (Id NSNumber)
status mtrContentAppObserverClusterContentAppMessageResponseParams  =
    sendMsg mtrContentAppObserverClusterContentAppMessageResponseParams (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams, IsNSNumber value) => mtrContentAppObserverClusterContentAppMessageResponseParams -> value -> IO ()
setStatus mtrContentAppObserverClusterContentAppMessageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentAppObserverClusterContentAppMessageResponseParams (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams => mtrContentAppObserverClusterContentAppMessageResponseParams -> IO (Id NSString)
data_ mtrContentAppObserverClusterContentAppMessageResponseParams  =
    sendMsg mtrContentAppObserverClusterContentAppMessageResponseParams (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams, IsNSString value) => mtrContentAppObserverClusterContentAppMessageResponseParams -> value -> IO ()
setData mtrContentAppObserverClusterContentAppMessageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentAppObserverClusterContentAppMessageResponseParams (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- encodingHint@
encodingHint :: IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams => mtrContentAppObserverClusterContentAppMessageResponseParams -> IO (Id NSString)
encodingHint mtrContentAppObserverClusterContentAppMessageResponseParams  =
    sendMsg mtrContentAppObserverClusterContentAppMessageResponseParams (mkSelector "encodingHint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEncodingHint:@
setEncodingHint :: (IsMTRContentAppObserverClusterContentAppMessageResponseParams mtrContentAppObserverClusterContentAppMessageResponseParams, IsNSString value) => mtrContentAppObserverClusterContentAppMessageResponseParams -> value -> IO ()
setEncodingHint mtrContentAppObserverClusterContentAppMessageResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentAppObserverClusterContentAppMessageResponseParams (mkSelector "setEncodingHint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @encodingHint@
encodingHintSelector :: Selector
encodingHintSelector = mkSelector "encodingHint"

-- | @Selector@ for @setEncodingHint:@
setEncodingHintSelector :: Selector
setEncodingHintSelector = mkSelector "setEncodingHint:"

