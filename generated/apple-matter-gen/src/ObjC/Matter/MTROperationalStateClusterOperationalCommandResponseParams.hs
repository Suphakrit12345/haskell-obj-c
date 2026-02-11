{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterOperationalCommandResponseParams@.
module ObjC.Matter.MTROperationalStateClusterOperationalCommandResponseParams
  ( MTROperationalStateClusterOperationalCommandResponseParams
  , IsMTROperationalStateClusterOperationalCommandResponseParams(..)
  , initWithResponseValue_error
  , commandResponseState
  , setCommandResponseState
  , initWithResponseValue_errorSelector
  , commandResponseStateSelector
  , setCommandResponseStateSelector


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

-- | Initialize an MTROperationalStateClusterOperationalCommandResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalStateClusterOperationalCommandResponseParams mtrOperationalStateClusterOperationalCommandResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalStateClusterOperationalCommandResponseParams -> responseValue -> error_ -> IO (Id MTROperationalStateClusterOperationalCommandResponseParams)
initWithResponseValue_error mtrOperationalStateClusterOperationalCommandResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrOperationalStateClusterOperationalCommandResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- commandResponseState@
commandResponseState :: IsMTROperationalStateClusterOperationalCommandResponseParams mtrOperationalStateClusterOperationalCommandResponseParams => mtrOperationalStateClusterOperationalCommandResponseParams -> IO (Id MTROperationalStateClusterErrorStateStruct)
commandResponseState mtrOperationalStateClusterOperationalCommandResponseParams  =
    sendMsg mtrOperationalStateClusterOperationalCommandResponseParams (mkSelector "commandResponseState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCommandResponseState:@
setCommandResponseState :: (IsMTROperationalStateClusterOperationalCommandResponseParams mtrOperationalStateClusterOperationalCommandResponseParams, IsMTROperationalStateClusterErrorStateStruct value) => mtrOperationalStateClusterOperationalCommandResponseParams -> value -> IO ()
setCommandResponseState mtrOperationalStateClusterOperationalCommandResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalStateClusterOperationalCommandResponseParams (mkSelector "setCommandResponseState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @commandResponseState@
commandResponseStateSelector :: Selector
commandResponseStateSelector = mkSelector "commandResponseState"

-- | @Selector@ for @setCommandResponseState:@
setCommandResponseStateSelector :: Selector
setCommandResponseStateSelector = mkSelector "setCommandResponseState:"

