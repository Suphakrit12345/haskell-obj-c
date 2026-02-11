{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterSetTimeZoneResponseParams@.
module ObjC.Matter.MTRTimeSynchronizationClusterSetTimeZoneResponseParams
  ( MTRTimeSynchronizationClusterSetTimeZoneResponseParams
  , IsMTRTimeSynchronizationClusterSetTimeZoneResponseParams(..)
  , initWithResponseValue_error
  , dstOffsetRequired
  , setDstOffsetRequired
  , initWithResponseValue_errorSelector
  , dstOffsetRequiredSelector
  , setDstOffsetRequiredSelector


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

-- | Initialize an MTRTimeSynchronizationClusterSetTimeZoneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTimeSynchronizationClusterSetTimeZoneResponseParams mtrTimeSynchronizationClusterSetTimeZoneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrTimeSynchronizationClusterSetTimeZoneResponseParams -> responseValue -> error_ -> IO (Id MTRTimeSynchronizationClusterSetTimeZoneResponseParams)
initWithResponseValue_error mtrTimeSynchronizationClusterSetTimeZoneResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrTimeSynchronizationClusterSetTimeZoneResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- dstOffsetRequired@
dstOffsetRequired :: IsMTRTimeSynchronizationClusterSetTimeZoneResponseParams mtrTimeSynchronizationClusterSetTimeZoneResponseParams => mtrTimeSynchronizationClusterSetTimeZoneResponseParams -> IO (Id NSNumber)
dstOffsetRequired mtrTimeSynchronizationClusterSetTimeZoneResponseParams  =
    sendMsg mtrTimeSynchronizationClusterSetTimeZoneResponseParams (mkSelector "dstOffsetRequired") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDstOffsetRequired:@
setDstOffsetRequired :: (IsMTRTimeSynchronizationClusterSetTimeZoneResponseParams mtrTimeSynchronizationClusterSetTimeZoneResponseParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetTimeZoneResponseParams -> value -> IO ()
setDstOffsetRequired mtrTimeSynchronizationClusterSetTimeZoneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterSetTimeZoneResponseParams (mkSelector "setDstOffsetRequired:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @dstOffsetRequired@
dstOffsetRequiredSelector :: Selector
dstOffsetRequiredSelector = mkSelector "dstOffsetRequired"

-- | @Selector@ for @setDstOffsetRequired:@
setDstOffsetRequiredSelector :: Selector
setDstOffsetRequiredSelector = mkSelector "setDstOffsetRequired:"

