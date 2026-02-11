{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterGetDayEntryResponseParams@.
module ObjC.Matter.MTRCommodityTariffClusterGetDayEntryResponseParams
  ( MTRCommodityTariffClusterGetDayEntryResponseParams
  , IsMTRCommodityTariffClusterGetDayEntryResponseParams(..)
  , initWithResponseValue_error
  , dayEntry
  , setDayEntry
  , initWithResponseValue_errorSelector
  , dayEntrySelector
  , setDayEntrySelector


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

-- | Initialize an MTRCommodityTariffClusterGetDayEntryResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommodityTariffClusterGetDayEntryResponseParams mtrCommodityTariffClusterGetDayEntryResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommodityTariffClusterGetDayEntryResponseParams -> responseValue -> error_ -> IO (Id MTRCommodityTariffClusterGetDayEntryResponseParams)
initWithResponseValue_error mtrCommodityTariffClusterGetDayEntryResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCommodityTariffClusterGetDayEntryResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- dayEntry@
dayEntry :: IsMTRCommodityTariffClusterGetDayEntryResponseParams mtrCommodityTariffClusterGetDayEntryResponseParams => mtrCommodityTariffClusterGetDayEntryResponseParams -> IO (Id MTRCommodityTariffClusterDayEntryStruct)
dayEntry mtrCommodityTariffClusterGetDayEntryResponseParams  =
    sendMsg mtrCommodityTariffClusterGetDayEntryResponseParams (mkSelector "dayEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayEntry:@
setDayEntry :: (IsMTRCommodityTariffClusterGetDayEntryResponseParams mtrCommodityTariffClusterGetDayEntryResponseParams, IsMTRCommodityTariffClusterDayEntryStruct value) => mtrCommodityTariffClusterGetDayEntryResponseParams -> value -> IO ()
setDayEntry mtrCommodityTariffClusterGetDayEntryResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterGetDayEntryResponseParams (mkSelector "setDayEntry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @dayEntry@
dayEntrySelector :: Selector
dayEntrySelector = mkSelector "dayEntry"

-- | @Selector@ for @setDayEntry:@
setDayEntrySelector :: Selector
setDayEntrySelector = mkSelector "setDayEntry:"

