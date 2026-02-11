{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterGetTariffComponentResponseParams@.
module ObjC.Matter.MTRCommodityTariffClusterGetTariffComponentResponseParams
  ( MTRCommodityTariffClusterGetTariffComponentResponseParams
  , IsMTRCommodityTariffClusterGetTariffComponentResponseParams(..)
  , initWithResponseValue_error
  , label
  , setLabel
  , dayEntryIDs
  , setDayEntryIDs
  , tariffComponent
  , setTariffComponent
  , initWithResponseValue_errorSelector
  , labelSelector
  , setLabelSelector
  , dayEntryIDsSelector
  , setDayEntryIDsSelector
  , tariffComponentSelector
  , setTariffComponentSelector


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

-- | Initialize an MTRCommodityTariffClusterGetTariffComponentResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommodityTariffClusterGetTariffComponentResponseParams -> responseValue -> error_ -> IO (Id MTRCommodityTariffClusterGetTariffComponentResponseParams)
initWithResponseValue_error mtrCommodityTariffClusterGetTariffComponentResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCommodityTariffClusterGetTariffComponentResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- label@
label :: IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams => mtrCommodityTariffClusterGetTariffComponentResponseParams -> IO (Id NSString)
label mtrCommodityTariffClusterGetTariffComponentResponseParams  =
    sendMsg mtrCommodityTariffClusterGetTariffComponentResponseParams (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams, IsNSString value) => mtrCommodityTariffClusterGetTariffComponentResponseParams -> value -> IO ()
setLabel mtrCommodityTariffClusterGetTariffComponentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterGetTariffComponentResponseParams (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dayEntryIDs@
dayEntryIDs :: IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams => mtrCommodityTariffClusterGetTariffComponentResponseParams -> IO (Id NSArray)
dayEntryIDs mtrCommodityTariffClusterGetTariffComponentResponseParams  =
    sendMsg mtrCommodityTariffClusterGetTariffComponentResponseParams (mkSelector "dayEntryIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDayEntryIDs:@
setDayEntryIDs :: (IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams, IsNSArray value) => mtrCommodityTariffClusterGetTariffComponentResponseParams -> value -> IO ()
setDayEntryIDs mtrCommodityTariffClusterGetTariffComponentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterGetTariffComponentResponseParams (mkSelector "setDayEntryIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tariffComponent@
tariffComponent :: IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams => mtrCommodityTariffClusterGetTariffComponentResponseParams -> IO (Id MTRCommodityTariffClusterTariffComponentStruct)
tariffComponent mtrCommodityTariffClusterGetTariffComponentResponseParams  =
    sendMsg mtrCommodityTariffClusterGetTariffComponentResponseParams (mkSelector "tariffComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTariffComponent:@
setTariffComponent :: (IsMTRCommodityTariffClusterGetTariffComponentResponseParams mtrCommodityTariffClusterGetTariffComponentResponseParams, IsMTRCommodityTariffClusterTariffComponentStruct value) => mtrCommodityTariffClusterGetTariffComponentResponseParams -> value -> IO ()
setTariffComponent mtrCommodityTariffClusterGetTariffComponentResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityTariffClusterGetTariffComponentResponseParams (mkSelector "setTariffComponent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @dayEntryIDs@
dayEntryIDsSelector :: Selector
dayEntryIDsSelector = mkSelector "dayEntryIDs"

-- | @Selector@ for @setDayEntryIDs:@
setDayEntryIDsSelector :: Selector
setDayEntryIDsSelector = mkSelector "setDayEntryIDs:"

-- | @Selector@ for @tariffComponent@
tariffComponentSelector :: Selector
tariffComponentSelector = mkSelector "tariffComponent"

-- | @Selector@ for @setTariffComponent:@
setTariffComponentSelector :: Selector
setTariffComponentSelector = mkSelector "setTariffComponent:"

