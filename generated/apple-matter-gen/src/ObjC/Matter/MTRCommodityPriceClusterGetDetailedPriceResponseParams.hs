{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterGetDetailedPriceResponseParams@.
module ObjC.Matter.MTRCommodityPriceClusterGetDetailedPriceResponseParams
  ( MTRCommodityPriceClusterGetDetailedPriceResponseParams
  , IsMTRCommodityPriceClusterGetDetailedPriceResponseParams(..)
  , initWithResponseValue_error
  , currentPrice
  , setCurrentPrice
  , initWithResponseValue_errorSelector
  , currentPriceSelector
  , setCurrentPriceSelector


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

-- | Initialize an MTRCommodityPriceClusterGetDetailedPriceResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommodityPriceClusterGetDetailedPriceResponseParams mtrCommodityPriceClusterGetDetailedPriceResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommodityPriceClusterGetDetailedPriceResponseParams -> responseValue -> error_ -> IO (Id MTRCommodityPriceClusterGetDetailedPriceResponseParams)
initWithResponseValue_error mtrCommodityPriceClusterGetDetailedPriceResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCommodityPriceClusterGetDetailedPriceResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- currentPrice@
currentPrice :: IsMTRCommodityPriceClusterGetDetailedPriceResponseParams mtrCommodityPriceClusterGetDetailedPriceResponseParams => mtrCommodityPriceClusterGetDetailedPriceResponseParams -> IO (Id MTRCommodityPriceClusterCommodityPriceStruct)
currentPrice mtrCommodityPriceClusterGetDetailedPriceResponseParams  =
    sendMsg mtrCommodityPriceClusterGetDetailedPriceResponseParams (mkSelector "currentPrice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentPrice:@
setCurrentPrice :: (IsMTRCommodityPriceClusterGetDetailedPriceResponseParams mtrCommodityPriceClusterGetDetailedPriceResponseParams, IsMTRCommodityPriceClusterCommodityPriceStruct value) => mtrCommodityPriceClusterGetDetailedPriceResponseParams -> value -> IO ()
setCurrentPrice mtrCommodityPriceClusterGetDetailedPriceResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterGetDetailedPriceResponseParams (mkSelector "setCurrentPrice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @currentPrice@
currentPriceSelector :: Selector
currentPriceSelector = mkSelector "currentPrice"

-- | @Selector@ for @setCurrentPrice:@
setCurrentPriceSelector :: Selector
setCurrentPriceSelector = mkSelector "setCurrentPrice:"

