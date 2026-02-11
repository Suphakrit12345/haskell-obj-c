{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityPriceClusterGetDetailedForecastResponseParams@.
module ObjC.Matter.MTRCommodityPriceClusterGetDetailedForecastResponseParams
  ( MTRCommodityPriceClusterGetDetailedForecastResponseParams
  , IsMTRCommodityPriceClusterGetDetailedForecastResponseParams(..)
  , initWithResponseValue_error
  , priceForecast
  , setPriceForecast
  , initWithResponseValue_errorSelector
  , priceForecastSelector
  , setPriceForecastSelector


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

-- | Initialize an MTRCommodityPriceClusterGetDetailedForecastResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommodityPriceClusterGetDetailedForecastResponseParams mtrCommodityPriceClusterGetDetailedForecastResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommodityPriceClusterGetDetailedForecastResponseParams -> responseValue -> error_ -> IO (Id MTRCommodityPriceClusterGetDetailedForecastResponseParams)
initWithResponseValue_error mtrCommodityPriceClusterGetDetailedForecastResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCommodityPriceClusterGetDetailedForecastResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- priceForecast@
priceForecast :: IsMTRCommodityPriceClusterGetDetailedForecastResponseParams mtrCommodityPriceClusterGetDetailedForecastResponseParams => mtrCommodityPriceClusterGetDetailedForecastResponseParams -> IO (Id NSArray)
priceForecast mtrCommodityPriceClusterGetDetailedForecastResponseParams  =
    sendMsg mtrCommodityPriceClusterGetDetailedForecastResponseParams (mkSelector "priceForecast") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPriceForecast:@
setPriceForecast :: (IsMTRCommodityPriceClusterGetDetailedForecastResponseParams mtrCommodityPriceClusterGetDetailedForecastResponseParams, IsNSArray value) => mtrCommodityPriceClusterGetDetailedForecastResponseParams -> value -> IO ()
setPriceForecast mtrCommodityPriceClusterGetDetailedForecastResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommodityPriceClusterGetDetailedForecastResponseParams (mkSelector "setPriceForecast:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @priceForecast@
priceForecastSelector :: Selector
priceForecastSelector = mkSelector "priceForecast"

-- | @Selector@ for @setPriceForecast:@
setPriceForecastSelector :: Selector
setPriceForecastSelector = mkSelector "setPriceForecast:"

