{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterGetTargetsResponseParams@.
module ObjC.Matter.MTREnergyEVSEClusterGetTargetsResponseParams
  ( MTREnergyEVSEClusterGetTargetsResponseParams
  , IsMTREnergyEVSEClusterGetTargetsResponseParams(..)
  , initWithResponseValue_error
  , chargingTargetSchedules
  , setChargingTargetSchedules
  , initWithResponseValue_errorSelector
  , chargingTargetSchedulesSelector
  , setChargingTargetSchedulesSelector


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

-- | Initialize an MTREnergyEVSEClusterGetTargetsResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTREnergyEVSEClusterGetTargetsResponseParams mtrEnergyEVSEClusterGetTargetsResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrEnergyEVSEClusterGetTargetsResponseParams -> responseValue -> error_ -> IO (Id MTREnergyEVSEClusterGetTargetsResponseParams)
initWithResponseValue_error mtrEnergyEVSEClusterGetTargetsResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrEnergyEVSEClusterGetTargetsResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- chargingTargetSchedules@
chargingTargetSchedules :: IsMTREnergyEVSEClusterGetTargetsResponseParams mtrEnergyEVSEClusterGetTargetsResponseParams => mtrEnergyEVSEClusterGetTargetsResponseParams -> IO (Id NSArray)
chargingTargetSchedules mtrEnergyEVSEClusterGetTargetsResponseParams  =
    sendMsg mtrEnergyEVSEClusterGetTargetsResponseParams (mkSelector "chargingTargetSchedules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChargingTargetSchedules:@
setChargingTargetSchedules :: (IsMTREnergyEVSEClusterGetTargetsResponseParams mtrEnergyEVSEClusterGetTargetsResponseParams, IsNSArray value) => mtrEnergyEVSEClusterGetTargetsResponseParams -> value -> IO ()
setChargingTargetSchedules mtrEnergyEVSEClusterGetTargetsResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyEVSEClusterGetTargetsResponseParams (mkSelector "setChargingTargetSchedules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @chargingTargetSchedules@
chargingTargetSchedulesSelector :: Selector
chargingTargetSchedulesSelector = mkSelector "chargingTargetSchedules"

-- | @Selector@ for @setChargingTargetSchedules:@
setChargingTargetSchedulesSelector :: Selector
setChargingTargetSchedulesSelector = mkSelector "setChargingTargetSchedules:"

