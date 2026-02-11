{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams@.
module ObjC.Matter.MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams
  ( MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams
  , IsMTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams(..)
  , initWithResponseValue_error
  , zoneID
  , setZoneID
  , initWithResponseValue_errorSelector
  , zoneIDSelector
  , setZoneIDSelector


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

-- | Initialize an MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams -> responseValue -> error_ -> IO (Id MTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams)
initWithResponseValue_error mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- zoneID@
zoneID :: IsMTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams => mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams -> IO (Id NSNumber)
zoneID mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams  =
    sendMsg mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setZoneID:@
setZoneID :: (IsMTRZoneManagementClusterCreateTwoDCartesianZoneResponseParams mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams, IsNSNumber value) => mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams -> value -> IO ()
setZoneID mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterCreateTwoDCartesianZoneResponseParams (mkSelector "setZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector
setZoneIDSelector = mkSelector "setZoneID:"

