{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadBorderRouterManagementClusterDatasetResponseParams@.
module ObjC.Matter.MTRThreadBorderRouterManagementClusterDatasetResponseParams
  ( MTRThreadBorderRouterManagementClusterDatasetResponseParams
  , IsMTRThreadBorderRouterManagementClusterDatasetResponseParams(..)
  , initWithResponseValue_error
  , dataset
  , setDataset
  , initWithResponseValue_errorSelector
  , datasetSelector
  , setDatasetSelector


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

-- | Initialize an MTRThreadBorderRouterManagementClusterDatasetResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThreadBorderRouterManagementClusterDatasetResponseParams mtrThreadBorderRouterManagementClusterDatasetResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThreadBorderRouterManagementClusterDatasetResponseParams -> responseValue -> error_ -> IO (Id MTRThreadBorderRouterManagementClusterDatasetResponseParams)
initWithResponseValue_error mtrThreadBorderRouterManagementClusterDatasetResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrThreadBorderRouterManagementClusterDatasetResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- dataset@
dataset :: IsMTRThreadBorderRouterManagementClusterDatasetResponseParams mtrThreadBorderRouterManagementClusterDatasetResponseParams => mtrThreadBorderRouterManagementClusterDatasetResponseParams -> IO (Id NSData)
dataset mtrThreadBorderRouterManagementClusterDatasetResponseParams  =
    sendMsg mtrThreadBorderRouterManagementClusterDatasetResponseParams (mkSelector "dataset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDataset:@
setDataset :: (IsMTRThreadBorderRouterManagementClusterDatasetResponseParams mtrThreadBorderRouterManagementClusterDatasetResponseParams, IsNSData value) => mtrThreadBorderRouterManagementClusterDatasetResponseParams -> value -> IO ()
setDataset mtrThreadBorderRouterManagementClusterDatasetResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadBorderRouterManagementClusterDatasetResponseParams (mkSelector "setDataset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @dataset@
datasetSelector :: Selector
datasetSelector = mkSelector "dataset"

-- | @Selector@ for @setDataset:@
setDatasetSelector :: Selector
setDatasetSelector = mkSelector "setDataset:"

