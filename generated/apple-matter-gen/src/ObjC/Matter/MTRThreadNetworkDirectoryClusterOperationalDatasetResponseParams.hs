{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams@.
module ObjC.Matter.MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams
  ( MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams
  , IsMTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams(..)
  , initWithResponseValue_error
  , operationalDataset
  , setOperationalDataset
  , initWithResponseValue_errorSelector
  , operationalDatasetSelector
  , setOperationalDatasetSelector


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

-- | Initialize an MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams -> responseValue -> error_ -> IO (Id MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams)
initWithResponseValue_error mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- operationalDataset@
operationalDataset :: IsMTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams => mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams -> IO (Id NSData)
operationalDataset mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams  =
    sendMsg mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams (mkSelector "operationalDataset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationalDataset:@
setOperationalDataset :: (IsMTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams, IsNSData value) => mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams -> value -> IO ()
setOperationalDataset mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams (mkSelector "setOperationalDataset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @operationalDataset@
operationalDatasetSelector :: Selector
operationalDatasetSelector = mkSelector "operationalDataset"

-- | @Selector@ for @setOperationalDataset:@
setOperationalDatasetSelector :: Selector
setOperationalDatasetSelector = mkSelector "setOperationalDataset:"

