{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A path indicating a specific cluster on a device (i.e. without any wildcards).
--
-- Generated bindings for @MTRClusterPath@.
module ObjC.Matter.MTRClusterPath
  ( MTRClusterPath
  , IsMTRClusterPath(..)
  , clusterPathWithEndpointID_clusterID
  , init_
  , new
  , endpoint
  , cluster
  , clusterPathWithEndpointID_clusterIDSelector
  , initSelector
  , newSelector
  , endpointSelector
  , clusterSelector


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

-- | @+ clusterPathWithEndpointID:clusterID:@
clusterPathWithEndpointID_clusterID :: (IsNSNumber endpointID, IsNSNumber clusterID) => endpointID -> clusterID -> IO (Id MTRClusterPath)
clusterPathWithEndpointID_clusterID endpointID clusterID =
  do
    cls' <- getRequiredClass "MTRClusterPath"
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr clusterID $ \raw_clusterID ->
        sendClassMsg cls' (mkSelector "clusterPathWithEndpointID:clusterID:") (retPtr retVoid) [argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_clusterID :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterPath mtrClusterPath => mtrClusterPath -> IO (Id MTRClusterPath)
init_ mtrClusterPath  =
    sendMsg mtrClusterPath (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterPath)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPath"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- endpoint@
endpoint :: IsMTRClusterPath mtrClusterPath => mtrClusterPath -> IO (Id NSNumber)
endpoint mtrClusterPath  =
    sendMsg mtrClusterPath (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cluster@
cluster :: IsMTRClusterPath mtrClusterPath => mtrClusterPath -> IO (Id NSNumber)
cluster mtrClusterPath  =
    sendMsg mtrClusterPath (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clusterPathWithEndpointID:clusterID:@
clusterPathWithEndpointID_clusterIDSelector :: Selector
clusterPathWithEndpointID_clusterIDSelector = mkSelector "clusterPathWithEndpointID:clusterID:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @cluster@
clusterSelector :: Selector
clusterSelector = mkSelector "cluster"

