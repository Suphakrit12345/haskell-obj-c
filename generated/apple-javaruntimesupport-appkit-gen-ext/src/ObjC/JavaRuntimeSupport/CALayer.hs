{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The base layer class. *
--
-- Generated bindings for @CALayer@.
module ObjC.JavaRuntimeSupport.CALayer
  ( CALayer
  , IsCALayer(..)
  , createRemoteLayerBoundTo
  , hostRemoteLayer
  , createRemoteLayerBoundToSelector
  , hostRemoteLayerSelector


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

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createRemoteLayerBoundTo:@
createRemoteLayerBoundTo :: IsCALayer caLayer => caLayer -> CUInt -> IO (Id NSObject)
createRemoteLayerBoundTo caLayer  serverPort =
    sendMsg caLayer (mkSelector "createRemoteLayerBoundTo:") (retPtr retVoid) [argCUInt serverPort] >>= retainedObject . castPtr

-- | @- hostRemoteLayer:@
hostRemoteLayer :: IsCALayer caLayer => caLayer -> CUInt -> IO ()
hostRemoteLayer caLayer  layerID =
    sendMsg caLayer (mkSelector "hostRemoteLayer:") retVoid [argCUInt layerID]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createRemoteLayerBoundTo:@
createRemoteLayerBoundToSelector :: Selector
createRemoteLayerBoundToSelector = mkSelector "createRemoteLayerBoundTo:"

-- | @Selector@ for @hostRemoteLayer:@
hostRemoteLayerSelector :: Selector
hostRemoteLayerSelector = mkSelector "hostRemoteLayer:"

