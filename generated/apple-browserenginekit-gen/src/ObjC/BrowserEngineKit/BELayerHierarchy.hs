{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BELayerHierarchy@.
module ObjC.BrowserEngineKit.BELayerHierarchy
  ( BELayerHierarchy
  , IsBELayerHierarchy(..)
  , init_
  , new
  , layerHierarchyWithError
  , invalidate
  , initSelector
  , newSelector
  , layerHierarchyWithErrorSelector
  , invalidateSelector


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

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO (Id BELayerHierarchy)
init_ beLayerHierarchy  =
    sendMsg beLayerHierarchy (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BELayerHierarchy)
new  =
  do
    cls' <- getRequiredClass "BELayerHierarchy"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | may fail if a connection to the render server cannot be established
--
-- ObjC selector: @+ layerHierarchyWithError:@
layerHierarchyWithError :: IsNSError error_ => error_ -> IO (Id BELayerHierarchy)
layerHierarchyWithError error_ =
  do
    cls' <- getRequiredClass "BELayerHierarchy"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "layerHierarchyWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | invalidate must be called before this layer hierarchy is disposed of
--
-- ObjC selector: @- invalidate@
invalidate :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO ()
invalidate beLayerHierarchy  =
    sendMsg beLayerHierarchy (mkSelector "invalidate") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @layerHierarchyWithError:@
layerHierarchyWithErrorSelector :: Selector
layerHierarchyWithErrorSelector = mkSelector "layerHierarchyWithError:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

