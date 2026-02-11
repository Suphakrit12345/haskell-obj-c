{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestFabricScopedEventEvent@.
module ObjC.Matter.MTRUnitTestingClusterTestFabricScopedEventEvent
  ( MTRUnitTestingClusterTestFabricScopedEventEvent
  , IsMTRUnitTestingClusterTestFabricScopedEventEvent(..)
  , fabricIndex
  , setFabricIndex
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- fabricIndex@
fabricIndex :: IsMTRUnitTestingClusterTestFabricScopedEventEvent mtrUnitTestingClusterTestFabricScopedEventEvent => mtrUnitTestingClusterTestFabricScopedEventEvent -> IO (Id NSNumber)
fabricIndex mtrUnitTestingClusterTestFabricScopedEventEvent  =
    sendMsg mtrUnitTestingClusterTestFabricScopedEventEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRUnitTestingClusterTestFabricScopedEventEvent mtrUnitTestingClusterTestFabricScopedEventEvent, IsNSNumber value) => mtrUnitTestingClusterTestFabricScopedEventEvent -> value -> IO ()
setFabricIndex mtrUnitTestingClusterTestFabricScopedEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterTestFabricScopedEventEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

