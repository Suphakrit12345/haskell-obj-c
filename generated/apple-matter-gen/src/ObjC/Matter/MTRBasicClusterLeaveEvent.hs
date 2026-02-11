{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicClusterLeaveEvent@.
module ObjC.Matter.MTRBasicClusterLeaveEvent
  ( MTRBasicClusterLeaveEvent
  , IsMTRBasicClusterLeaveEvent(..)
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
fabricIndex :: IsMTRBasicClusterLeaveEvent mtrBasicClusterLeaveEvent => mtrBasicClusterLeaveEvent -> IO (Id NSNumber)
fabricIndex mtrBasicClusterLeaveEvent  =
    sendMsg mtrBasicClusterLeaveEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRBasicClusterLeaveEvent mtrBasicClusterLeaveEvent, IsNSNumber value) => mtrBasicClusterLeaveEvent -> value -> IO ()
setFabricIndex mtrBasicClusterLeaveEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicClusterLeaveEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

