{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base for all MTRCluster* types.
--
-- Generated bindings for @MTRGenericCluster@.
module ObjC.Matter.MTRGenericCluster
  ( MTRGenericCluster
  , IsMTRGenericCluster(..)
  , device
  , deviceSelector


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

-- | The device this cluster object is associated with.
--
-- ObjC selector: @- device@
device :: IsMTRGenericCluster mtrGenericCluster => mtrGenericCluster -> IO (Id MTRDevice)
device mtrGenericCluster  =
    sendMsg mtrGenericCluster (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

