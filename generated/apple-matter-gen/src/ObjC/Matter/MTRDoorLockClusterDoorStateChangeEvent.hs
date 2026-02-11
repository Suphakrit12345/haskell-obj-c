{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterDoorStateChangeEvent@.
module ObjC.Matter.MTRDoorLockClusterDoorStateChangeEvent
  ( MTRDoorLockClusterDoorStateChangeEvent
  , IsMTRDoorLockClusterDoorStateChangeEvent(..)
  , doorState
  , setDoorState
  , doorStateSelector
  , setDoorStateSelector


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

-- | @- doorState@
doorState :: IsMTRDoorLockClusterDoorStateChangeEvent mtrDoorLockClusterDoorStateChangeEvent => mtrDoorLockClusterDoorStateChangeEvent -> IO (Id NSNumber)
doorState mtrDoorLockClusterDoorStateChangeEvent  =
    sendMsg mtrDoorLockClusterDoorStateChangeEvent (mkSelector "doorState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDoorState:@
setDoorState :: (IsMTRDoorLockClusterDoorStateChangeEvent mtrDoorLockClusterDoorStateChangeEvent, IsNSNumber value) => mtrDoorLockClusterDoorStateChangeEvent -> value -> IO ()
setDoorState mtrDoorLockClusterDoorStateChangeEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDoorLockClusterDoorStateChangeEvent (mkSelector "setDoorState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @doorState@
doorStateSelector :: Selector
doorStateSelector = mkSelector "doorState"

-- | @Selector@ for @setDoorState:@
setDoorStateSelector :: Selector
setDoorStateSelector = mkSelector "setDoorState:"

