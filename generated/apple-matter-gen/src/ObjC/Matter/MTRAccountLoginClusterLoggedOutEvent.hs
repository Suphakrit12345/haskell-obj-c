{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccountLoginClusterLoggedOutEvent@.
module ObjC.Matter.MTRAccountLoginClusterLoggedOutEvent
  ( MTRAccountLoginClusterLoggedOutEvent
  , IsMTRAccountLoginClusterLoggedOutEvent(..)
  , node
  , setNode
  , fabricIndex
  , setFabricIndex
  , nodeSelector
  , setNodeSelector
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

-- | @- node@
node :: IsMTRAccountLoginClusterLoggedOutEvent mtrAccountLoginClusterLoggedOutEvent => mtrAccountLoginClusterLoggedOutEvent -> IO (Id NSNumber)
node mtrAccountLoginClusterLoggedOutEvent  =
    sendMsg mtrAccountLoginClusterLoggedOutEvent (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNode:@
setNode :: (IsMTRAccountLoginClusterLoggedOutEvent mtrAccountLoginClusterLoggedOutEvent, IsNSNumber value) => mtrAccountLoginClusterLoggedOutEvent -> value -> IO ()
setNode mtrAccountLoginClusterLoggedOutEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterLoggedOutEvent (mkSelector "setNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccountLoginClusterLoggedOutEvent mtrAccountLoginClusterLoggedOutEvent => mtrAccountLoginClusterLoggedOutEvent -> IO (Id NSNumber)
fabricIndex mtrAccountLoginClusterLoggedOutEvent  =
    sendMsg mtrAccountLoginClusterLoggedOutEvent (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccountLoginClusterLoggedOutEvent mtrAccountLoginClusterLoggedOutEvent, IsNSNumber value) => mtrAccountLoginClusterLoggedOutEvent -> value -> IO ()
setFabricIndex mtrAccountLoginClusterLoggedOutEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccountLoginClusterLoggedOutEvent (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @setNode:@
setNodeSelector :: Selector
setNodeSelector = mkSelector "setNode:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

