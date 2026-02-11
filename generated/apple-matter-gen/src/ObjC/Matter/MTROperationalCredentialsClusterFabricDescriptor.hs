{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterFabricDescriptor@.
module ObjC.Matter.MTROperationalCredentialsClusterFabricDescriptor
  ( MTROperationalCredentialsClusterFabricDescriptor
  , IsMTROperationalCredentialsClusterFabricDescriptor(..)
  , rootPublicKey
  , setRootPublicKey
  , label
  , setLabel
  , fabricIndex
  , setFabricIndex
  , rootPublicKeySelector
  , setRootPublicKeySelector
  , labelSelector
  , setLabelSelector
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

-- | @- rootPublicKey@
rootPublicKey :: IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor => mtrOperationalCredentialsClusterFabricDescriptor -> IO (Id NSData)
rootPublicKey mtrOperationalCredentialsClusterFabricDescriptor  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptor (mkSelector "rootPublicKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRootPublicKey:@
setRootPublicKey :: (IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor, IsNSData value) => mtrOperationalCredentialsClusterFabricDescriptor -> value -> IO ()
setRootPublicKey mtrOperationalCredentialsClusterFabricDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptor (mkSelector "setRootPublicKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor => mtrOperationalCredentialsClusterFabricDescriptor -> IO (Id NSString)
label mtrOperationalCredentialsClusterFabricDescriptor  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor, IsNSString value) => mtrOperationalCredentialsClusterFabricDescriptor -> value -> IO ()
setLabel mtrOperationalCredentialsClusterFabricDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor => mtrOperationalCredentialsClusterFabricDescriptor -> IO (Id NSNumber)
fabricIndex mtrOperationalCredentialsClusterFabricDescriptor  =
    sendMsg mtrOperationalCredentialsClusterFabricDescriptor (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTROperationalCredentialsClusterFabricDescriptor mtrOperationalCredentialsClusterFabricDescriptor, IsNSNumber value) => mtrOperationalCredentialsClusterFabricDescriptor -> value -> IO ()
setFabricIndex mtrOperationalCredentialsClusterFabricDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalCredentialsClusterFabricDescriptor (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rootPublicKey@
rootPublicKeySelector :: Selector
rootPublicKeySelector = mkSelector "rootPublicKey"

-- | @Selector@ for @setRootPublicKey:@
setRootPublicKeySelector :: Selector
setRootPublicKeySelector = mkSelector "setRootPublicKey:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

