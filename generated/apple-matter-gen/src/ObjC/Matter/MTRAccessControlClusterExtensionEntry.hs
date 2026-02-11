{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterExtensionEntry@.
module ObjC.Matter.MTRAccessControlClusterExtensionEntry
  ( MTRAccessControlClusterExtensionEntry
  , IsMTRAccessControlClusterExtensionEntry(..)
  , data_
  , setData
  , fabricIndex
  , setFabricIndex
  , dataSelector
  , setDataSelector
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

-- | @- data@
data_ :: IsMTRAccessControlClusterExtensionEntry mtrAccessControlClusterExtensionEntry => mtrAccessControlClusterExtensionEntry -> IO (Id NSData)
data_ mtrAccessControlClusterExtensionEntry  =
    sendMsg mtrAccessControlClusterExtensionEntry (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRAccessControlClusterExtensionEntry mtrAccessControlClusterExtensionEntry, IsNSData value) => mtrAccessControlClusterExtensionEntry -> value -> IO ()
setData mtrAccessControlClusterExtensionEntry  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterExtensionEntry (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterExtensionEntry mtrAccessControlClusterExtensionEntry => mtrAccessControlClusterExtensionEntry -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterExtensionEntry  =
    sendMsg mtrAccessControlClusterExtensionEntry (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterExtensionEntry mtrAccessControlClusterExtensionEntry, IsNSNumber value) => mtrAccessControlClusterExtensionEntry -> value -> IO ()
setFabricIndex mtrAccessControlClusterExtensionEntry  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterExtensionEntry (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

