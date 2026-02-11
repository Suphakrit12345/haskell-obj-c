{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessControlExtensionStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessControlExtensionStruct
  ( MTRAccessControlClusterAccessControlExtensionStruct
  , IsMTRAccessControlClusterAccessControlExtensionStruct(..)
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
data_ :: IsMTRAccessControlClusterAccessControlExtensionStruct mtrAccessControlClusterAccessControlExtensionStruct => mtrAccessControlClusterAccessControlExtensionStruct -> IO (Id NSData)
data_ mtrAccessControlClusterAccessControlExtensionStruct  =
    sendMsg mtrAccessControlClusterAccessControlExtensionStruct (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsMTRAccessControlClusterAccessControlExtensionStruct mtrAccessControlClusterAccessControlExtensionStruct, IsNSData value) => mtrAccessControlClusterAccessControlExtensionStruct -> value -> IO ()
setData mtrAccessControlClusterAccessControlExtensionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlExtensionStruct (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessControlExtensionStruct mtrAccessControlClusterAccessControlExtensionStruct => mtrAccessControlClusterAccessControlExtensionStruct -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessControlExtensionStruct  =
    sendMsg mtrAccessControlClusterAccessControlExtensionStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessControlExtensionStruct mtrAccessControlClusterAccessControlExtensionStruct, IsNSNumber value) => mtrAccessControlClusterAccessControlExtensionStruct -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessControlExtensionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessControlExtensionStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

