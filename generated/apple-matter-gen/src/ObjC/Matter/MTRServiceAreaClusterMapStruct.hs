{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterMapStruct@.
module ObjC.Matter.MTRServiceAreaClusterMapStruct
  ( MTRServiceAreaClusterMapStruct
  , IsMTRServiceAreaClusterMapStruct(..)
  , mapID
  , setMapID
  , name
  , setName
  , mapIDSelector
  , setMapIDSelector
  , nameSelector
  , setNameSelector


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

-- | @- mapID@
mapID :: IsMTRServiceAreaClusterMapStruct mtrServiceAreaClusterMapStruct => mtrServiceAreaClusterMapStruct -> IO (Id NSNumber)
mapID mtrServiceAreaClusterMapStruct  =
    sendMsg mtrServiceAreaClusterMapStruct (mkSelector "mapID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMapID:@
setMapID :: (IsMTRServiceAreaClusterMapStruct mtrServiceAreaClusterMapStruct, IsNSNumber value) => mtrServiceAreaClusterMapStruct -> value -> IO ()
setMapID mtrServiceAreaClusterMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterMapStruct (mkSelector "setMapID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRServiceAreaClusterMapStruct mtrServiceAreaClusterMapStruct => mtrServiceAreaClusterMapStruct -> IO (Id NSString)
name mtrServiceAreaClusterMapStruct  =
    sendMsg mtrServiceAreaClusterMapStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRServiceAreaClusterMapStruct mtrServiceAreaClusterMapStruct, IsNSString value) => mtrServiceAreaClusterMapStruct -> value -> IO ()
setName mtrServiceAreaClusterMapStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterMapStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapID@
mapIDSelector :: Selector
mapIDSelector = mkSelector "mapID"

-- | @Selector@ for @setMapID:@
setMapIDSelector :: Selector
setMapIDSelector = mkSelector "setMapID:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

