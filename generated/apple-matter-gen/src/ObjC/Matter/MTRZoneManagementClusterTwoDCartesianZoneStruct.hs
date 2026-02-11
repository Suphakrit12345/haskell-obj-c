{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterTwoDCartesianZoneStruct@.
module ObjC.Matter.MTRZoneManagementClusterTwoDCartesianZoneStruct
  ( MTRZoneManagementClusterTwoDCartesianZoneStruct
  , IsMTRZoneManagementClusterTwoDCartesianZoneStruct(..)
  , name
  , setName
  , use
  , setUse
  , vertices
  , setVertices
  , color
  , setColor
  , nameSelector
  , setNameSelector
  , useSelector
  , setUseSelector
  , verticesSelector
  , setVerticesSelector
  , colorSelector
  , setColorSelector


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

-- | @- name@
name :: IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct => mtrZoneManagementClusterTwoDCartesianZoneStruct -> IO (Id NSString)
name mtrZoneManagementClusterTwoDCartesianZoneStruct  =
    sendMsg mtrZoneManagementClusterTwoDCartesianZoneStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct, IsNSString value) => mtrZoneManagementClusterTwoDCartesianZoneStruct -> value -> IO ()
setName mtrZoneManagementClusterTwoDCartesianZoneStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterTwoDCartesianZoneStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- use@
use :: IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct => mtrZoneManagementClusterTwoDCartesianZoneStruct -> IO (Id NSNumber)
use mtrZoneManagementClusterTwoDCartesianZoneStruct  =
    sendMsg mtrZoneManagementClusterTwoDCartesianZoneStruct (mkSelector "use") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUse:@
setUse :: (IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct, IsNSNumber value) => mtrZoneManagementClusterTwoDCartesianZoneStruct -> value -> IO ()
setUse mtrZoneManagementClusterTwoDCartesianZoneStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterTwoDCartesianZoneStruct (mkSelector "setUse:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vertices@
vertices :: IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct => mtrZoneManagementClusterTwoDCartesianZoneStruct -> IO (Id NSArray)
vertices mtrZoneManagementClusterTwoDCartesianZoneStruct  =
    sendMsg mtrZoneManagementClusterTwoDCartesianZoneStruct (mkSelector "vertices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVertices:@
setVertices :: (IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct, IsNSArray value) => mtrZoneManagementClusterTwoDCartesianZoneStruct -> value -> IO ()
setVertices mtrZoneManagementClusterTwoDCartesianZoneStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterTwoDCartesianZoneStruct (mkSelector "setVertices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- color@
color :: IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct => mtrZoneManagementClusterTwoDCartesianZoneStruct -> IO (Id NSString)
color mtrZoneManagementClusterTwoDCartesianZoneStruct  =
    sendMsg mtrZoneManagementClusterTwoDCartesianZoneStruct (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColor:@
setColor :: (IsMTRZoneManagementClusterTwoDCartesianZoneStruct mtrZoneManagementClusterTwoDCartesianZoneStruct, IsNSString value) => mtrZoneManagementClusterTwoDCartesianZoneStruct -> value -> IO ()
setColor mtrZoneManagementClusterTwoDCartesianZoneStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrZoneManagementClusterTwoDCartesianZoneStruct (mkSelector "setColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @use@
useSelector :: Selector
useSelector = mkSelector "use"

-- | @Selector@ for @setUse:@
setUseSelector :: Selector
setUseSelector = mkSelector "setUse:"

-- | @Selector@ for @vertices@
verticesSelector :: Selector
verticesSelector = mkSelector "vertices"

-- | @Selector@ for @setVertices:@
setVerticesSelector :: Selector
setVerticesSelector = mkSelector "setVertices:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

