{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterSceneInfoStruct@.
module ObjC.Matter.MTRScenesManagementClusterSceneInfoStruct
  ( MTRScenesManagementClusterSceneInfoStruct
  , IsMTRScenesManagementClusterSceneInfoStruct(..)
  , sceneCount
  , setSceneCount
  , currentScene
  , setCurrentScene
  , currentGroup
  , setCurrentGroup
  , sceneValid
  , setSceneValid
  , remainingCapacity
  , setRemainingCapacity
  , fabricIndex
  , setFabricIndex
  , sceneCountSelector
  , setSceneCountSelector
  , currentSceneSelector
  , setCurrentSceneSelector
  , currentGroupSelector
  , setCurrentGroupSelector
  , sceneValidSelector
  , setSceneValidSelector
  , remainingCapacitySelector
  , setRemainingCapacitySelector
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

-- | @- sceneCount@
sceneCount :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
sceneCount mtrScenesManagementClusterSceneInfoStruct  =
    sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "sceneCount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneCount:@
setSceneCount :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setSceneCount mtrScenesManagementClusterSceneInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "setSceneCount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentScene@
currentScene :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
currentScene mtrScenesManagementClusterSceneInfoStruct  =
    sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "currentScene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentScene:@
setCurrentScene :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setCurrentScene mtrScenesManagementClusterSceneInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "setCurrentScene:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currentGroup@
currentGroup :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
currentGroup mtrScenesManagementClusterSceneInfoStruct  =
    sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "currentGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentGroup:@
setCurrentGroup :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setCurrentGroup mtrScenesManagementClusterSceneInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "setCurrentGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sceneValid@
sceneValid :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
sceneValid mtrScenesManagementClusterSceneInfoStruct  =
    sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "sceneValid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSceneValid:@
setSceneValid :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setSceneValid mtrScenesManagementClusterSceneInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "setSceneValid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- remainingCapacity@
remainingCapacity :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
remainingCapacity mtrScenesManagementClusterSceneInfoStruct  =
    sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "remainingCapacity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRemainingCapacity:@
setRemainingCapacity :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setRemainingCapacity mtrScenesManagementClusterSceneInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "setRemainingCapacity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
fabricIndex mtrScenesManagementClusterSceneInfoStruct  =
    sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setFabricIndex mtrScenesManagementClusterSceneInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterSceneInfoStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sceneCount@
sceneCountSelector :: Selector
sceneCountSelector = mkSelector "sceneCount"

-- | @Selector@ for @setSceneCount:@
setSceneCountSelector :: Selector
setSceneCountSelector = mkSelector "setSceneCount:"

-- | @Selector@ for @currentScene@
currentSceneSelector :: Selector
currentSceneSelector = mkSelector "currentScene"

-- | @Selector@ for @setCurrentScene:@
setCurrentSceneSelector :: Selector
setCurrentSceneSelector = mkSelector "setCurrentScene:"

-- | @Selector@ for @currentGroup@
currentGroupSelector :: Selector
currentGroupSelector = mkSelector "currentGroup"

-- | @Selector@ for @setCurrentGroup:@
setCurrentGroupSelector :: Selector
setCurrentGroupSelector = mkSelector "setCurrentGroup:"

-- | @Selector@ for @sceneValid@
sceneValidSelector :: Selector
sceneValidSelector = mkSelector "sceneValid"

-- | @Selector@ for @setSceneValid:@
setSceneValidSelector :: Selector
setSceneValidSelector = mkSelector "setSceneValid:"

-- | @Selector@ for @remainingCapacity@
remainingCapacitySelector :: Selector
remainingCapacitySelector = mkSelector "remainingCapacity"

-- | @Selector@ for @setRemainingCapacity:@
setRemainingCapacitySelector :: Selector
setRemainingCapacitySelector = mkSelector "setRemainingCapacity:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

