{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSoftwareDiagnosticsClusterThreadMetricsStruct@.
module ObjC.Matter.MTRSoftwareDiagnosticsClusterThreadMetricsStruct
  ( MTRSoftwareDiagnosticsClusterThreadMetricsStruct
  , IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct(..)
  , id_
  , setId
  , name
  , setName
  , stackFreeCurrent
  , setStackFreeCurrent
  , stackFreeMinimum
  , setStackFreeMinimum
  , stackSize
  , setStackSize
  , idSelector
  , setIdSelector
  , nameSelector
  , setNameSelector
  , stackFreeCurrentSelector
  , setStackFreeCurrentSelector
  , stackFreeMinimumSelector
  , setStackFreeMinimumSelector
  , stackSizeSelector
  , setStackSizeSelector


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

-- | @- id@
id_ :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSNumber)
id_ mtrSoftwareDiagnosticsClusterThreadMetricsStruct  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "id") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setId:@
setId :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setId mtrSoftwareDiagnosticsClusterThreadMetricsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "setId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSString)
name mtrSoftwareDiagnosticsClusterThreadMetricsStruct  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSString value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setName mtrSoftwareDiagnosticsClusterThreadMetricsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stackFreeCurrent@
stackFreeCurrent :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSNumber)
stackFreeCurrent mtrSoftwareDiagnosticsClusterThreadMetricsStruct  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "stackFreeCurrent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStackFreeCurrent:@
setStackFreeCurrent :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setStackFreeCurrent mtrSoftwareDiagnosticsClusterThreadMetricsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "setStackFreeCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stackFreeMinimum@
stackFreeMinimum :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSNumber)
stackFreeMinimum mtrSoftwareDiagnosticsClusterThreadMetricsStruct  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "stackFreeMinimum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStackFreeMinimum:@
setStackFreeMinimum :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setStackFreeMinimum mtrSoftwareDiagnosticsClusterThreadMetricsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "setStackFreeMinimum:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stackSize@
stackSize :: IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> IO (Id NSNumber)
stackSize mtrSoftwareDiagnosticsClusterThreadMetricsStruct  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "stackSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStackSize:@
setStackSize :: (IsMTRSoftwareDiagnosticsClusterThreadMetricsStruct mtrSoftwareDiagnosticsClusterThreadMetricsStruct, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetricsStruct -> value -> IO ()
setStackSize mtrSoftwareDiagnosticsClusterThreadMetricsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetricsStruct (mkSelector "setStackSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @stackFreeCurrent@
stackFreeCurrentSelector :: Selector
stackFreeCurrentSelector = mkSelector "stackFreeCurrent"

-- | @Selector@ for @setStackFreeCurrent:@
setStackFreeCurrentSelector :: Selector
setStackFreeCurrentSelector = mkSelector "setStackFreeCurrent:"

-- | @Selector@ for @stackFreeMinimum@
stackFreeMinimumSelector :: Selector
stackFreeMinimumSelector = mkSelector "stackFreeMinimum"

-- | @Selector@ for @setStackFreeMinimum:@
setStackFreeMinimumSelector :: Selector
setStackFreeMinimumSelector = mkSelector "setStackFreeMinimum:"

-- | @Selector@ for @stackSize@
stackSizeSelector :: Selector
stackSizeSelector = mkSelector "stackSize"

-- | @Selector@ for @setStackSize:@
setStackSizeSelector :: Selector
setStackSizeSelector = mkSelector "setStackSize:"

