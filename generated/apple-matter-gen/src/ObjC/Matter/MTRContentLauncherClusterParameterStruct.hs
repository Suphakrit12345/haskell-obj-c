{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterParameterStruct@.
module ObjC.Matter.MTRContentLauncherClusterParameterStruct
  ( MTRContentLauncherClusterParameterStruct
  , IsMTRContentLauncherClusterParameterStruct(..)
  , type_
  , setType
  , value
  , setValue
  , externalIDList
  , setExternalIDList
  , typeSelector
  , setTypeSelector
  , valueSelector
  , setValueSelector
  , externalIDListSelector
  , setExternalIDListSelector


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

-- | @- type@
type_ :: IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct => mtrContentLauncherClusterParameterStruct -> IO (Id NSNumber)
type_ mtrContentLauncherClusterParameterStruct  =
    sendMsg mtrContentLauncherClusterParameterStruct (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct, IsNSNumber value) => mtrContentLauncherClusterParameterStruct -> value -> IO ()
setType mtrContentLauncherClusterParameterStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterParameterStruct (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct => mtrContentLauncherClusterParameterStruct -> IO (Id NSString)
value mtrContentLauncherClusterParameterStruct  =
    sendMsg mtrContentLauncherClusterParameterStruct (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct, IsNSString value) => mtrContentLauncherClusterParameterStruct -> value -> IO ()
setValue mtrContentLauncherClusterParameterStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterParameterStruct (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- externalIDList@
externalIDList :: IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct => mtrContentLauncherClusterParameterStruct -> IO (Id NSArray)
externalIDList mtrContentLauncherClusterParameterStruct  =
    sendMsg mtrContentLauncherClusterParameterStruct (mkSelector "externalIDList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRContentLauncherClusterParameterStruct mtrContentLauncherClusterParameterStruct, IsNSArray value) => mtrContentLauncherClusterParameterStruct -> value -> IO ()
setExternalIDList mtrContentLauncherClusterParameterStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterParameterStruct (mkSelector "setExternalIDList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @externalIDList@
externalIDListSelector :: Selector
externalIDListSelector = mkSelector "externalIDList"

-- | @Selector@ for @setExternalIDList:@
setExternalIDListSelector :: Selector
setExternalIDListSelector = mkSelector "setExternalIDList:"

