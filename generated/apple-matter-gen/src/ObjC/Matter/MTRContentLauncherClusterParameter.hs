{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterParameter@.
module ObjC.Matter.MTRContentLauncherClusterParameter
  ( MTRContentLauncherClusterParameter
  , IsMTRContentLauncherClusterParameter(..)
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
type_ :: IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter => mtrContentLauncherClusterParameter -> IO (Id NSNumber)
type_ mtrContentLauncherClusterParameter  =
    sendMsg mtrContentLauncherClusterParameter (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter, IsNSNumber value) => mtrContentLauncherClusterParameter -> value -> IO ()
setType mtrContentLauncherClusterParameter  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterParameter (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter => mtrContentLauncherClusterParameter -> IO (Id NSString)
value mtrContentLauncherClusterParameter  =
    sendMsg mtrContentLauncherClusterParameter (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter, IsNSString value) => mtrContentLauncherClusterParameter -> value -> IO ()
setValue mtrContentLauncherClusterParameter  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterParameter (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- externalIDList@
externalIDList :: IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter => mtrContentLauncherClusterParameter -> IO (Id NSArray)
externalIDList mtrContentLauncherClusterParameter  =
    sendMsg mtrContentLauncherClusterParameter (mkSelector "externalIDList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRContentLauncherClusterParameter mtrContentLauncherClusterParameter, IsNSArray value) => mtrContentLauncherClusterParameter -> value -> IO ()
setExternalIDList mtrContentLauncherClusterParameter  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentLauncherClusterParameter (mkSelector "setExternalIDList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

