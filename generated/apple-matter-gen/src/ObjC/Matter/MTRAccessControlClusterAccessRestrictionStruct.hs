{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessRestrictionStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessRestrictionStruct
  ( MTRAccessControlClusterAccessRestrictionStruct
  , IsMTRAccessControlClusterAccessRestrictionStruct(..)
  , type_
  , setType
  , id_
  , setId
  , typeSelector
  , setTypeSelector
  , idSelector
  , setIdSelector


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
type_ :: IsMTRAccessControlClusterAccessRestrictionStruct mtrAccessControlClusterAccessRestrictionStruct => mtrAccessControlClusterAccessRestrictionStruct -> IO (Id NSNumber)
type_ mtrAccessControlClusterAccessRestrictionStruct  =
    sendMsg mtrAccessControlClusterAccessRestrictionStruct (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsMTRAccessControlClusterAccessRestrictionStruct mtrAccessControlClusterAccessRestrictionStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionStruct -> value -> IO ()
setType mtrAccessControlClusterAccessRestrictionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessRestrictionStruct (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- id@
id_ :: IsMTRAccessControlClusterAccessRestrictionStruct mtrAccessControlClusterAccessRestrictionStruct => mtrAccessControlClusterAccessRestrictionStruct -> IO (Id NSNumber)
id_ mtrAccessControlClusterAccessRestrictionStruct  =
    sendMsg mtrAccessControlClusterAccessRestrictionStruct (mkSelector "id") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setId:@
setId :: (IsMTRAccessControlClusterAccessRestrictionStruct mtrAccessControlClusterAccessRestrictionStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionStruct -> value -> IO ()
setId mtrAccessControlClusterAccessRestrictionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessRestrictionStruct (mkSelector "setId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @id@
idSelector :: Selector
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector
setIdSelector = mkSelector "setId:"

