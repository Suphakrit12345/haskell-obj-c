{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A base type that identifies containers and volumes.
--
-- An ``FSEntityIdentifier`` is a UUID to identify a container or volume, optionally with eight bytes of qualifying (differentiating) data. You use the qualifiers in cases in which a file server can receive multiple connections from the same client, which differ by user credentials. In this case, the identifier for each client is the server's base UUID, and a unique qualifier that differs by client.
--
-- > Important: Don't subclass this class.
--
-- Generated bindings for @FSEntityIdentifier@.
module ObjC.FSKit.FSEntityIdentifier
  ( FSEntityIdentifier
  , IsFSEntityIdentifier(..)
  , init_
  , initWithUUID
  , initWithUUID_qualifier
  , initWithUUID_data
  , uuid
  , setUuid
  , qualifier
  , setQualifier
  , initSelector
  , initWithUUIDSelector
  , initWithUUID_qualifierSelector
  , initWithUUID_dataSelector
  , uuidSelector
  , setUuidSelector
  , qualifierSelector
  , setQualifierSelector


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

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an entity identifier with a random UUID.
--
-- ObjC selector: @- init@
init_ :: IsFSEntityIdentifier fsEntityIdentifier => fsEntityIdentifier -> IO (Id FSEntityIdentifier)
init_ fsEntityIdentifier  =
    sendMsg fsEntityIdentifier (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates an entity identifier with the given UUID.
--
-- - Parameter uuid: The UUID to use for this identifier.
--
-- ObjC selector: @- initWithUUID:@
initWithUUID :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSUUID uuid) => fsEntityIdentifier -> uuid -> IO (Id FSEntityIdentifier)
initWithUUID fsEntityIdentifier  uuid =
  withObjCPtr uuid $ \raw_uuid ->
      sendMsg fsEntityIdentifier (mkSelector "initWithUUID:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ())] >>= ownedObject . castPtr

-- | Creates an entity identifier with the given UUID and qualifier data as a 64-bit unsigned integer. - Parameters:   - uuid: The UUID to use for this identifier.   - qualifier: The data to distinguish entities that otherwise share the same UUID.
--
-- ObjC selector: @- initWithUUID:qualifier:@
initWithUUID_qualifier :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSUUID uuid) => fsEntityIdentifier -> uuid -> CULong -> IO (Id FSEntityIdentifier)
initWithUUID_qualifier fsEntityIdentifier  uuid qualifier =
  withObjCPtr uuid $ \raw_uuid ->
      sendMsg fsEntityIdentifier (mkSelector "initWithUUID:qualifier:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argCULong qualifier] >>= ownedObject . castPtr

-- | Creates an entity identifier with the given UUID and qualifier data.
--
-- - Parameters:   - uuid: The UUID to use for this identifier.   - qualifierData: The data to distinguish entities that otherwise share the same UUID.
--
-- ObjC selector: @- initWithUUID:data:@
initWithUUID_data :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSUUID uuid, IsNSData qualifierData) => fsEntityIdentifier -> uuid -> qualifierData -> IO (Id FSEntityIdentifier)
initWithUUID_data fsEntityIdentifier  uuid qualifierData =
  withObjCPtr uuid $ \raw_uuid ->
    withObjCPtr qualifierData $ \raw_qualifierData ->
        sendMsg fsEntityIdentifier (mkSelector "initWithUUID:data:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_qualifierData :: Ptr ())] >>= ownedObject . castPtr

-- | A UUID to uniquely identify this entity.
--
-- ObjC selector: @- uuid@
uuid :: IsFSEntityIdentifier fsEntityIdentifier => fsEntityIdentifier -> IO (Id NSUUID)
uuid fsEntityIdentifier  =
    sendMsg fsEntityIdentifier (mkSelector "uuid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A UUID to uniquely identify this entity.
--
-- ObjC selector: @- setUuid:@
setUuid :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSUUID value) => fsEntityIdentifier -> value -> IO ()
setUuid fsEntityIdentifier  value =
  withObjCPtr value $ \raw_value ->
      sendMsg fsEntityIdentifier (mkSelector "setUuid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An optional piece of data to distinguish entities that otherwise share the same UUID.
--
-- ObjC selector: @- qualifier@
qualifier :: IsFSEntityIdentifier fsEntityIdentifier => fsEntityIdentifier -> IO (Id NSData)
qualifier fsEntityIdentifier  =
    sendMsg fsEntityIdentifier (mkSelector "qualifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional piece of data to distinguish entities that otherwise share the same UUID.
--
-- ObjC selector: @- setQualifier:@
setQualifier :: (IsFSEntityIdentifier fsEntityIdentifier, IsNSData value) => fsEntityIdentifier -> value -> IO ()
setQualifier fsEntityIdentifier  value =
  withObjCPtr value $ \raw_value ->
      sendMsg fsEntityIdentifier (mkSelector "setQualifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithUUID:@
initWithUUIDSelector :: Selector
initWithUUIDSelector = mkSelector "initWithUUID:"

-- | @Selector@ for @initWithUUID:qualifier:@
initWithUUID_qualifierSelector :: Selector
initWithUUID_qualifierSelector = mkSelector "initWithUUID:qualifier:"

-- | @Selector@ for @initWithUUID:data:@
initWithUUID_dataSelector :: Selector
initWithUUID_dataSelector = mkSelector "initWithUUID:data:"

-- | @Selector@ for @uuid@
uuidSelector :: Selector
uuidSelector = mkSelector "uuid"

-- | @Selector@ for @setUuid:@
setUuidSelector :: Selector
setUuidSelector = mkSelector "setUuid:"

-- | @Selector@ for @qualifier@
qualifierSelector :: Selector
qualifierSelector = mkSelector "qualifier"

-- | @Selector@ for @setQualifier:@
setQualifierSelector :: Selector
setQualifierSelector = mkSelector "setQualifier:"

