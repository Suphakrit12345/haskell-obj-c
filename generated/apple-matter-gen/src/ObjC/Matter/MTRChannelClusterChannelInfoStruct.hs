{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterChannelInfoStruct@.
module ObjC.Matter.MTRChannelClusterChannelInfoStruct
  ( MTRChannelClusterChannelInfoStruct
  , IsMTRChannelClusterChannelInfoStruct(..)
  , majorNumber
  , setMajorNumber
  , minorNumber
  , setMinorNumber
  , name
  , setName
  , callSign
  , setCallSign
  , affiliateCallSign
  , setAffiliateCallSign
  , identifier
  , setIdentifier
  , type_
  , setType
  , majorNumberSelector
  , setMajorNumberSelector
  , minorNumberSelector
  , setMinorNumberSelector
  , nameSelector
  , setNameSelector
  , callSignSelector
  , setCallSignSelector
  , affiliateCallSignSelector
  , setAffiliateCallSignSelector
  , identifierSelector
  , setIdentifierSelector
  , typeSelector
  , setTypeSelector


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

-- | @- majorNumber@
majorNumber :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSNumber)
majorNumber mtrChannelClusterChannelInfoStruct  =
    sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "majorNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMajorNumber:@
setMajorNumber :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSNumber value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setMajorNumber mtrChannelClusterChannelInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "setMajorNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minorNumber@
minorNumber :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSNumber)
minorNumber mtrChannelClusterChannelInfoStruct  =
    sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "minorNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinorNumber:@
setMinorNumber :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSNumber value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setMinorNumber mtrChannelClusterChannelInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "setMinorNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSString)
name mtrChannelClusterChannelInfoStruct  =
    sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSString value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setName mtrChannelClusterChannelInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- callSign@
callSign :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSString)
callSign mtrChannelClusterChannelInfoStruct  =
    sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "callSign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCallSign:@
setCallSign :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSString value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setCallSign mtrChannelClusterChannelInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "setCallSign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- affiliateCallSign@
affiliateCallSign :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSString)
affiliateCallSign mtrChannelClusterChannelInfoStruct  =
    sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "affiliateCallSign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAffiliateCallSign:@
setAffiliateCallSign :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSString value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setAffiliateCallSign mtrChannelClusterChannelInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "setAffiliateCallSign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSString)
identifier mtrChannelClusterChannelInfoStruct  =
    sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSString value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setIdentifier mtrChannelClusterChannelInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct => mtrChannelClusterChannelInfoStruct -> IO (Id NSNumber)
type_ mtrChannelClusterChannelInfoStruct  =
    sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsMTRChannelClusterChannelInfoStruct mtrChannelClusterChannelInfoStruct, IsNSNumber value) => mtrChannelClusterChannelInfoStruct -> value -> IO ()
setType mtrChannelClusterChannelInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfoStruct (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @majorNumber@
majorNumberSelector :: Selector
majorNumberSelector = mkSelector "majorNumber"

-- | @Selector@ for @setMajorNumber:@
setMajorNumberSelector :: Selector
setMajorNumberSelector = mkSelector "setMajorNumber:"

-- | @Selector@ for @minorNumber@
minorNumberSelector :: Selector
minorNumberSelector = mkSelector "minorNumber"

-- | @Selector@ for @setMinorNumber:@
setMinorNumberSelector :: Selector
setMinorNumberSelector = mkSelector "setMinorNumber:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @callSign@
callSignSelector :: Selector
callSignSelector = mkSelector "callSign"

-- | @Selector@ for @setCallSign:@
setCallSignSelector :: Selector
setCallSignSelector = mkSelector "setCallSign:"

-- | @Selector@ for @affiliateCallSign@
affiliateCallSignSelector :: Selector
affiliateCallSignSelector = mkSelector "affiliateCallSign"

-- | @Selector@ for @setAffiliateCallSign:@
setAffiliateCallSignSelector :: Selector
setAffiliateCallSignSelector = mkSelector "setAffiliateCallSign:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

