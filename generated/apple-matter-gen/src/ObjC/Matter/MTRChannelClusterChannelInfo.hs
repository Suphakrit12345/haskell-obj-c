{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterChannelInfo@.
module ObjC.Matter.MTRChannelClusterChannelInfo
  ( MTRChannelClusterChannelInfo
  , IsMTRChannelClusterChannelInfo(..)
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
majorNumber :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSNumber)
majorNumber mtrChannelClusterChannelInfo  =
    sendMsg mtrChannelClusterChannelInfo (mkSelector "majorNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMajorNumber:@
setMajorNumber :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSNumber value) => mtrChannelClusterChannelInfo -> value -> IO ()
setMajorNumber mtrChannelClusterChannelInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfo (mkSelector "setMajorNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minorNumber@
minorNumber :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSNumber)
minorNumber mtrChannelClusterChannelInfo  =
    sendMsg mtrChannelClusterChannelInfo (mkSelector "minorNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMinorNumber:@
setMinorNumber :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSNumber value) => mtrChannelClusterChannelInfo -> value -> IO ()
setMinorNumber mtrChannelClusterChannelInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfo (mkSelector "setMinorNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSString)
name mtrChannelClusterChannelInfo  =
    sendMsg mtrChannelClusterChannelInfo (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSString value) => mtrChannelClusterChannelInfo -> value -> IO ()
setName mtrChannelClusterChannelInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfo (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- callSign@
callSign :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSString)
callSign mtrChannelClusterChannelInfo  =
    sendMsg mtrChannelClusterChannelInfo (mkSelector "callSign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCallSign:@
setCallSign :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSString value) => mtrChannelClusterChannelInfo -> value -> IO ()
setCallSign mtrChannelClusterChannelInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfo (mkSelector "setCallSign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- affiliateCallSign@
affiliateCallSign :: IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo => mtrChannelClusterChannelInfo -> IO (Id NSString)
affiliateCallSign mtrChannelClusterChannelInfo  =
    sendMsg mtrChannelClusterChannelInfo (mkSelector "affiliateCallSign") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAffiliateCallSign:@
setAffiliateCallSign :: (IsMTRChannelClusterChannelInfo mtrChannelClusterChannelInfo, IsNSString value) => mtrChannelClusterChannelInfo -> value -> IO ()
setAffiliateCallSign mtrChannelClusterChannelInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelInfo (mkSelector "setAffiliateCallSign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

