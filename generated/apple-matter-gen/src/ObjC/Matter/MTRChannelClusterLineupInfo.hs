{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterLineupInfo@.
module ObjC.Matter.MTRChannelClusterLineupInfo
  ( MTRChannelClusterLineupInfo
  , IsMTRChannelClusterLineupInfo(..)
  , operatorName
  , setOperatorName
  , lineupName
  , setLineupName
  , postalCode
  , setPostalCode
  , lineupInfoType
  , setLineupInfoType
  , operatorNameSelector
  , setOperatorNameSelector
  , lineupNameSelector
  , setLineupNameSelector
  , postalCodeSelector
  , setPostalCodeSelector
  , lineupInfoTypeSelector
  , setLineupInfoTypeSelector


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

-- | @- operatorName@
operatorName :: IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo => mtrChannelClusterLineupInfo -> IO (Id NSString)
operatorName mtrChannelClusterLineupInfo  =
    sendMsg mtrChannelClusterLineupInfo (mkSelector "operatorName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperatorName:@
setOperatorName :: (IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo, IsNSString value) => mtrChannelClusterLineupInfo -> value -> IO ()
setOperatorName mtrChannelClusterLineupInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterLineupInfo (mkSelector "setOperatorName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lineupName@
lineupName :: IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo => mtrChannelClusterLineupInfo -> IO (Id NSString)
lineupName mtrChannelClusterLineupInfo  =
    sendMsg mtrChannelClusterLineupInfo (mkSelector "lineupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineupName:@
setLineupName :: (IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo, IsNSString value) => mtrChannelClusterLineupInfo -> value -> IO ()
setLineupName mtrChannelClusterLineupInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterLineupInfo (mkSelector "setLineupName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- postalCode@
postalCode :: IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo => mtrChannelClusterLineupInfo -> IO (Id NSString)
postalCode mtrChannelClusterLineupInfo  =
    sendMsg mtrChannelClusterLineupInfo (mkSelector "postalCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPostalCode:@
setPostalCode :: (IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo, IsNSString value) => mtrChannelClusterLineupInfo -> value -> IO ()
setPostalCode mtrChannelClusterLineupInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterLineupInfo (mkSelector "setPostalCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lineupInfoType@
lineupInfoType :: IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo => mtrChannelClusterLineupInfo -> IO (Id NSNumber)
lineupInfoType mtrChannelClusterLineupInfo  =
    sendMsg mtrChannelClusterLineupInfo (mkSelector "lineupInfoType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineupInfoType:@
setLineupInfoType :: (IsMTRChannelClusterLineupInfo mtrChannelClusterLineupInfo, IsNSNumber value) => mtrChannelClusterLineupInfo -> value -> IO ()
setLineupInfoType mtrChannelClusterLineupInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterLineupInfo (mkSelector "setLineupInfoType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operatorName@
operatorNameSelector :: Selector
operatorNameSelector = mkSelector "operatorName"

-- | @Selector@ for @setOperatorName:@
setOperatorNameSelector :: Selector
setOperatorNameSelector = mkSelector "setOperatorName:"

-- | @Selector@ for @lineupName@
lineupNameSelector :: Selector
lineupNameSelector = mkSelector "lineupName"

-- | @Selector@ for @setLineupName:@
setLineupNameSelector :: Selector
setLineupNameSelector = mkSelector "setLineupName:"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @setPostalCode:@
setPostalCodeSelector :: Selector
setPostalCodeSelector = mkSelector "setPostalCode:"

-- | @Selector@ for @lineupInfoType@
lineupInfoTypeSelector :: Selector
lineupInfoTypeSelector = mkSelector "lineupInfoType"

-- | @Selector@ for @setLineupInfoType:@
setLineupInfoTypeSelector :: Selector
setLineupInfoTypeSelector = mkSelector "setLineupInfoType:"

