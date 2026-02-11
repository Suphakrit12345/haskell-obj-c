{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterLineupInfoStruct@.
module ObjC.Matter.MTRChannelClusterLineupInfoStruct
  ( MTRChannelClusterLineupInfoStruct
  , IsMTRChannelClusterLineupInfoStruct(..)
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
operatorName :: IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct => mtrChannelClusterLineupInfoStruct -> IO (Id NSString)
operatorName mtrChannelClusterLineupInfoStruct  =
    sendMsg mtrChannelClusterLineupInfoStruct (mkSelector "operatorName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperatorName:@
setOperatorName :: (IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct, IsNSString value) => mtrChannelClusterLineupInfoStruct -> value -> IO ()
setOperatorName mtrChannelClusterLineupInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterLineupInfoStruct (mkSelector "setOperatorName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lineupName@
lineupName :: IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct => mtrChannelClusterLineupInfoStruct -> IO (Id NSString)
lineupName mtrChannelClusterLineupInfoStruct  =
    sendMsg mtrChannelClusterLineupInfoStruct (mkSelector "lineupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineupName:@
setLineupName :: (IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct, IsNSString value) => mtrChannelClusterLineupInfoStruct -> value -> IO ()
setLineupName mtrChannelClusterLineupInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterLineupInfoStruct (mkSelector "setLineupName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- postalCode@
postalCode :: IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct => mtrChannelClusterLineupInfoStruct -> IO (Id NSString)
postalCode mtrChannelClusterLineupInfoStruct  =
    sendMsg mtrChannelClusterLineupInfoStruct (mkSelector "postalCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPostalCode:@
setPostalCode :: (IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct, IsNSString value) => mtrChannelClusterLineupInfoStruct -> value -> IO ()
setPostalCode mtrChannelClusterLineupInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterLineupInfoStruct (mkSelector "setPostalCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lineupInfoType@
lineupInfoType :: IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct => mtrChannelClusterLineupInfoStruct -> IO (Id NSNumber)
lineupInfoType mtrChannelClusterLineupInfoStruct  =
    sendMsg mtrChannelClusterLineupInfoStruct (mkSelector "lineupInfoType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLineupInfoType:@
setLineupInfoType :: (IsMTRChannelClusterLineupInfoStruct mtrChannelClusterLineupInfoStruct, IsNSNumber value) => mtrChannelClusterLineupInfoStruct -> value -> IO ()
setLineupInfoType mtrChannelClusterLineupInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterLineupInfoStruct (mkSelector "setLineupInfoType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

