{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAudioOutputClusterOutputInfoStruct@.
module ObjC.Matter.MTRAudioOutputClusterOutputInfoStruct
  ( MTRAudioOutputClusterOutputInfoStruct
  , IsMTRAudioOutputClusterOutputInfoStruct(..)
  , index
  , setIndex
  , outputType
  , setOutputType
  , name
  , setName
  , indexSelector
  , setIndexSelector
  , outputTypeSelector
  , setOutputTypeSelector
  , nameSelector
  , setNameSelector


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

-- | @- index@
index :: IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct => mtrAudioOutputClusterOutputInfoStruct -> IO (Id NSNumber)
index mtrAudioOutputClusterOutputInfoStruct  =
    sendMsg mtrAudioOutputClusterOutputInfoStruct (mkSelector "index") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIndex:@
setIndex :: (IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct, IsNSNumber value) => mtrAudioOutputClusterOutputInfoStruct -> value -> IO ()
setIndex mtrAudioOutputClusterOutputInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAudioOutputClusterOutputInfoStruct (mkSelector "setIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- outputType@
outputType :: IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct => mtrAudioOutputClusterOutputInfoStruct -> IO (Id NSNumber)
outputType mtrAudioOutputClusterOutputInfoStruct  =
    sendMsg mtrAudioOutputClusterOutputInfoStruct (mkSelector "outputType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutputType:@
setOutputType :: (IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct, IsNSNumber value) => mtrAudioOutputClusterOutputInfoStruct -> value -> IO ()
setOutputType mtrAudioOutputClusterOutputInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAudioOutputClusterOutputInfoStruct (mkSelector "setOutputType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct => mtrAudioOutputClusterOutputInfoStruct -> IO (Id NSString)
name mtrAudioOutputClusterOutputInfoStruct  =
    sendMsg mtrAudioOutputClusterOutputInfoStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRAudioOutputClusterOutputInfoStruct mtrAudioOutputClusterOutputInfoStruct, IsNSString value) => mtrAudioOutputClusterOutputInfoStruct -> value -> IO ()
setName mtrAudioOutputClusterOutputInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAudioOutputClusterOutputInfoStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector
setIndexSelector = mkSelector "setIndex:"

-- | @Selector@ for @outputType@
outputTypeSelector :: Selector
outputTypeSelector = mkSelector "outputType"

-- | @Selector@ for @setOutputType:@
setOutputTypeSelector :: Selector
setOutputTypeSelector = mkSelector "setOutputType:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

