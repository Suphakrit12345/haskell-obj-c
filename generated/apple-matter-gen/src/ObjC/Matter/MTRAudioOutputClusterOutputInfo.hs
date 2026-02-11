{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAudioOutputClusterOutputInfo@.
module ObjC.Matter.MTRAudioOutputClusterOutputInfo
  ( MTRAudioOutputClusterOutputInfo
  , IsMTRAudioOutputClusterOutputInfo(..)
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
index :: IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo => mtrAudioOutputClusterOutputInfo -> IO (Id NSNumber)
index mtrAudioOutputClusterOutputInfo  =
    sendMsg mtrAudioOutputClusterOutputInfo (mkSelector "index") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIndex:@
setIndex :: (IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo, IsNSNumber value) => mtrAudioOutputClusterOutputInfo -> value -> IO ()
setIndex mtrAudioOutputClusterOutputInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAudioOutputClusterOutputInfo (mkSelector "setIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- outputType@
outputType :: IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo => mtrAudioOutputClusterOutputInfo -> IO (Id NSNumber)
outputType mtrAudioOutputClusterOutputInfo  =
    sendMsg mtrAudioOutputClusterOutputInfo (mkSelector "outputType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutputType:@
setOutputType :: (IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo, IsNSNumber value) => mtrAudioOutputClusterOutputInfo -> value -> IO ()
setOutputType mtrAudioOutputClusterOutputInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAudioOutputClusterOutputInfo (mkSelector "setOutputType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo => mtrAudioOutputClusterOutputInfo -> IO (Id NSString)
name mtrAudioOutputClusterOutputInfo  =
    sendMsg mtrAudioOutputClusterOutputInfo (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRAudioOutputClusterOutputInfo mtrAudioOutputClusterOutputInfo, IsNSString value) => mtrAudioOutputClusterOutputInfo -> value -> IO ()
setName mtrAudioOutputClusterOutputInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAudioOutputClusterOutputInfo (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

