{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents an audio session
--
-- Generated bindings for @BEAudioSession@.
module ObjC.BrowserEngineCore.BEAudioSession
  ( BEAudioSession
  , IsBEAudioSession(..)
  , initWithAudioSession
  , setPreferredOutput_error
  , availableOutputs
  , preferredOutput
  , initWithAudioSessionSelector
  , setPreferredOutput_errorSelector
  , availableOutputsSelector
  , preferredOutputSelector


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

import ObjC.BrowserEngineCore.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a BE audio session from an  AV audio session
--
-- - Parameters:   - session: The AV audio session
--
-- ObjC selector: @- initWithAudioSession:@
initWithAudioSession :: (IsBEAudioSession beAudioSession, IsAVAudioSession audioSession) => beAudioSession -> audioSession -> IO (Id BEAudioSession)
initWithAudioSession beAudioSession  audioSession =
  withObjCPtr audioSession $ \raw_audioSession ->
      sendMsg beAudioSession (mkSelector "initWithAudioSession:") (retPtr retVoid) [argPtr (castPtr raw_audioSession :: Ptr ())] >>= ownedObject . castPtr

-- | Select a preferred output port for audio routing.    Setting a nil value will clear the preference.
--
-- ObjC selector: @- setPreferredOutput:error:@
setPreferredOutput_error :: (IsBEAudioSession beAudioSession, IsAVAudioSessionPortDescription outPort, IsNSError outError) => beAudioSession -> outPort -> outError -> IO Bool
setPreferredOutput_error beAudioSession  outPort outError =
  withObjCPtr outPort $ \raw_outPort ->
    withObjCPtr outError $ \raw_outError ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg beAudioSession (mkSelector "setPreferredOutput:error:") retCULong [argPtr (castPtr raw_outPort :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | Gets the set of output ports that are available for routing.
--
-- ObjC selector: @- availableOutputs@
availableOutputs :: IsBEAudioSession beAudioSession => beAudioSession -> IO (Id NSArray)
availableOutputs beAudioSession  =
    sendMsg beAudioSession (mkSelector "availableOutputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the preferred output port.  Will be nil if no preference has been set.
--
-- ObjC selector: @- preferredOutput@
preferredOutput :: IsBEAudioSession beAudioSession => beAudioSession -> IO (Id AVAudioSessionPortDescription)
preferredOutput beAudioSession  =
    sendMsg beAudioSession (mkSelector "preferredOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioSession:@
initWithAudioSessionSelector :: Selector
initWithAudioSessionSelector = mkSelector "initWithAudioSession:"

-- | @Selector@ for @setPreferredOutput:error:@
setPreferredOutput_errorSelector :: Selector
setPreferredOutput_errorSelector = mkSelector "setPreferredOutput:error:"

-- | @Selector@ for @availableOutputs@
availableOutputsSelector :: Selector
availableOutputsSelector = mkSelector "availableOutputs"

-- | @Selector@ for @preferredOutput@
preferredOutputSelector :: Selector
preferredOutputSelector = mkSelector "preferredOutput"

