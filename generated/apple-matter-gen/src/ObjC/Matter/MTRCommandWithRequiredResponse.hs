{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object representing a single command to be invoked and the response required for the invoke to be considered successful.
--
-- Generated bindings for @MTRCommandWithRequiredResponse@.
module ObjC.Matter.MTRCommandWithRequiredResponse
  ( MTRCommandWithRequiredResponse
  , IsMTRCommandWithRequiredResponse(..)
  , initWithPath_commandFields_requiredResponse
  , path
  , setPath
  , commandFields
  , setCommandFields
  , requiredResponse
  , setRequiredResponse
  , initWithPath_commandFields_requiredResponseSelector
  , pathSelector
  , setPathSelector
  , commandFieldsSelector
  , setCommandFieldsSelector
  , requiredResponseSelector
  , setRequiredResponseSelector


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

-- | @- initWithPath:commandFields:requiredResponse:@
initWithPath_commandFields_requiredResponse :: (IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse, IsMTRCommandPath path, IsNSDictionary commandFields, IsNSDictionary requiredResponse) => mtrCommandWithRequiredResponse -> path -> commandFields -> requiredResponse -> IO (Id MTRCommandWithRequiredResponse)
initWithPath_commandFields_requiredResponse mtrCommandWithRequiredResponse  path commandFields requiredResponse =
  withObjCPtr path $ \raw_path ->
    withObjCPtr commandFields $ \raw_commandFields ->
      withObjCPtr requiredResponse $ \raw_requiredResponse ->
          sendMsg mtrCommandWithRequiredResponse (mkSelector "initWithPath:commandFields:requiredResponse:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_commandFields :: Ptr ()), argPtr (castPtr raw_requiredResponse :: Ptr ())] >>= ownedObject . castPtr

-- | The path of the command being invoked.
--
-- ObjC selector: @- path@
path :: IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse => mtrCommandWithRequiredResponse -> IO (Id MTRCommandPath)
path mtrCommandWithRequiredResponse  =
    sendMsg mtrCommandWithRequiredResponse (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The path of the command being invoked.
--
-- ObjC selector: @- setPath:@
setPath :: (IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse, IsMTRCommandPath value) => mtrCommandWithRequiredResponse -> value -> IO ()
setPath mtrCommandWithRequiredResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommandWithRequiredResponse (mkSelector "setPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The command fields to pass for the command invoke.  nil if this command does not have any fields.  If not nil, this should be a data-value dictionary of MTRStructureValueType.
--
-- ObjC selector: @- commandFields@
commandFields :: IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse => mtrCommandWithRequiredResponse -> IO (Id NSDictionary)
commandFields mtrCommandWithRequiredResponse  =
    sendMsg mtrCommandWithRequiredResponse (mkSelector "commandFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The command fields to pass for the command invoke.  nil if this command does not have any fields.  If not nil, this should be a data-value dictionary of MTRStructureValueType.
--
-- ObjC selector: @- setCommandFields:@
setCommandFields :: (IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse, IsNSDictionary value) => mtrCommandWithRequiredResponse -> value -> IO ()
setCommandFields mtrCommandWithRequiredResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommandWithRequiredResponse (mkSelector "setCommandFields:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The response that represents this command succeeding.
--
-- If this is nil, that indicates that the invoke is considered successful if it does not result in an error status response.
--
-- If this is is not nil, then the invoke is considered successful if the response is a data response and for each entry in the provided requiredResponse the field whose field ID matches the key of the entry has a value that equals the value of the entry.  Values of entries are data-value dictionaries.
--
-- ObjC selector: @- requiredResponse@
requiredResponse :: IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse => mtrCommandWithRequiredResponse -> IO (Id NSDictionary)
requiredResponse mtrCommandWithRequiredResponse  =
    sendMsg mtrCommandWithRequiredResponse (mkSelector "requiredResponse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The response that represents this command succeeding.
--
-- If this is nil, that indicates that the invoke is considered successful if it does not result in an error status response.
--
-- If this is is not nil, then the invoke is considered successful if the response is a data response and for each entry in the provided requiredResponse the field whose field ID matches the key of the entry has a value that equals the value of the entry.  Values of entries are data-value dictionaries.
--
-- ObjC selector: @- setRequiredResponse:@
setRequiredResponse :: (IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse, IsNSDictionary value) => mtrCommandWithRequiredResponse -> value -> IO ()
setRequiredResponse mtrCommandWithRequiredResponse  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommandWithRequiredResponse (mkSelector "setRequiredResponse:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPath:commandFields:requiredResponse:@
initWithPath_commandFields_requiredResponseSelector :: Selector
initWithPath_commandFields_requiredResponseSelector = mkSelector "initWithPath:commandFields:requiredResponse:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @commandFields@
commandFieldsSelector :: Selector
commandFieldsSelector = mkSelector "commandFields"

-- | @Selector@ for @setCommandFields:@
setCommandFieldsSelector :: Selector
setCommandFieldsSelector = mkSelector "setCommandFields:"

-- | @Selector@ for @requiredResponse@
requiredResponseSelector :: Selector
requiredResponseSelector = mkSelector "requiredResponse"

-- | @Selector@ for @setRequiredResponse:@
setRequiredResponseSelector :: Selector
setRequiredResponseSelector = mkSelector "setRequiredResponse:"

