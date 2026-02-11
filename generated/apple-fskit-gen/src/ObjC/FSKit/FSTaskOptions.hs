{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that passes command options to a task, optionally providing security-scoped URLs.
--
-- Generated bindings for @FSTaskOptions@.
module ObjC.FSKit.FSTaskOptions
  ( FSTaskOptions
  , IsFSTaskOptions(..)
  , init_
  , urlForOption
  , taskOptions
  , initSelector
  , urlForOptionSelector
  , taskOptionsSelector


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

-- | @- init@
init_ :: IsFSTaskOptions fsTaskOptions => fsTaskOptions -> IO (Id FSTaskOptions)
init_ fsTaskOptions  =
    sendMsg fsTaskOptions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Retrieves a URL for a given option.
--
-- Some command-line options refer to paths that indicate a location in which the module needs access to a file outside of its container. FSKit passes these paths as a URL tagged by the option name.
--
-- For example, @"-B" "./someFile"@ returns the URL for @./someFile@ when passed an option @"B"@. To indicate that your module treats a given option as a path, include it in the @pathOptions@ dictionary within a command options dictionary (@FSActivatOptionSyntax@, @FSCheckOptionSyntax@, or @FSFormatOptionSyntax@). This dictionary uses the command option name as a key, and each entry has a value indicating what kind of entry to create.
--
-- - Parameter option: The option for which to retrieve the URL. This value doesn't include leading dashes.
--
-- ObjC selector: @- urlForOption:@
urlForOption :: (IsFSTaskOptions fsTaskOptions, IsNSString option) => fsTaskOptions -> option -> IO (Id NSURL)
urlForOption fsTaskOptions  option =
  withObjCPtr option $ \raw_option ->
      sendMsg fsTaskOptions (mkSelector "urlForOption:") (retPtr retVoid) [argPtr (castPtr raw_option :: Ptr ())] >>= retainedObject . castPtr

-- | An array of strings that represent command-line options for the task.
--
-- This property is equivalent to the @argv@ array of C strings passed to a command-line tool.
--
-- ObjC selector: @- taskOptions@
taskOptions :: IsFSTaskOptions fsTaskOptions => fsTaskOptions -> IO (Id NSArray)
taskOptions fsTaskOptions  =
    sendMsg fsTaskOptions (mkSelector "taskOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @urlForOption:@
urlForOptionSelector :: Selector
urlForOptionSelector = mkSelector "urlForOption:"

-- | @Selector@ for @taskOptions@
taskOptionsSelector :: Selector
taskOptionsSelector = mkSelector "taskOptions"

