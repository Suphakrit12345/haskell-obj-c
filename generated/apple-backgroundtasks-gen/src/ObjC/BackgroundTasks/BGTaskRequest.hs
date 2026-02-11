{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract class that represents a request for the app to be launched in the background to perform work. Do not instantiate instances of this class directly. Instead, use one of its concrete subclasses.
--
-- Generated bindings for @BGTaskRequest@.
module ObjC.BackgroundTasks.BGTaskRequest
  ( BGTaskRequest
  , IsBGTaskRequest(..)
  , init_
  , new
  , identifier
  , earliestBeginDate
  , setEarliestBeginDate
  , initSelector
  , newSelector
  , identifierSelector
  , earliestBeginDateSelector
  , setEarliestBeginDateSelector


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

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBGTaskRequest bgTaskRequest => bgTaskRequest -> IO (Id BGTaskRequest)
init_ bgTaskRequest  =
    sendMsg bgTaskRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BGTaskRequest)
new  =
  do
    cls' <- getRequiredClass "BGTaskRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The identifier of the task associated with the request.
--
-- ObjC selector: @- identifier@
identifier :: IsBGTaskRequest bgTaskRequest => bgTaskRequest -> IO (Id NSString)
identifier bgTaskRequest  =
    sendMsg bgTaskRequest (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The earliest date and time at which to run the task.
--
-- Specify @nil@ for no start delay.
--
-- Setting the property indicates that the background task shouldn’t start any earlier than this date. However, the system doesn’t guarantee launching the task at the specified date, but only that it won’t begin sooner.
--
-- ObjC selector: @- earliestBeginDate@
earliestBeginDate :: IsBGTaskRequest bgTaskRequest => bgTaskRequest -> IO (Id NSDate)
earliestBeginDate bgTaskRequest  =
    sendMsg bgTaskRequest (mkSelector "earliestBeginDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The earliest date and time at which to run the task.
--
-- Specify @nil@ for no start delay.
--
-- Setting the property indicates that the background task shouldn’t start any earlier than this date. However, the system doesn’t guarantee launching the task at the specified date, but only that it won’t begin sooner.
--
-- ObjC selector: @- setEarliestBeginDate:@
setEarliestBeginDate :: (IsBGTaskRequest bgTaskRequest, IsNSDate value) => bgTaskRequest -> value -> IO ()
setEarliestBeginDate bgTaskRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg bgTaskRequest (mkSelector "setEarliestBeginDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @earliestBeginDate@
earliestBeginDateSelector :: Selector
earliestBeginDateSelector = mkSelector "earliestBeginDate"

-- | @Selector@ for @setEarliestBeginDate:@
setEarliestBeginDateSelector :: Selector
setEarliestBeginDateSelector = mkSelector "setEarliestBeginDate:"

