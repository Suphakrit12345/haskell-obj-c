{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Symbols.Internal.Classes (
    module ObjC.Symbols.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- NSSymbolContentTransition ----------

-- | An abstract base class for transitions that can be applied to both NSImageViews and UIImageViews that have symbol-based images.
--
-- Don't use this class directly, instead use any of the concrete subclasses.
-- 
-- Phantom type for @NSSymbolContentTransition@.
data NSSymbolContentTransition

instance IsObjCObject (Id NSSymbolContentTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolContentTransition"

class IsNSObject a => IsNSSymbolContentTransition a where
  toNSSymbolContentTransition :: a -> Id NSSymbolContentTransition

instance IsNSSymbolContentTransition (Id NSSymbolContentTransition) where
  toNSSymbolContentTransition = unsafeCastId

instance IsNSObject (Id NSSymbolContentTransition) where
  toNSObject = unsafeCastId

-- ---------- NSSymbolEffect ----------

-- | An abstract base class for effects that can be applied to both NSImageViews and UIImageViews that have symbol-based images.
--
-- Don't use this class directly, instead use any of the concrete subclasses.
-- 
-- Phantom type for @NSSymbolEffect@.
data NSSymbolEffect

instance IsObjCObject (Id NSSymbolEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolEffect"

class IsNSObject a => IsNSSymbolEffect a where
  toNSSymbolEffect :: a -> Id NSSymbolEffect

instance IsNSSymbolEffect (Id NSSymbolEffect) where
  toNSSymbolEffect = unsafeCastId

instance IsNSObject (Id NSSymbolEffect) where
  toNSObject = unsafeCastId

-- ---------- NSSymbolEffectOptions ----------

-- | Options configuring how symbol effects apply to symbol views.
-- 
-- Phantom type for @NSSymbolEffectOptions@.
data NSSymbolEffectOptions

instance IsObjCObject (Id NSSymbolEffectOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolEffectOptions"

class IsNSObject a => IsNSSymbolEffectOptions a where
  toNSSymbolEffectOptions :: a -> Id NSSymbolEffectOptions

instance IsNSSymbolEffectOptions (Id NSSymbolEffectOptions) where
  toNSSymbolEffectOptions = unsafeCastId

instance IsNSObject (Id NSSymbolEffectOptions) where
  toNSObject = unsafeCastId

-- ---------- NSSymbolEffectOptionsRepeatBehavior ----------

-- | The behavior of repetition to use when a symbol effect is animating.
-- 
-- Phantom type for @NSSymbolEffectOptionsRepeatBehavior@.
data NSSymbolEffectOptionsRepeatBehavior

instance IsObjCObject (Id NSSymbolEffectOptionsRepeatBehavior) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolEffectOptionsRepeatBehavior"

class IsNSObject a => IsNSSymbolEffectOptionsRepeatBehavior a where
  toNSSymbolEffectOptionsRepeatBehavior :: a -> Id NSSymbolEffectOptionsRepeatBehavior

instance IsNSSymbolEffectOptionsRepeatBehavior (Id NSSymbolEffectOptionsRepeatBehavior) where
  toNSSymbolEffectOptionsRepeatBehavior = unsafeCastId

instance IsNSObject (Id NSSymbolEffectOptionsRepeatBehavior) where
  toNSObject = unsafeCastId

-- ---------- NSSymbolAutomaticContentTransition ----------

-- | The default symbol transition, resolves to a particular transition in a context-sensitive manner.
-- 
-- Phantom type for @NSSymbolAutomaticContentTransition@.
data NSSymbolAutomaticContentTransition

instance IsObjCObject (Id NSSymbolAutomaticContentTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolAutomaticContentTransition"

class IsNSSymbolContentTransition a => IsNSSymbolAutomaticContentTransition a where
  toNSSymbolAutomaticContentTransition :: a -> Id NSSymbolAutomaticContentTransition

instance IsNSSymbolAutomaticContentTransition (Id NSSymbolAutomaticContentTransition) where
  toNSSymbolAutomaticContentTransition = unsafeCastId

instance IsNSObject (Id NSSymbolAutomaticContentTransition) where
  toNSObject = unsafeCastId

instance IsNSSymbolContentTransition (Id NSSymbolAutomaticContentTransition) where
  toNSSymbolContentTransition = unsafeCastId

-- ---------- NSSymbolMagicReplaceContentTransition ----------

-- | A symbol effect applies the MagicReplace animation to symbol images.
--
-- The MagicReplace effect animates common elements across symbol images.
-- 
-- Phantom type for @NSSymbolMagicReplaceContentTransition@.
data NSSymbolMagicReplaceContentTransition

instance IsObjCObject (Id NSSymbolMagicReplaceContentTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolMagicReplaceContentTransition"

class IsNSSymbolContentTransition a => IsNSSymbolMagicReplaceContentTransition a where
  toNSSymbolMagicReplaceContentTransition :: a -> Id NSSymbolMagicReplaceContentTransition

instance IsNSSymbolMagicReplaceContentTransition (Id NSSymbolMagicReplaceContentTransition) where
  toNSSymbolMagicReplaceContentTransition = unsafeCastId

instance IsNSObject (Id NSSymbolMagicReplaceContentTransition) where
  toNSObject = unsafeCastId

instance IsNSSymbolContentTransition (Id NSSymbolMagicReplaceContentTransition) where
  toNSSymbolContentTransition = unsafeCastId

-- ---------- NSSymbolReplaceContentTransition ----------

-- | A symbol effect that animates the replacement of one symbol image with another.
-- 
-- Phantom type for @NSSymbolReplaceContentTransition@.
data NSSymbolReplaceContentTransition

instance IsObjCObject (Id NSSymbolReplaceContentTransition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolReplaceContentTransition"

class IsNSSymbolContentTransition a => IsNSSymbolReplaceContentTransition a where
  toNSSymbolReplaceContentTransition :: a -> Id NSSymbolReplaceContentTransition

instance IsNSSymbolReplaceContentTransition (Id NSSymbolReplaceContentTransition) where
  toNSSymbolReplaceContentTransition = unsafeCastId

instance IsNSObject (Id NSSymbolReplaceContentTransition) where
  toNSObject = unsafeCastId

instance IsNSSymbolContentTransition (Id NSSymbolReplaceContentTransition) where
  toNSSymbolContentTransition = unsafeCastId

-- ---------- NSSymbolAppearEffect ----------

-- | A symbol effect that applies the Appear animation to symbol images.
--
-- The Appear animation makes the symbol visible either as a whole, or one motion group at a time.
-- 
-- Phantom type for @NSSymbolAppearEffect@.
data NSSymbolAppearEffect

instance IsObjCObject (Id NSSymbolAppearEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolAppearEffect"

class IsNSSymbolEffect a => IsNSSymbolAppearEffect a where
  toNSSymbolAppearEffect :: a -> Id NSSymbolAppearEffect

instance IsNSSymbolAppearEffect (Id NSSymbolAppearEffect) where
  toNSSymbolAppearEffect = unsafeCastId

instance IsNSObject (Id NSSymbolAppearEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolAppearEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolBounceEffect ----------

-- | A symbol effect that applies the Bounce animation to symbol images.
--
-- The Bounce animation applies a transitory scaling effect to the symbol.
-- 
-- Phantom type for @NSSymbolBounceEffect@.
data NSSymbolBounceEffect

instance IsObjCObject (Id NSSymbolBounceEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolBounceEffect"

class IsNSSymbolEffect a => IsNSSymbolBounceEffect a where
  toNSSymbolBounceEffect :: a -> Id NSSymbolBounceEffect

instance IsNSSymbolBounceEffect (Id NSSymbolBounceEffect) where
  toNSSymbolBounceEffect = unsafeCastId

instance IsNSObject (Id NSSymbolBounceEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolBounceEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolBreatheEffect ----------

-- | A symbol effect that applies the Breathe animation to symbol images.
--
-- The Breathe animation smoothly scales a symbol up and down.
-- 
-- Phantom type for @NSSymbolBreatheEffect@.
data NSSymbolBreatheEffect

instance IsObjCObject (Id NSSymbolBreatheEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolBreatheEffect"

class IsNSSymbolEffect a => IsNSSymbolBreatheEffect a where
  toNSSymbolBreatheEffect :: a -> Id NSSymbolBreatheEffect

instance IsNSSymbolBreatheEffect (Id NSSymbolBreatheEffect) where
  toNSSymbolBreatheEffect = unsafeCastId

instance IsNSObject (Id NSSymbolBreatheEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolBreatheEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolDisappearEffect ----------

-- | A symbol effect that applies the Disappear animation to symbol images.
--
-- The Disappear animation makes the symbol visible either as a whole, or one motion group at a time.
-- 
-- Phantom type for @NSSymbolDisappearEffect@.
data NSSymbolDisappearEffect

instance IsObjCObject (Id NSSymbolDisappearEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolDisappearEffect"

class IsNSSymbolEffect a => IsNSSymbolDisappearEffect a where
  toNSSymbolDisappearEffect :: a -> Id NSSymbolDisappearEffect

instance IsNSSymbolDisappearEffect (Id NSSymbolDisappearEffect) where
  toNSSymbolDisappearEffect = unsafeCastId

instance IsNSObject (Id NSSymbolDisappearEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolDisappearEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolDrawOffEffect ----------

-- | A symbol effect that applies the DrawOff animation to symbol images.
--
-- The DrawOff animation makes the symbol hidden either as a whole, or one motion group at a time, animating parts of the symbol with draw data.
-- 
-- Phantom type for @NSSymbolDrawOffEffect@.
data NSSymbolDrawOffEffect

instance IsObjCObject (Id NSSymbolDrawOffEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolDrawOffEffect"

class IsNSSymbolEffect a => IsNSSymbolDrawOffEffect a where
  toNSSymbolDrawOffEffect :: a -> Id NSSymbolDrawOffEffect

instance IsNSSymbolDrawOffEffect (Id NSSymbolDrawOffEffect) where
  toNSSymbolDrawOffEffect = unsafeCastId

instance IsNSObject (Id NSSymbolDrawOffEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolDrawOffEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolDrawOnEffect ----------

-- | A symbol effect that applies the DrawOn animation to symbol images.
--
-- The DrawOn animation makes the symbol visible either as a whole, or one motion group at a time, animating parts of the symbol with draw data.
-- 
-- Phantom type for @NSSymbolDrawOnEffect@.
data NSSymbolDrawOnEffect

instance IsObjCObject (Id NSSymbolDrawOnEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolDrawOnEffect"

class IsNSSymbolEffect a => IsNSSymbolDrawOnEffect a where
  toNSSymbolDrawOnEffect :: a -> Id NSSymbolDrawOnEffect

instance IsNSSymbolDrawOnEffect (Id NSSymbolDrawOnEffect) where
  toNSSymbolDrawOnEffect = unsafeCastId

instance IsNSObject (Id NSSymbolDrawOnEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolDrawOnEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolPulseEffect ----------

-- | A symbol effect that applies the Pulse animation to symbol images.
--
-- The Pulse animation fades the opacity of either all layers in the symbol, or of a subset of the layers in the symbol.
-- 
-- Phantom type for @NSSymbolPulseEffect@.
data NSSymbolPulseEffect

instance IsObjCObject (Id NSSymbolPulseEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolPulseEffect"

class IsNSSymbolEffect a => IsNSSymbolPulseEffect a where
  toNSSymbolPulseEffect :: a -> Id NSSymbolPulseEffect

instance IsNSSymbolPulseEffect (Id NSSymbolPulseEffect) where
  toNSSymbolPulseEffect = unsafeCastId

instance IsNSObject (Id NSSymbolPulseEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolPulseEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolRotateEffect ----------

-- | A symbol effect that applies the Rotate animation to symbol images.
--
-- The Rotate animation rotates parts of a symbol around a symbol-provided anchor point.
-- 
-- Phantom type for @NSSymbolRotateEffect@.
data NSSymbolRotateEffect

instance IsObjCObject (Id NSSymbolRotateEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolRotateEffect"

class IsNSSymbolEffect a => IsNSSymbolRotateEffect a where
  toNSSymbolRotateEffect :: a -> Id NSSymbolRotateEffect

instance IsNSSymbolRotateEffect (Id NSSymbolRotateEffect) where
  toNSSymbolRotateEffect = unsafeCastId

instance IsNSObject (Id NSSymbolRotateEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolRotateEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolScaleEffect ----------

-- | A symbol effect that scales symbol images.
-- 
-- Phantom type for @NSSymbolScaleEffect@.
data NSSymbolScaleEffect

instance IsObjCObject (Id NSSymbolScaleEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolScaleEffect"

class IsNSSymbolEffect a => IsNSSymbolScaleEffect a where
  toNSSymbolScaleEffect :: a -> Id NSSymbolScaleEffect

instance IsNSSymbolScaleEffect (Id NSSymbolScaleEffect) where
  toNSSymbolScaleEffect = unsafeCastId

instance IsNSObject (Id NSSymbolScaleEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolScaleEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolVariableColorEffect ----------

-- | A symbol effect that applies the Variable Color animation to symbol images.
--
-- The Variable Color animation replaces the opacity of variable layers in the symbol by a possibly repeating pattern that moves up and possibly back down the variable layers. It has no effect for non-variable color symbol images.
-- 
-- Phantom type for @NSSymbolVariableColorEffect@.
data NSSymbolVariableColorEffect

instance IsObjCObject (Id NSSymbolVariableColorEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolVariableColorEffect"

class IsNSSymbolEffect a => IsNSSymbolVariableColorEffect a where
  toNSSymbolVariableColorEffect :: a -> Id NSSymbolVariableColorEffect

instance IsNSSymbolVariableColorEffect (Id NSSymbolVariableColorEffect) where
  toNSSymbolVariableColorEffect = unsafeCastId

instance IsNSObject (Id NSSymbolVariableColorEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolVariableColorEffect) where
  toNSSymbolEffect = unsafeCastId

-- ---------- NSSymbolWiggleEffect ----------

-- | A symbol effect that applies the Wiggle animation to symbol images.
--
-- The Wiggle animation applies a transitory translation or rotation effect to the symbol.
-- 
-- Phantom type for @NSSymbolWiggleEffect@.
data NSSymbolWiggleEffect

instance IsObjCObject (Id NSSymbolWiggleEffect) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolWiggleEffect"

class IsNSSymbolEffect a => IsNSSymbolWiggleEffect a where
  toNSSymbolWiggleEffect :: a -> Id NSSymbolWiggleEffect

instance IsNSSymbolWiggleEffect (Id NSSymbolWiggleEffect) where
  toNSSymbolWiggleEffect = unsafeCastId

instance IsNSObject (Id NSSymbolWiggleEffect) where
  toNSObject = unsafeCastId

instance IsNSSymbolEffect (Id NSSymbolWiggleEffect) where
  toNSSymbolEffect = unsafeCastId
