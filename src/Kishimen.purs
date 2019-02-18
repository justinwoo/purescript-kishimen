module Kishimen where

import Data.Generic.Rep as GR
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row as Row
import Type.Prelude (class IsSymbol, SProxy(..))

genericSumToVariant :: forall a rep r
   . GR.Generic a rep => GenericSumToVariant rep r
  => a -> Variant r
genericSumToVariant a = genericSumToVariantImpl (GR.from a)

-- | Give Generic Sum Rep, get Variant
class GenericSumToVariant rep (r :: # Type) | rep -> r where
  genericSumToVariantImpl :: rep -> Variant r

instance genericSumToVariantConstructor ::
  ( IsSymbol name
  , Row.Cons name ty () r
  , GenericSumToVariantArg a ty
  ) => GenericSumToVariant (GR.Constructor name a) r where
  genericSumToVariantImpl (GR.Constructor a) =
    Variant.inj nameS value
    where
      nameS = SProxy :: _ name
      value = genericSumToVariantArgImpl a

instance genericSumToVariantSum ::
  ( GenericSumToVariant a ra
  , GenericSumToVariant b rb
  , Row.Union ra ra' r
  , Row.Union rb rb' r
  , Row.Union ra rb r
  ) => GenericSumToVariant (GR.Sum a b) r where
  genericSumToVariantImpl (GR.Inl a) = Variant.expand (genericSumToVariantImpl a :: Variant ra)
  genericSumToVariantImpl (GR.Inr b) = Variant.expand (genericSumToVariantImpl b :: Variant rb)

-- | Give Generic Rep, get Type
class GenericSumToVariantArg rep out | rep -> out where
  genericSumToVariantArgImpl :: rep -> out

-- | NoArg encoded as empty record, for convenience (e.g. JSON)
instance genericSumToVariantArgNoArguments ::
  GenericSumToVariantArg GR.NoArguments {} where
  genericSumToVariantArgImpl _ = {}

instance genericSumToVariantArgArguments ::
  GenericSumToVariantArg (GR.Argument a) a where
  genericSumToVariantArgImpl (GR.Argument x) = x
