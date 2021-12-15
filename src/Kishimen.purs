module Kishimen where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Data.Variant.Internal (class VariantTags)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, SProxy(..))

-- | Convert a generic sum into a variant
genericSumToVariant :: forall a rep r
   . GR.Generic a rep => GenericSumToVariant rep r
  => a -> Variant r
genericSumToVariant a = genericSumToVariantImpl (GR.from a)

-- | Convert a variant into a generic sum, where the type of the sum determines
-- | the type of the variant, because you could not construct the sum otherwise.
-- | This also means that we can recover from the alternative introduced when
-- | expanding and contracting the Variant rows.
variantToGenericSum :: forall a rep r
   . GR.Generic a rep => GenericSumToVariant rep r
  => Variant r -> a
variantToGenericSum v =
  case GR.to <$> (variantImplToGenericSumImpl v) of
    Just x -> x
    Nothing -> unsafeCrashWith
      "the impossible occurred: the input Variant was not a valid representation of the generic sum."

-- | Give Generic Sum Rep, get Variant
class GenericSumToVariant rep (r :: Row Type) | rep -> r where
  genericSumToVariantImpl :: rep -> Variant r
  variantImplToGenericSumImpl :: Variant r -> Maybe rep

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
  variantImplToGenericSumImpl v = do
    x :: ty <- Variant.prj nameS v
    Just $ GR.Constructor $ variantArgImplToGenericSumImpl x
    where
      nameS = SProxy :: _ name

instance genericSumToVariantSum ::
  ( GenericSumToVariant a ra
  , GenericSumToVariant b rb
  , Row.Union ra ra' r
  , Row.Union rb rb' r
  , Row.Union ra rb r
  -- For Variant
  , RL.RowToList ra raL
  , VariantTags raL
  , RL.RowToList rb rbL
  , VariantTags rbL
  ) => GenericSumToVariant (GR.Sum a b) r where
  genericSumToVariantImpl (GR.Inl a) = Variant.expand (genericSumToVariantImpl a :: Variant ra)
  genericSumToVariantImpl (GR.Inr b) = Variant.expand (genericSumToVariantImpl b :: Variant rb)
  variantImplToGenericSumImpl v = do
    readLeft <|> readRight
    where
      readLeft = do
        v' :: Variant ra  <- Variant.contract v
        gs :: a <- variantImplToGenericSumImpl v'
        Just (GR.Inl gs)
      readRight = do
        v' :: Variant rb  <- Variant.contract v
        gs :: b <- variantImplToGenericSumImpl v'
        Just (GR.Inr gs)

-- | Give Generic Rep, get Type
class GenericSumToVariantArg rep out | rep -> out where
  genericSumToVariantArgImpl :: rep -> out
  variantArgImplToGenericSumImpl :: out -> rep

-- | NoArg encoded as empty record, for convenience (e.g. JSON)
instance genericSumToVariantArgNoArguments ::
  GenericSumToVariantArg GR.NoArguments {} where
  genericSumToVariantArgImpl _ = {}
  variantArgImplToGenericSumImpl {} = GR.NoArguments

instance genericSumToVariantArgArguments ::
  GenericSumToVariantArg (GR.Argument a) a where
  genericSumToVariantArgImpl (GR.Argument x) = x
  variantArgImplToGenericSumImpl x = GR.Argument x
