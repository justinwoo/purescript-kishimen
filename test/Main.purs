module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import ExpectInferred (expectInferred)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Simple.JSON as JSON
import Test.Assert (assertEqual)
import Type.Prelude (Proxy(..), SProxy(..))

-- | Normally defined sum type
data Fruit = Apple | Banana Int | Kiwi String
derive instance genericFruit :: Generic Fruit _

-- for the assertion tests below
derive instance eqFruit :: Eq Fruit
instance showFruit :: Show Fruit where
  show Apple = "Apple"
  show (Banana x) = "Banana " <> show x
  show (Kiwi s) = "Kiwi " <> show s

test1 :: Unit
test1 = expectInferred expectedP simpleValue
  where
    expectedP = Proxy :: _
      (Variant
        ( "Apple" :: {}
        , "Banana" :: Int
        , "Kiwi" :: String
        ))
    simpleValue = genericSumToVariant Apple

test2 :: Unit
test2 = expectInferred expectedP simpleValue
  where
    expectedP = Proxy :: _ Fruit
    simpleValue = variantToGenericSum (Variant.inj (SProxy :: _ "Apple") {}) :: Fruit

test3 :: Unit
test3 = expectInferred expectedP simpleValue
  where
    expectedP = Proxy :: _ Fruit
    simpleValue = variantToGenericSum (Variant.inj (SProxy :: _ "Banana") 3) :: Fruit

main :: Effect Unit
main = do
  logShow $ genericSumToVariant Apple
  logShow $ genericSumToVariant (Banana 3)
  logShow $ genericSumToVariant (Kiwi "green")

  -- results:
  -- (inj @"Apple" {})
  -- (inj @"Banana" 3)
  -- (inj @"Kiwi" "green")

  log $ JSON.writeJSON $ genericSumToVariant Apple
  log $ JSON.writeJSON $ genericSumToVariant (Banana 3)
  log $ JSON.writeJSON $ genericSumToVariant (Kiwi "green")

  -- results:
  -- {"type":"Apple","value":{}}
  -- {"type":"Banana","value":3}
  -- {"type":"Kiwi","value":"green"}

  -- converts the other way around too, given a fixed sum type
  assertEqual
    { expected: Apple
    , actual: variantToGenericSum (Variant.inj (SProxy :: _ "Apple") {})
    }
  assertEqual
    { expected: Banana 3
    , actual: variantToGenericSum (Variant.inj (SProxy :: _ "Banana") 3)
    }
  assertEqual
    { expected: Kiwi "green"
    , actual: variantToGenericSum (Variant.inj (SProxy :: _ "Kiwi") "green")
    }

  let testJSON1 = """{"type":"Apple","value":{}}"""
  assertEqual
    { expected: Right Apple
    , actual: variantToGenericSum <$> JSON.readJSON testJSON1
    }

  let testJSON2 = """{"type":"Banana","value":123}"""
  assertEqual
    { expected: Right (Banana 123)
    , actual: variantToGenericSum <$> JSON.readJSON testJSON2
    }

  let testJSON3 = """{"type":"Kiwi","value":"pink"}"""
  assertEqual
    { expected: Right (Kiwi "pink")
    , actual: variantToGenericSum <$> JSON.readJSON testJSON3
    }
