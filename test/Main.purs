module Test.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import ExpectInferred (expectInferred)
import Kishimen (genericSumToVariant)
import Simple.JSON as JSON
import Type.Prelude (Proxy(..))

-- | Normally defined sum type
data Fruit = Apple | Banana Int | Kiwi String
derive instance genericFruit :: Generic Fruit _

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
