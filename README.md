# PureScript-Kishimen

## aka PureScript-Dan-Abramov

![](https://i.imgur.com/SiCbrx8.jpg) ![](https://i.imgur.com/MNr1Hzr.jpg)

Sum types with Generics-Rep instances to Variant for free!

## Usage

First, make a sum type of zero or one argument constructors, and derive generic:

```purs
data Fruit = Apple | Banana Int | Kiwi String
derive instance genericFruit :: Generic Fruit _
```

Then use `genericSumToVariant`:

```purs
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
```

Amazing!

## How?

You can see the inferred type in the expectInferred test:

```purs
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
```
