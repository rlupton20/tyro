# jason

`jason` is a dependently type JSON parsing library, that provides a quick way to create JSON parsers by deriving them from a type level description of the position of the value to be obtained.

## Example

```Haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Jason
import Data.Aeson (decode)
import Data.Text (Text)

json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}" :: Text

-- Extract [41, 42] inside the Jason types
parsed = decode json :: Maybe ("key1" |>| List ("key2" |>| Parse Integer))

-- We can dispose of the types using unwrap
values :: Maybe [Integer]
values = fmap unwrap parsed
```