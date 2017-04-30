# tyro

`tyro` is a dependently type JSON parsing library, that provides a quick way to create JSON parsers by deriving them from a type level description of the position of the value to be obtained.

## Examples

### Type driven interface

```Haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Tyro
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B

json = "{\"key1\":[{\"key2\":41},{\"key2\":42}]}" :: B.ByteString

-- Extract [41, 42] inside the Tyro types
parsed = decode json :: Maybe ("key1" >%> List ("key2" >%> Extract Integer))

-- We can dispose of the types using unwrap
values :: Maybe [Integer]
values = fmap unwrap parsed
```

### Value driven interface

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Tyro

json = "{\"key1\": {\"key2\" :  [41, 42]}}" :: B.ByteString

-- Extract [41, 42] inside the JSON
parsed = json %%> "key1" >%> "key2" >%> extract :: Maybe [Integer]
```
