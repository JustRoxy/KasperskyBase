# Base16 | Base32 | Base64 encoder and decoder

module BaseCrypto contains
```haskell
module BaseCrypto
    ( encode64
    , encodeUrlSafe
    , encode32
    , encode16
    , decode64
    , decodeUrlSafe
    , decode32
    , decode16
    , Error
    )
```
module BasedArray contains

```haskell
module BasedArray
  ( ArrayContent
  , Base
  , fromList
  , slice
  )
```

**ArrayContent**
```haskell
data ArrayContent = B Base | Str String
```

**Base**
```haskell
data Base = Base64 String | Base32 String | Base16 String | Base64Url String
```
