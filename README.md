# Base16 | Base32 | Base64 encoder and decoder

**module BaseCrypto**
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
**module BasedArray**

```haskell
module BasedArray
    ( fromString
    , example
    , mkSlice
    )
```

**module QuasiQuoter**
```haskell
module QuasiQuoter
    ( base64
    , base32
    , base16
    )
    
[base64|ZXhhbXBsZQ==|] -- QuasiQuotation
```


**ArrayContent**
```haskell
newtype VectorContent = VectorContent Word8
  deriving (Show, Storable)
```

**fromString**
```haskell
fromString :: String -> V.Vector VectorContent
```

**O(1) mkSlice -> ExpQ, using [Vector.Slice](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:slice)**
```haskell
mkSlice :: Integer -> Integer -> TH.Name -> TH.ExpQ

print ($(mkSlice 1 2 'example))
```
