# idk what the project does

**module BasedArray**

```haskell
module BasedArray
  ( packVec,
    unpackVec,
    example,
    mkSlice,
    vectorPointer,
    recreateVector,
  )
```

**module QuasiQuoter**
```haskell
module QuasiQuoter
    ( base64
    , base32
    , base16
    , runBase
    )
    
[base64|ZXhhbXBsZQ==|] -- QuasiQuotation
```


**Allocated**
```haskell
type Allocated = VS.Vector Word8
```

**packing | unpacking**
```haskell
packVec :: BS.ByteString -> Allocated
packVec = byteStringToVector

unpackVec :: Allocated -> BS.ByteString
unpackVec = vectorToByteString
```

**O(1) mkSlice -> ExpQ, using [Vector.Slice](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:slice)**
```haskell
mkSlice :: Integer -> Integer -> TH.Name -> TH.ExpQ

print ($(mkSlice 1 2 'example))
```
