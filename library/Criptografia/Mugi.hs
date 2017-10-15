{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BinaryLiterals #-}

module Criptografia.Mugi where

import Criptografia.Internal.Mugi
import Data.Vector.Generic.Sized (fromList)
import Data.Maybe (fromJust)
import Data.Finite (finite)
import Data.Word (Word8, Word64)

p :: [Word8] -> [Word8] -> [Word8]
p x buffer = do
  xi <- x
  bi <- buffer
  return (xi <+> bi)

q :: [Word8] -> [Word8]
q pMatrix = fromJust $ rearrange is (qh ++ ql)
  where
    coerceNum = finite . fromIntegral
    sboxed = [ sbox x | x <- map coerceNum pMatrix ]
    qh = map (mul2 . coerceNum) $ take 4 sboxed
    ql = map (mul2 . coerceNum) $ drop 4 sboxed
    is = [4, 5, 2, 3, 0, 1, 6, 7]

f :: Word64 -> Word64 -> Word64
f x y = fromByte qMatrix
  where
    bytesOfX = toByte x
    bytesOfY = toByte y
    qMatrix = q (p bytesOfX bytesOfY)

ρ :: IState -> Vector 3 Word64
ρ (IState a b) = fromJust $ fromList [a0, a1, a2]
  where
    a0 = a ! 1
    a1 = (a ! 2) <+> f (a ! 1) (b ! 4) <+> c1
    a2 = (a ! 0) <+> f (a ! 1) (b ! 10 <<< 17) <+> c2

λ :: IState -> Vector 16 Word64
λ state@(IState _ b) = b `updateWith` newValues
  where
    updateB (IState a' b') 0 = (b' ! 15) <+> (a' ! 0)
    updateB (IState _ b')  4 = (b' ! 3)  <+> (b' ! 7)
    updateB (IState _ b') 10 = (b' ! 9)  <+> (b' ! 13 <<< 32)
    updateB (IState _ b')  k = b' ! (k - 1)

    newValues = [ (fromIntegral i, updateB state i)
                | i <- map finite [0..17]
                ]

updateF :: IState -> IState
updateF state = IState (ρ state) (λ state)

