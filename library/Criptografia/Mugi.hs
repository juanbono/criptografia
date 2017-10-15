{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BinaryLiterals #-}

module Criptografia.Mugi where

import Data.Vector.Generic.Sized (fromList)
import Data.Maybe
import Data.Finite
import Criptografia.Internal.Mugi

p :: [Byte] -> [Byte] -> [Byte]
p x buffer = do
  xi <- x
  bi <- buffer
  return (xi <+> bi)

q :: [Byte] -> [Byte]
q pMatrix = [qM!!4,qM!!5,qM!!2, qM!!3,qM!!0,qM!!1,qM!!6,qM!!7]
  where
    coerceNum = finite . fromIntegral
    sboxed = [ sbox x | x <- map coerceNum pMatrix]
    qh = map (mul2 . coerceNum) $ take 4 sboxed
    ql = map (mul2 . coerceNum) $ drop 4 sboxed
    qM = qh ++ ql

f :: Unit -> Unit -> Unit
f x y = fromByte qMatrix
  where
    bytesOfX = toByte x
    bytesOfY = toByte y
    qMatrix = q (p bytesOfX bytesOfY)

ρ :: IState -> Vector 3 Unit
ρ (IState a b) = fromJust $ fromList [a0, a1, a2]
  where
    a0 = a ! 1
    a1 = (a ! 2) <+> f (a ! 1) (b ! 4) <+> c1
    a2 = (a ! 0) <+> f (a ! 1) (b ! 10 <<< 17) <+> c2

λ :: IState -> Vector 16 Unit
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

