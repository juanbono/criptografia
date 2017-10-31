{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}

module Criptografia.Mugi where

import Criptografia.Mugi.Internal
import qualified Data.Vector.Generic.Sized as V
import Data.Maybe (fromJust)
import Data.Word (Word8, Word64)
import Data.LargeWord (Word128, loHalf, hiHalf)
import Control.Lens
import Data.Finite

p :: [Word8] -> [Word8] -> [Word8]
p a b = sbox . fromIntegral <$> zipWith (<+>) a b

mds :: [Word8] -> [Word8]
mds xs = [x0Eq, x1Eq, x2Eq, x3Eq]
  where
    xss = map fromIntegral xs
    x0Eq = mul2 (xss!!0) ⊕ (xs!!1) ⊕ mul2 (xss!!1) ⊕ (xs!!2) ⊕ (xs!!3)
    x1Eq = (xs!!0) ⊕ mul2 (xss!!1) ⊕ (xs!!2) ⊕ mul2 (xss!!2) ⊕ (xs!!3)
    x2Eq = (xs!!0) ⊕ (xs!!1) ⊕ mul2 (xss!!2) ⊕ (xs!!3) ⊕ mul2 (xss!!3)
    x3Eq = (xs!!0) ⊕ mul2 (xss!!0) ⊕ (xs!!1) ⊕ (xs!!2) ⊕ mul2 (xss!!3)

q :: [Word8] -> [Word8]
q pMatrix = fromJust $ rearrange is (qh ++ ql)
  where
    qh = mds $ take 4 pMatrix
    ql = mds $ drop 4 pMatrix
    is = [4, 5, 2, 3, 0, 1, 6, 7]

f :: Word64 -> Word64 -> Word64
f a b = fromByte . q $ p (toByte a) (toByte b)

ρ :: IState -> Vector 3 Word64
ρ (IState a b) = unsafeFromList [a0, a1, a2]
  where
    a0 = a ! 1
    a1 = (a ! 2) ⊕ f (a ! 1) (b ! 4) ⊕ c1
    a2 = (a ! 0) ⊕ f (a ! 1) ((b ! 10) <<< 17) ⊕ c2

λ :: IState -> Vector 16 Word64
λ state@(IState _ b) = b `updateWith` newValues
  where
    updateB (IState a' b') 0 = (b' ! 15) ⊕ (a' ! 0)
    updateB (IState _ b')  4 = (b' ! 3)  ⊕ (b' ! 7)
    updateB (IState _ b') 10 = (b' ! 9)  ⊕ (b' ! 13 <<< 32)
    updateB (IState _ b')  k = b' ! (k - 1)

    newValues = [ (fromIntegral i, updateB state i)
                | i <- map finite [0..15]
                ]

updateF :: IState -> IState
updateF state = IState (ρ state) (λ state)

firstStep :: Word128 -> IState
firstStep secretKey = IState (unsafeFromList [a0, a1, a2]) emptyBuffer
  where
    k0 = hiHalf secretKey
    k1 = loHalf secretKey
    (a0, a1, a2) = (k0, k1, (k0 <<< 7) ⊕ (k1 >>> 7) ⊕ c0)

mixing :: IState -> IState
mixing s = IState (last iterations^.stateA) (unsafeFromList bs)
  where
    rho_i st = IState (ρ st) emptyBuffer
    iterations = drop 1 . take 17 $ iterate rho_i (IState (s^.stateA) emptyBuffer)
    bs = reverse $ map ((!0) . _stateA) iterations

ivInput :: IState -> Word128 -> IState
ivInput (IState a b) iv = IState a1 b
  where
    (i0, i1) = (hiHalf iv, loHalf iv)
    a10 = (a!0) ⊕ i0
    a11 = (a!1) ⊕ i1
    a12 = (a!2) ⊕ (i0 <<< 7) ⊕ (i1 >>> 7) ⊕ c0
    a1 = unsafeFromList [a10, a11, a12]

thirdStep :: IState -> IState
thirdStep s = last . take 17 $ iterate updateF s

init :: Word128 -> Word128 -> IState
init k iv = undefined
  where
    s0 = firstStep k
    s1 = mixing s0
    s2 = ivInput s1 iv
    s3 = IState (mixing s2^.stateA) (s2^.stateB)

mugi :: IState -> Word128 -> Word128 -> Word64
mugi = undefined

mugiEncrypt :: Word128 -> Word128 -> Word64 -> Word64
mugiEncrypt = undefined

mugiDecrypt :: Word128 -> Word128 -> Word64 -> Word64
mugiDecrypt = undefined
