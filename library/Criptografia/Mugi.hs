{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Strict #-}

module Criptografia.Mugi
  ( mixing
  , initMugi
  , mugiStream
  , mugiGenerate
  , mugiEncrypt
  , randomStream
  , firstStep
  , thirdStep
  , ivInput
  ) where

import Criptografia.Mugi.Internal
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word64)
import Data.LargeWord (Word128, loHalf, hiHalf)

mugiEncrypt :: Word128 -> Word128 -> [Word64] -> [Word64]
mugiEncrypt k iv xs = zipWith (⊕) xs (mugiStream k iv)

mugiStream :: Word128 -> Word128 -> [Word64]
mugiStream k iv = randomStream (initMugi k iv)

randomStream :: IState -> [Word64]
randomStream = map ((!2) . stateA) . iterate updateFunction

mugiGenerate :: IState -> Word64
mugiGenerate = (!2) . stateA . updateFunction

initMugi :: Word128 -> Word128 -> IState
initMugi k iv = thirdStep s3
  where
    s1 = mixing (firstStep k)
    s2 = ivInput s1 iv
    s3 = IState (stateA . mixing $ s2) (stateB s2)

p :: [Word8] -> [Word8] -> [Word8]
p a b = sbox . fromIntegral <$> zipWith (⊕) a b

mds :: [Word8] -> [Word8]
mds xs = [ m0 ⊕ x1 ⊕ m1 ⊕ x2 ⊕ x3
         , x0 ⊕ m1 ⊕ x2 ⊕ m2 ⊕ x3
         , x0 ⊕ x1 ⊕ m2 ⊕ x3 ⊕ m3
         , x0 ⊕ m0 ⊕ x1 ⊕ x2 ⊕ m3 ]
  where
    (T4Byte x0 x1 x2 x3) = toT4Byte xs
    mul2f = mul2 . fromIntegral
    T4Byte m0 m1 m2 m3 = T4Byte (mul2f x0) (mul2f x1) (mul2f x2) (mul2f x3)

q :: [Word8] -> [Word8]
q xs = [q4, q5, q2, q3, q0, q1, q6, q7]
  where
    T8Byte q0 q1 q2 q3 q4 q5 q6 q7
      = (toT8Byte . uncurry (++) . mapTuple mds . splitAt 4) xs

f :: Word64 -> Word64 -> Word64
f a b = fromByte . q $ p (toByte a) (toByte b)

ρ :: IState -> U.Vector Word64
ρ (IState a b) = U.fromList [a0, a1, a2]
  where
    a0 = a ! 1
    a1 = (a ! 2) ⊕ f a0 (b ! 4) ⊕ c1
    a2 = (a ! 0) ⊕ f a0 ((b ! 10) <<< 17) ⊕ c2

updateB :: IState -> Int -> Word64
updateB (IState a' b') 0 = (b' ! 15) ⊕ (a' ! 0)
updateB (IState _ b')  4 = (b' ! 3)  ⊕ (b' ! 7)
updateB (IState _ b') 10 = (b' ! 9)  ⊕ (b' ! 13 <<< 32)
updateB (IState _ b')  k = b' ! (k - 1)

λ :: (IState -> Int -> Word64) -> IState -> U.Vector Word64
λ func state = U.map (func state) (U.enumFromN 0 16)

updateFunction :: IState -> IState
updateFunction state = IState (ρ state) (λ updateB state)

firstStep :: Word128 -> IState
firstStep secretKey = IState (U.fromList [a0, a1, a2]) emptyBuffer
  where
    k0 = hiHalf secretKey
    k1 = loHalf secretKey
    (a0, a1, a2) = (k0, k1, (k0 <<< 7) ⊕ (k1 >>> 7) ⊕ c0)

mixing :: IState -> IState
mixing s = IState (stateA . last $ iterations) (U.fromList bs)
  where
    rhoI st = IState (ρ st) emptyBuffer
    iterations = drop 1 . take 17 $ iterate rhoI (IState (stateA s) emptyBuffer)
    bs = reverse . map (U.unsafeHead . stateA) $ iterations

ivInput :: IState -> Word128 -> IState
ivInput (IState a b) iv = IState a1 b
  where
    (i0, i1) = (hiHalf iv, loHalf iv)
    a10 = (a!0) ⊕ i0
    a11 = (a!1) ⊕ i1
    a12 = (a!2) ⊕ (i0 <<< 7) ⊕ (i1 >>> 7) ⊕ c0
    a1 = U.fromList [a10, a11, a12]

applyN :: Int -> (t -> t) -> t -> t
applyN 0 _ x = x
applyN n g x = applyN (n - 1) g (g x)

thirdStep :: IState -> IState
thirdStep = applyN 16 updateFunction
