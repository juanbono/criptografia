{-# LANGUAGE UnicodeSyntax #-}

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

import Criptografia.Mugi.InternalVector
import qualified Data.Vector.Unboxed as V
import Data.Maybe (fromJust)
import Data.Word (Word8, Word64)
import Data.LargeWord (Word128, loHalf, hiHalf)
import Control.Lens  ((^.))

mugiEncrypt :: Word128 -> Word128 -> [Word64] -> [Word64]
mugiEncrypt k iv xs = zipWith (<+>) xs (mugiStream k iv)

mugiStream :: Word128 -> Word128 -> [Word64]
mugiStream k iv = randomStream (initMugi k iv)

randomStream :: IState -> [Word64]
randomStream = map ((!2) . _stateA) . iterate updateFunction

mugiGenerate :: IState -> Word64
mugiGenerate st = (updateFunction st^.stateA) ! 2

initMugi :: Word128 -> Word128 -> IState
initMugi k iv = thirdStep s3
  where
    s1 = mixing (firstStep k)
    s2 = ivInput s1 iv
    s3 = IState (mixing s2^.stateA) (s2^.stateB)

p :: [Word8] -> [Word8] -> [Word8]
p a b = sbox . fromIntegral <$> zipWith (<+>) a b

mds :: [Word8] -> [Word8]
mds xs = [x0Eq, x1Eq, x2Eq, x3Eq]
  where
    (xs0, xs1, mul2xss0) = (head xs, xs!!1, mul2 . fromIntegral $ head xs)
    (mul2xss3, mul2xss1) = (mul2 . fromIntegral $ (xs!!3), mul2 . fromIntegral $ (xs!!1))
    (xs2, xs3, mul2xss2) = (xs!!2, xs!!3, mul2 . fromIntegral $ (xs!!2))

    x0Eq = mul2xss0 ⊕ xs1 ⊕ mul2xss1 ⊕ xs2 ⊕ xs3
    x1Eq = xs0 ⊕ mul2xss1 ⊕ xs2 ⊕ mul2xss2 ⊕ xs3
    x2Eq = xs0 ⊕ xs1 ⊕ mul2xss2 ⊕ xs3 ⊕ mul2xss3
    x3Eq = xs0 ⊕ mul2xss0 ⊕ xs1 ⊕ xs2 ⊕ mul2xss3

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple fun (a, b) = (fun a, fun b)

q :: [Word8] -> [Word8]
q xs = fromJust $ rearrange ixs (qh ++ ql)
  where
    (qh, ql) = (mapTuple mds . splitAt 4) xs
    ixs = [4, 5, 2, 3, 0, 1, 6, 7]

f :: Word64 -> Word64 -> Word64
f a b = fromByte . q $ p (toByte a) (toByte b)

ρ :: IState -> V.Vector Word64
ρ (IState a b) = V.fromList [a0, a1, a2]
  where
    a0 = a ! 1
    a1 = (a ! 2) ⊕ f a0 (b ! 4) ⊕ c1
    a2 = (a ! 0) ⊕ f a0 ((b ! 10) <<< 17) ⊕ c2

λ :: IState -> V.Vector Word64
λ state@(IState _ _) = V.fromList (map (updateB state) [0..15])
  where
    updateB (IState a' b') 0 = (b' ! 15) ⊕ (a' ! 0)
    updateB (IState _ b')  4 = (b' ! 3)  ⊕ (b' ! 7)
    updateB (IState _ b') 10 = (b' ! 9)  ⊕ (b' ! 13 <<< 32)
    updateB (IState _ b')  k = b' ! (k - 1)

updateFunction :: IState -> IState
updateFunction state = IState (ρ state) (λ state)

firstStep :: Word128 -> IState
firstStep secretKey = IState (V.fromList [a0, a1, a2]) emptyBuffer
  where
    k0 = hiHalf secretKey
    k1 = loHalf secretKey
    (a0, a1, a2) = (k0, k1, (k0 <<< 7) ⊕ (k1 >>> 7) ⊕ c0)

mixing :: IState -> IState
mixing s = IState (last iterations^.stateA) (V.fromList bs)
  where
    rhoI st = IState (ρ st) emptyBuffer
    iterations = drop 1 . take 17 $ iterate rhoI (IState (s^.stateA) emptyBuffer)
    bs = reverse . map ((!0) . _stateA) $ iterations

ivInput :: IState -> Word128 -> IState
ivInput (IState a b) iv = IState a1 b
  where
    (i0, i1) = (hiHalf iv, loHalf iv)
    a10 = (a!0) ⊕ i0
    a11 = (a!1) ⊕ i1
    a12 = (a!2) ⊕ (i0 <<< 7) ⊕ (i1 >>> 7) ⊕ c0
    a1 = V.fromList [a10, a11, a12]

applyN :: Int -> (t -> t) -> t -> t
applyN 0 _ x = x
applyN n g x = applyN (n - 1) g (g x)

thirdStep :: IState -> IState
thirdStep = applyN 16 updateFunction
