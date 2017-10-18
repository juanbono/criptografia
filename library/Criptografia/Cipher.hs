{-# LANGUAGE TemplateHaskell #-}

module Criptografia.Cipher
  ( Cipher (..)
  -- * Lenses
  , alphabet
  , encrypt
  , decrypt
  ) where

import Control.Lens

data Cipher a k c = MkCipher
  { _alphabet :: a
  , _encrypt  :: k -> c -> c
  , _decrypt  :: k -> c -> c
  }

makeLenses ''Cipher
