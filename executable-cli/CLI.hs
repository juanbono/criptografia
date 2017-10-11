module CLI (parseCommand) where

import Options.Applicative
import Data.Semigroup

data CipherType
  = Caesar
  | Affine

data CommandType
  = EncryptFile CipherType FilePath
  | EncryptDirectory CipherType FilePath

data Command
  = UseCaesar String
  | UseAffine

useCaesarParser :: Parser Command
useCaesarParser = UseCaesar <$> strOption
  (long "file"
   <> short 'f'
   <> metavar "FILENAME"
   <> help "input file")

useAffineParser :: Parser Command
useAffineParser = undefined

parseCommand :: Parser Command
parseCommand = useCaesarParser <|> useAffineParser
