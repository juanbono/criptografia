
module CLI (parseCommand, opts, CommandType (..)) where

import Options.Applicative
import Data.Semigroup

data CommandType
  = EncryptBMPImage { secretKey :: String, iv ::  String, filename :: FilePath }
  | DecryptBMPImage { secretKey :: String, iv ::  String, filename :: FilePath }
  deriving (Show)

mkBMPParser :: (String -> String -> FilePath -> CommandType)
            -> Parser CommandType
mkBMPParser cmdType = cmdType
  <$> strOption
  (long "secretKey"
   <> short 'k'
   <> metavar "SECRETKEY"
   <> help "128-bit secret key used to generate the random numbers")
  <*> strOption
  (long "initialization vector"
   <> short 'v'
   <> metavar "IV"
   <> help "128-bit initialization vector")
  <*> strOption
  (long "image"
   <> short 'i'
   <> metavar "IMAGE"
   <> help "image to encrypt")

encryptBMPParser :: Parser CommandType
encryptBMPParser = mkBMPParser EncryptBMPImage

decryptBMPParser :: Parser CommandType
decryptBMPParser = mkBMPParser DecryptBMPImage

parseCommand :: Parser CommandType
parseCommand = encryptBMPParser <|> decryptBMPParser


opts :: ParserInfo CommandType
opts = info (parseCommand <**> helper)
  ( fullDesc
  <> progDesc "Encrypt or decrypt and image with MUGI, japanese power at your fingertips!"
  <> header "MUGI - Random Number Generator")

