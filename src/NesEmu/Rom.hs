module NesEmu.Rom where

import           Data.Bits       (shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.Word

data NesRom = NesRom
    { prgRom :: BS.ByteString
    , chrRom :: BS.ByteString
    , mapper :: Word8
    }
    deriving (Show)

loadRom :: FilePath -> IO (Either String NesRom)
loadRom path = do
    content <- BS.readFile path
    if BS.length content < 16
        then return $ Left "Invalid NES file: too short"
        else do
            let header = BS.take 16 content
            if BS.take 4 header /= BS.pack [0x4E, 0x45, 0x53, 0x1A] -- "NES" + EOF
                then return $ Left "Invalid NES file: bad magic number"
                else do
                    let prgSize = fromIntegral (BS.index header 4) * 16384
                    let chrSize = fromIntegral (BS.index header 5) * 8192
                    let flags6 = BS.index header 6
                    let flags7 = BS.index header 7
                    let mapperId = (flags7 .&. 0xF0) .|. (flags6 `shiftR` 4)

                    let prgStart = 16
                    let chrStart = prgStart + prgSize

                    if BS.length content < chrStart + chrSize
                        then return $ Left "Invalid NES file: incomplete ROM data"
                        else do
                            let prg = BS.take prgSize (BS.drop prgStart content)
                            let chr = BS.take chrSize (BS.drop chrStart content)
                            return $
                                Right
                                    NesRom
                                        { prgRom = prg
                                        , chrRom = chr
                                        , mapper = mapperId
                                        }
