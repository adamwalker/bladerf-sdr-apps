{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Trans.Either
import Data.Complex
import Foreign.C.Types
import Data.Maybe
import Control.Monad
import Data.Monoid

import Pipes as P
import Pipes.Prelude as P
import Options.Applicative
import Data.Vector.Storable as VS hiding ((++))
import Data.Vector.Generic as VG hiding ((++))

import SDR.Util as U
import SDR.ArgUtils

import LibBladeRF.LibBladeRF
import LibBladeRF.Types
import LibBladeRF.Pipes
import LibBladeRF.Utils

data Options = Options {
    frequency    :: Int,
    sampleRate   :: Int,
    bandwidth    :: Maybe Int
}

optParser :: Parser Options
optParser = Options 
          <$> option (fmap fromIntegral parseSize) (
                 long "frequency"  
              <> short 'f' 
              <> metavar "FREQUENCY" 
              <> help "Frequency to tune to"
              )
          <*> option (fmap fromIntegral parseSize) (
                 long "samplerate" 
              <> short 'r' 
              <> metavar "RATE" 
              <> help "Sample rate"
              )
          <*> optional (option (fmap fromIntegral parseSize) (
                 long "bandwidth" 
              <> short 'b' 
              <> metavar "BW" 
              <> help "Bandwidth. Defaults to sample rate / 2"
              ))

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Transmit a sine wave" <> header "BladeRF Transmit")

producer = forever $ yield $ VS.replicate 8192 1024

doIt Options{..} = do
    dev <- lift openBladeRF
    sink <- bladeRFSink dev (BladeRFTxConfig frequency sampleRate (fromMaybe (sampleRate `quot` 2) bandwidth) (-10) 20)
    lift $ runEffect $ producer >->  sink

    lift $ bladeRFEnableModule dev MODULE_TX False
    return ()

main = execParser opt >>= eitherT putStrLn return . doIt

