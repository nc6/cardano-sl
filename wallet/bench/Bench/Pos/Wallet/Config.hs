module Bench.Pos.Wallet.Config
    ( getBenchConfig
    , extractConfigFor
    ) where

import           Universum

import           Data.Csv               (HasHeader (..), decode)

import           Data.Monoid            ((<>))
import qualified Data.ByteString.Lazy   as Lazy
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Control.Exception      (SomeException)

import           Bench.Pos.Wallet.Types (AdditionalBenchConfig (..),
                                         BenchEndpoint (..),
                                         DelayFrom (..), DelayTo (..))

-- | Read benchmark configuration from the local .csv-file. The format is:
--
-- BenchName,BenchDuration,MinDelayBetweenCalls,MaxDelayBetweenCalls,PathToReportFile
-- GetHistoryBench,5.0,0.5,2.5,/tmp/GetHistoryBenchReport.csv
-- GetWalletsBench,5.5,0.2,2.1,/tmp/GetWalletsBenchReport.csv
getBenchConfig :: IO [AdditionalBenchConfig]
getBenchConfig = do
    content <- Lazy.readFile pathToConfig `catch` anyProblems
    case extractConfig content of
        Left problem -> reportAboutInvalidConfig problem
        Right (V.toList -> rawConfigs) -> return $ map toRealConfig rawConfigs
  where
    extractConfig rawContent = decode HasHeader rawContent :: Either String (Vector RawTuple)

    toRealConfig :: RawTuple -> AdditionalBenchConfig
    toRealConfig (name, duration, delayMin, delayMax, pathToReport) =
        AdditionalBenchConfig name
                              duration
                              (From delayMin, To delayMax)
                              pathToReport

    pathToConfig :: String
    pathToConfig = "wallet/bench/config/Endpoints.csv" 

    anyProblems :: SomeException -> IO a
    anyProblems whatHappened = error . toText $
        "Unable to open configuration " <> pathToConfig <> ": " <> show whatHappened

    reportAboutInvalidConfig :: String -> IO a
    reportAboutInvalidConfig problem = error . toText $
        "Invalid configuration " <> pathToConfig <> ": " <> show problem

-- Raw representation of the single line of the configuration.
type RawTuple = (String, Double, Double, Double, FilePath)

-- |
extractConfigFor
    :: BenchEndpoint
    -> [AdditionalBenchConfig]
    -> Maybe AdditionalBenchConfig
extractConfigFor bench configs = find (byName bench) configs
  where
    byName benchEp (AdditionalBenchConfig name _ _ _) = show benchEp == name
