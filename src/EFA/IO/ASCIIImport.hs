
-- | ASCII Import

module EFA.IO.ASCIIImport (modelicaASCIIImport) where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec (parse)
import EFA.Signal.Base (Val)

import EFA.Signal.Record (SignalRecord(SignalRecord), SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import EFA.IO.CSVParser (csvFile)



makeASCIIRecord :: [[String]] -> SignalRecord [] Val
makeASCIIRecord [] = error "This is not possible!"
makeASCIIRecord hs =
  SignalRecord (S.fromList time) (M.fromList $ zip sigIdents (map S.fromList sigs))
  where sigIdents = map (SigId . ("sig_" ++) . show) [(0::Int)..]
        time:sigs = SV.transpose (map (map read . init) hs)

-- | Main ASCII Import Function
modelicaASCIIImport ::  FilePath -> IO (SignalRecord [] Val)
modelicaASCIIImport path = do
  text <- readFile path
  case parse (csvFile ' ') path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return $ makeASCIIRecord table
