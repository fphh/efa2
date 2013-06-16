
-- | ASCII Import

module EFA.IO.ASCIIImport (modelicaASCIIImport) where

import EFA.IO.CSVParser (csvFile)

import EFA.Signal.Record (Record(Record),SignalRecord, SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Zip as Zip
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (parse)


makeASCIIRecord ::
  (SV.Storage t v, SV.FromList t, Read v) =>
  NonEmpty.T [] (NonEmpty.T (NonEmpty.T []) String) ->
  SignalRecord t v
makeASCIIRecord hs =
  Record (S.fromList time) (Map.fromList $ zip sigIdents $ map S.fromList sigs)
  where sigIdents = map (SigId . ("sig_" ++) . show) [(0::Int)..]
        NonEmpty.Cons time sigs =
           Zip.transposeClip $ fmap (fmap read . NonEmpty.init) $
           NonEmpty.flatten hs

-- | Main ASCII Import Function
modelicaASCIIImport ::
  (SV.Storage t v, SV.FromList t, Read v) =>
  FilePath -> IO (SignalRecord t v)
modelicaASCIIImport path = do
  text <- readFile path
  case parse (csvFile ' ') path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return $ makeASCIIRecord table
