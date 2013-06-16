

module EFA.IO.PLTImport where

import EFA.IO.PLTParser (Tables, pltFile)
import Text.ParserCombinators.Parsec (parse)

import EFA.Signal.Record (Record(Record), SignalRecord)

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import qualified Data.Map as Map


makePLTRecord ::
  (SV.FromList v, SV.Storage v a) =>
  Tables a -> SignalRecord v a
makePLTRecord (time, table) =
  Record (S.fromList time) (fmap S.fromList $ Map.fromList table)

modelicaPLTImport ::
  (SV.Storage v a, SV.FromList v, Read a) =>
  FilePath -> IO (SignalRecord v a)
modelicaPLTImport path = do
  text <- readFile path
  case parse pltFile path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return $ makePLTRecord table
