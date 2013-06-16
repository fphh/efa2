

module EFA.IO.PLTImport where

import EFA.IO.PLTParser (Table, pltFile)
import Text.ParserCombinators.Parsec (Parser, parse)

import EFA.Signal.Record(Record(Record),SignalRecord, SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Map as Map


makePLTRecord ::
  (SV.FromList v, SV.Storage v a) =>
  NonEmpty.T [] (Table a) ->
  Parser (SignalRecord v a)
makePLTRecord (NonEmpty.Cons (SigId timeStr, time) table) =
  case timeStr of
    "time" ->
      return $ Record (S.fromList time) (fmap S.fromList $ Map.fromList table)
    _ -> fail "makePLTRecord no time"

modelicaPLTImport ::
  (SV.Storage t v, SV.FromList t, Read v) =>
  FilePath -> IO (SignalRecord t v)
modelicaPLTImport path = do
  text <- readFile path
  case parse (makePLTRecord =<< pltFile) path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return table
