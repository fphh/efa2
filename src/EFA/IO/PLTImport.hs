

module EFA.IO.PLTImport where

import qualified Data.Map as M 

import Text.ParserCombinators.Parsec (parse)
import EFA.Signal.Record(Record(Record),SignalRecord, SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import EFA.IO.PLTParser (pltFile, Table)

makePLTRecord ::
  (SV.FromList t, SV.Storage t v) => [Table v] -> SignalRecord t v
makePLTRecord ((SigId "time", time):table) =
  Record (S.fromList time) (M.map S.fromList $ M.fromList table)
makePLTRecord ((SigId _ , _):_) = error "makePLTRecord no time"
makePLTRecord [] = error "makePLTRecord empty list"

modelicaPLTImport ::
  (SV.Storage t v, SV.FromList t, Read v) =>
  FilePath -> IO (SignalRecord t v)
modelicaPLTImport path = do
  text <- readFile path
  case parse pltFile path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return $ makePLTRecord table

