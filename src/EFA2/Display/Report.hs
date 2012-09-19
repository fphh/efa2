{-# LANGUAGE FlexibleInstances #-}

module EFA2.Display.Report (module EFA2.Display.Report) where

import qualified Data.List as L
import qualified Text.PrettyPrint as PP
import EFA2.Signal.Base (Val)


-- | Report Options 
data ROpt = RVertical | RAll | RTimeMask Val Val | RIndexMask [Int] deriving (Show,Eq)
type ROpts = [ROpt]

{- geht nicht
checkOpt :: ROpts -> ROpt -> 
checkOpt os o = L.find g os
  where
    g o = 
-}

-- | Report  
type Report = [Table]


-- | Table with Table Format and Table Data
data Table  = Table { tableTitle :: Title,                       
                      tableFormat :: TableFormat,
                      tableData :: TableData,
                      tableSubTitle :: SubTitle} 

type Title = String
type SubTitle = String

-- | Table-Data including string length
data TableData = TableData {tableBody :: [[Cell]],
                            titleRow :: [[Cell]],
                            titleCols :: [[Cell]],
                            endCols :: [[Cell]]} deriving (Show,Eq)

data Cell = Cell {cellWidth :: Length, cellContent :: PP.Doc} deriving (Show)

instance Eq Cell where
   Cell xl xd == Cell yl yd  =
      xl==yl && PP.render xd == PP.render yd

type Length = Int


-- | Table Format
data TableFormat = TableFormat {colFormat :: ColFormat,
                                rowFormat :: RowFormat} 
                                                              
type ColFormat = [(Width,Align)]
type RowFormat = [Rows]

type Width = Int 
data Align = HLeft | HMid | HRight deriving Show
type Rows = Int -- Nr of Rows to be left free before

-- | 
tvcat :: [Table] -> Table
tvcat = foldl1 tvapp


tvapp :: Table -> Table -> Table
tvapp y1 y2 = if check then Table {tableTitle = tableTitle y1 ++ " ++  " ++ tableTitle y2,
                                   tableFormat = f (tableFormat y1) (tableFormat y2),
                                   tableData = g (tableData y1) (tableData y2),
                                   tableSubTitle = tableSubTitle y1 ++ " ++  " ++ tableSubTitle y2} else error m
                                   where
                                         g :: TableData -> TableData -> TableData
                                         g x1 x2 = TableData {titleRow = titleRow x1,
                                                             tableBody = tableBody x1 ++ tableBody x2,
                                                             titleCols = titleCols x1 ++ titleCols x2,
                                                             endCols = endCols x1 ++ endCols x2}

                                         f :: TableFormat -> TableFormat -> TableFormat
                                         f x1 x2 = TableFormat {colFormat = maxColWidth (colFormat x1) (colFormat x2),
                                                               rowFormat = rowFormat x1 ++ (tail $ rowFormat x2)}

                                         check = (titleRow $ tableData y1) ==  (titleRow $ tableData y2)
                                         m = "Error in tvCat -- not same column labels"

thcat :: [Table] -> Table
thcat = foldl1 thapp

thapp :: Table -> Table -> Table
thapp y1 y2 = if check then Table {tableTitle = tableTitle y1 ++ " ++  " ++ tableTitle y2,
                                   tableFormat = f (tableFormat y1) (tableFormat y2),
                                   tableData = g (tableData y1) (tableData y2),
                                   tableSubTitle = tableSubTitle y1 ++ " ++  " ++ tableSubTitle y2} else error m
              where g :: TableData -> TableData -> TableData
                    g x1 x2 = TableData {titleRow = titleRow x1++titleRow x2,
                                        tableBody = L.zipWith (++) (tableBody x1) (tableBody x2),
                                        titleCols = titleCols x1,
                                        endCols = endCols x1}

                    f :: TableFormat -> TableFormat -> TableFormat
                    f x1 x2 = TableFormat {colFormat = (init $ colFormat x1)++(init $ tail $ colFormat x2)++[(last $ colFormat x2)],
                                           rowFormat = rowFormat x1}
                    check = (titleCols $ tableData y1) == (titleCols $ tableData y2) && (endCols $ tableData y1) == (endCols $ tableData y2)
                    m = "Error in thcat - not same column title and end row"


maxColWidth :: ColFormat -> ColFormat -> ColFormat
maxColWidth cf1 cf2 = zipWith f cf1 cf2 
  where f (w1,a) (w2,_) = (max w1 w2,a) 

getMaxColWidth :: ColFormat -> Width
getMaxColWidth cf = maximum $ map fst cf

-- | Generate report from Table List 
makeReport :: ROpts -> [Table] -> PP.Doc
makeReport os ts = PP.vcat$ L.intersperse PP.empty $ map (makeTable os) ts 


-- | Generate doc from Table
makeTable :: ROpts -> Table -> PP.Doc
makeTable  os t = PP.text (tableTitle t) PP.$$ (makeCol os rf $ map (makeRow os cft) xt) PP.$$ PP.text (tableSubTitle t) 
  where
    rf = rowFormat $ tableFormat t
    cf = colFormat $ tableFormat t
    cft = if transpose then maxColWidth cf (repeat (getMaxColWidth cf,HLeft)) else cf
    x = buildDocTable $ tableData t
    transpose = L.elem RVertical os
    xt = if transpose then L.transpose x else x

-- | Generate doc table including title rows and colums
buildDocTable :: TableData -> [[Cell]]
buildDocTable td =
   titleRow td ++
   L.zipWith3 (\x y z -> x ++ y ++ z)
      (titleCols td) (tableBody td) (endCols td)

-- | Generate Table Row
makeRow :: ROpts -> ColFormat -> [Cell]  -> PP.Doc
makeRow os cf cs = PP.hcat (zipWith (makeCell os) cf cs)

-- | Generate Table Cell
makeCell :: ROpts -> (Width,Align) -> Cell -> PP.Doc
makeCell _os (w,HRight) (Cell l c) = PP.hcat (replicate (w-l) PP.space ++[c])
makeCell _os (w,HLeft) (Cell l c) = PP.hcat ([c]++replicate (w-l) PP.space)
-- makeCell (w,HMid) (Cell l c) = PP.hcat ([c]++replicate (w-l) PP.space) where h = (w-l)/2

-- | Generate Table Column
makeCol :: ROpts -> RowFormat -> [PP.Doc] -> PP.Doc
makeCol _os _rf rs = PP.vcat rs

-- | To Table Class to defining generation of Documents  
class ToTable a where
      toTable :: ROpts -> (String,a) -> [Table]

instance (Show a) => ToTable [[a]] where
      toTable _os (ti,xs)  = [Table {tableTitle = "Matrix - " ++ ti,
                                    tableFormat = autoFormat td,
                                    tableData = td,
                                    tableSubTitle = ""}]

        where td = TableData {tableBody = map (map (toDoc show)) xs,
                              titleCols = [],
                              titleRow = [],
                              endCols = []}

-- | convert raw data to doc elements, using given function
toDoc :: (a->String) -> a -> Cell
toDoc f xs = Cell (length $ f xs) (PP.text $ f xs)

-- | generate Auto Format from Table data
autoFormat :: TableData -> TableFormat
autoFormat td = TableFormat {colFormat = zip cf (repeat HLeft),
                             rowFormat = replicate (length x) 0}
  where
    x = buildDocTable td
    cf = map f $ L.transpose x where f col = (maximum $ map cellWidth col)+2


-- | OutPut Functions  --------------------------------------------
-- | TODO: write formatDocHor versions of this functions.
report :: (ToTable a) => ROpts -> (String,a) -> IO ()
report os = putStrLn . PP.render . (makeReport os) . (toTable os) 
  