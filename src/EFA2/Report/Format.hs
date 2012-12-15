module EFA2.Report.Format where

-- * special Unicode characters

delta :: Char
delta = '\x2206'

heart :: Char
heart = '\x2665'


-- * common output types

newtype Plain = Plain { unPlain :: String }

newtype Latex = Latex { unLatex :: String }

