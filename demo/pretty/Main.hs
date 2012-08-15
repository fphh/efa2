
import qualified Text.PrettyPrint as PP
import qualified Text.Show.Pretty as SP
import qualified Data.List as L
import Text.PrettyPrint ((<+>),(<>),($+$),($$))


l = [0,1,1,0,1]
l2 = [l,l]

lp = map PP.double l

tabs = [10,10..100]

z11 = PP.text "z11"
z12 = PP.text "z12"    
z21 = PP.text "z21"    
z22 = PP.text "z22"    

zt1 = PP.text "Zeile1"
zt2 = PP.text "Zeile2"
st1 = PP.text "Spalte1"
st2 = PP.text "Splate2"

empty = PP.empty
zeile1 = PP.nest 0 z11 <+> PP.nest 50 z12 
zeile2 = PP.nest 0 z21 <+> PP.nest 50 z22    
tabelle = zeile1 $+$ zeile2     

tabelle2 = [[empty,st1,st2],[zt1,z11,z12],[zt2,z21,z22]]
tabelle3 = tabelle2 ++ tabelle2
format t = PP.vcat $ map PP.hsep t   

(<++>) x y = L.transpose (L.transpose x ++ L.transpose y) 

tabelle4 = tabelle2 <++> tabelle2

main = do
  
  putStrLn $ PP.render $ PP.hcat $ L.intersperse PP.space lp 
  
  putStrLn $ PP.render $ PP.fsep lp 
  
  putStrLn $ PP.render $ PP.nest 10 (PP.double 1) PP.$$ PP.nest 20 (PP.double 1)
  putStrLn $ PP.render $ PP.nest 10 (PP.double 1100) PP.$$ PP.nest 20 (PP.double 20)
  
  putStrLn "Tabelle" 
  putStrLn $ PP.render tabelle
  
  putStrLn "Tabelle2"
  putStrLn $ PP.render $ PP.nest 50 $ PP.fcat [PP.nest 0 tabelle, PP.nest 20 tabelle]
  
  putStrLn $ PP.render $ format tabelle2
  putStrLn ""
  putStrLn $ PP.render $ format $ L.transpose $ tabelle2
  putStrLn ""
  putStrLn $ PP.render $ format $ tabelle3
  putStrLn ""
  putStrLn $ PP.render $ format $ tabelle4
  