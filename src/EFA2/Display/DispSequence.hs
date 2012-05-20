module EFA2.Display.DispSequence (module EFA2.Display.DispSequence) where


class DisplayRecord s where
  dispRecord :: Record s -> String

instance DisplayRecord s where
  dispRecord rec = 
  
  