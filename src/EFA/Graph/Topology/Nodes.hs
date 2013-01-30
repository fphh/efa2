

module EFA.Graph.Topology.Nodes where


data Nodes = Node Int deriving (Show, Eq, Ord)

instance Enum Nodes where
         toEnum = Node
         fromEnum (Node n) = n

class (Show nty) => ShowNode nty where
      showNode :: nty -> String
      showNode = show


instance ShowNode Nodes where
         showNode = show