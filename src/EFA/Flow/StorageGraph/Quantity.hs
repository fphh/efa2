module EFA.Flow.StorageGraph.Quantity where


class Functor f => Carry f where
   carryEnergy, carryXOut, carryXIn :: f a -> a
