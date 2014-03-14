

module Main where


-- Wir sollten die tatsaechlichen Algorithmen trennen
-- von den Bedingungen, wie lange iteriert wird.
-- Das würde alles viel einfacher machen, denn
-- die Abbruchbedingungen sind nur Nebensache.
--
-- So koennen die eigentlichen Algorithmen unabhängig von
-- den Abbruchbedingungen programmiert werden.
--
-- Die Auswertungsstrategie (wie lazy) wuerde in den
-- Treiber-Funktionen implementiert werden.


-- 0. Datenstrukturen

data Optimisation = Optimisation { outerLoop :: [OuterLoop] } deriving (Show)

data OuterLoop =
  OuterLoop { outerLoopResult :: Integer,
              innerLoop :: InnerLoop } deriving (Show)

data InnerLoop = InnerLoop { items :: [Integer] } deriving (Show)

-- 1. Die eigentlichen Algorithmen (dummies)

outerAlgorithm :: Integer -> Integer
outerAlgorithm x = 503*x `mod` 101 + 1

innerAlgorithm :: Integer -> Integer
innerAlgorithm x = 2*x + 1

extractQuintessenceForInnerLoop :: Integer -> Integer
extractQuintessenceForInnerLoop x = x + 1

-- 2. Die Abbruchbedingung

condition :: OuterLoop -> OuterLoop
condition (OuterLoop ores (InnerLoop xs)) =
  OuterLoop ores (InnerLoop $ take maxStep $ takeWhile p xs)
  where
        -- Hier die Bedingung:
        p x = True

        -- Maximale Anzahl von Schritten, falls p nicht greift.
        maxStep = 10


-- 3. Der treibende Algorithmus
-- Diese Funktionen muessen nie mehr angefasst werden,
-- ausser um die Auswertungsstrategie (lazyness) zu kontrollieren.

mkInnerLoop :: Integer -> InnerLoop
mkInnerLoop n = InnerLoop $ iterate innerAlgorithm n

mkOuterLoop :: Integer -> OuterLoop
mkOuterLoop n =
  let res = outerAlgorithm n
  in OuterLoop res (mkInnerLoop (extractQuintessenceForInnerLoop res))


-- Diese Treiber-Funktion soll aufgerufen werden
-- um die Optimierung zu starten
optimisation :: Optimisation
optimisation =
  let start = 0
      loop0 = condition (mkOuterLoop start)

      f (OuterLoop ores (InnerLoop xs)) = condition (mkOuterLoop (last xs))
 
  in Optimisation $ iterate f loop0



----------------------------------------------------------------
-- Zu Illustrationszwecken
-- Die ersten 6 Schritte wie oben.

optimisation_explicit :: Optimisation
optimisation_explicit =
  let start = 0
      lastInner (OuterLoop _ (InnerLoop xs)) = last xs

      loop0 = condition (mkOuterLoop start)
      loop1 = condition (mkOuterLoop (lastInner loop0))
      loop2 = condition (mkOuterLoop (lastInner loop1))
      loop3 = condition (mkOuterLoop (lastInner loop2))
      loop4 = condition (mkOuterLoop (lastInner loop3))
      loop5 = condition (mkOuterLoop (lastInner loop4))
      loop6 = condition (mkOuterLoop (lastInner loop5))
  in Optimisation [loop0, loop1, loop2, loop3, loop4, loop5, loop6]
