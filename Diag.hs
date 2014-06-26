{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


mySquare :: [Char] -> Diagram B R2
mySquare t = position [(p2 (0,0.12),(text t))] `atop` position [((p2 (0,0.5)),(square 1 # lwL 0.035))]

dots :: Int -> [Bool] -> Diagram B R2
dots n l = position ((zip (map mkPoint (map snd (filter fst (zip l [n, n-1 .. 1])))) (repeat dot))++(zip (map mkPoint (map snd (filter (not . fst) (zip l [n, n-1 .. 1])))) (repeat dotV)))
  where dot       = circle (0.5/(fromIntegral n)) # fc black
        dotV      = circle (0.5/(fromIntegral n))
        mkPoint x = p2 (0,((fromIntegral x)-0.5)/(fromIntegral n))

myLine :: Diagram B R2
myLine = strokeT . fromOffsets $ [unitX]

line = myLine # lwG 0.03


wires :: Int -> Diagram B R2
wires n = position (zip (map mkLine [n, n-1 .. 1]) (repeat line))
  where mkLine x = p2 (0,((fromIntegral x)-0.5)/(fromIntegral n))

myGate :: Int -> ([Char] , [Bool]) -> Diagram B R2
myGate n (a, l) = (dots n l) ||| (mySquare a) ||| (wires n)

myCircuit :: Int -> [([Char], [Bool])] -> Diagram B R2
myCircuit n l = (wires n) ||| myC n l
  where myC n [(a,l)] = myGate n (a,l)
        myC n ((a,l):s) = (myGate n (a,l)) ||| (myC n s)

main = mainWith $ myCircuit 10 [("C", [False, False, True, True, True, True, False, True, False, False]),("H", [True, False, True,True, False, False, True, True, False, True])] # bg white


