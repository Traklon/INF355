{-# LANGUAGE NoMonomorphismRestriction #-}

module Diag where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine




-- Draws a square with a letter.
mySquare :: [Char] -> Diagram B R2
mySquare t = (text t) <> position [((p2 (0,0.5)),(square 1 # lwL 0.035))] 


-- Draws a vertical line of dots.
-- The filled dots correspond to the gate entries, 
-- and the unfilled ones to the unchanged Qubits.
dots :: Int -> [Bool] -> Diagram B R2
dots n l = position ((zip (map mkPoint (map snd (filter fst (zip l [n, n-1 .. 1])))) (repeat dot))++(zip (map mkPoint (map snd (filter (not . fst) (zip l [n, n-1 .. 1])))) (repeat dotV)))
  where dot       = circle (0.5/(fromIntegral n)) # fc black
        dotV      = circle (0.5/(fromIntegral n))
        mkPoint x = p2 (0,((fromIntegral x)-0.5)/(fromIntegral n))


-- Draws an horizontal line.
myLine :: Diagram B R2
myLine = strokeT . fromOffsets $ [unitX]

line = myLine # lwG 0.03

-- Draws the wires (a vertical block of lines).
wires :: Int -> Diagram B R2
wires n = position (zip (map mkLine [n, n-1 .. 1]) (repeat line))
  where mkLine x = p2 (0,((fromIntegral x)-0.5)/(fromIntegral n))


-- Converts [1,3] 6 in [True, False, True, False, False, False].
intToBool :: [Int] -> Int -> [Bool]
intToBool l i = reverse $ itb (reverse l) i
  where itb [] n = [False | _ <- [1 .. n]]
        itb (x:s) n = if (x == n) then True:(itb s (n-1)) else False:(itb (x:s) (n-1))

-- Opposite function. 
boolToInt :: [Bool] -> [Int]
boolToInt l = map fst $ filter snd $ zip [1..(length l)] l


-- Draws a Gate
myGate :: Int -> ([Char] , [Bool]) -> Diagram B R2
myGate n (a, l) = (dots n l) ||| (mySquare a) ||| (wires n)


-- Draws a Circuit.
myCircuit :: Int -> [([Char], a, [Int])] -> Diagram B R2
myCircuit n l = (wires n) # bg white ||| myC n l # bg white
  where myC n [(a, _ , l)] = myGate n (a,(intToBool l n))
        myC n ((a, _ , l):s) = (myGate n (a,(intToBool l n))) ||| (myC n s)
