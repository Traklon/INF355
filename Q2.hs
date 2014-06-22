module Qubit where

-- Some imports are done to ensure efficient matrix calculations.
import qualified Data.Packed.Vector as V
import qualified Data.Vector as VV
import qualified Data.Packed.Matrix as M
import Data.Bits ((.&.), xor)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Complex 
import Numeric.Container
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (sortBy, groupBy)


-- Multiplication by a scalar.
mDC :: Double -> Complex Double -> Complex Double
mDC d (a :+ b) = (a*d) :+ (b*d)

-- expC theta = exp(i*theta) (rad).
-- Classical function for complex exponentiation.
expC :: Double -> Complex Double
expC theta = (cos theta) :+ (sin theta)

-- Fonction that generates registers in the right order.
genKets :: Int -> [String]
genKets i = reverse (gK i) where
  gK 0 = ["|0>"]
  gK i = ("|" ++ (showIntAtBase 2 intToDigit i "") ++ ">"):(gK (i-1))



-- Introduction of Register.
--
-- In quantum computing, Bits are replaced with Register.
-- They can be in state |0> or |1> just like normal Bits, but also
-- in a linear combination of them, in regard with the complex field.
--
-- The two Complex fields represent the coordinates in regard with
-- the {|0>, |1>} basis.
--
-- However, when we mesure the value of a Qubit, we only see one
-- value, drawn at random with a probability being proportional with
-- the module of the coefficients. Then, the value is fixed.
-- I still don't know whether or not I should state if a value is
-- fixed in the Qubit definition. I will see later.
--
-- Registers are concatenated Register. They can be seen as binary numbers.
--
-- Now, all the functions are directly done on Registers.
-- In fact, a single Qubit is a register too.
-- This makes calculations easier and cleaner.
--
-- Registers are represented by a Vector. The value in the index i represent
-- the possible value of the register i-1.
-- This is much quicker than with lists, and much more logical.
-- Furthermore, it interacts well with matrices.

data Register = Qubits (V.Vector (Complex Double))

data Qubit = Q (Complex Double) (Complex Double) deriving (Show)

data QubitList = QL (VV.Vector (Qubit)) deriving (Show)

qlToReg :: QubitList -> [Int] -> Register
qlToReg (QL v) l = Qubits $ V.fromList $ qlToReg' [(VV.!) v i | i <- l]
  where qlToReg' [(Q a b)] = [a,b]
        qlToReg' ((Q a b):s) = let r = (qlToReg' s) in (map (*a) r) ++ (map (*b) r)

mask :: Int -> Int -> [Bool]
mask i n = map (\x -> ((x `div` 2^i) `mod` 2) == 1) [0..(n-1)]

mask' :: Int -> Int -> [Bool]
mask' i n = map (\x -> ((x `div` 2^i) `mod` 2) == 0) [0..(n-1)]

-- Some useful Registers.
qMoy :: Register
qMoy = signum $ Qubits $ V.fromList [1,1]

qInv :: Register
qInv = signum $ Qubits $ V.fromList [1,-1]

-- Pretty printing.
instance Show Register where
  show (Qubits t) = show' t' where
    t' = zip (genKets (V.dim t)) (V.toList t)
    show' [] = ""
    show' [(k,c)] = "(" ++ show c ++ ")" ++ (show k)
    show' ((k,c):s) = "(" ++ show c ++ ")" ++ (show k) ++ " + " ++ show' s

-- Allows operations between two Qubits, useful for the instanciation of Num.
op :: (Complex Double -> Complex Double -> Complex Double) -> Register -> Register -> Register
op f (Qubits t) (Qubits u) = Qubits $ V.zipVectorWith f t u

-- Same with unary operations.
opU :: (Complex Double -> Complex Double) -> Register -> Register
opU f (Qubits l) = Qubits $ V.mapVector f l

-- Some useful calculations, some are useless, and are just here for
-- the sake of completeness.
instance Num Register where
  (+) q1 q2 = op (+) q1 q2
  (-) q1 q2 = op (-) q1 q2
  (*) q1 q2 = op (*) q1 q2

  abs q = qOne 2 0                                  -- Totally useless.

  signum q = opU (mDC (1/(sqrt d))) q               -- Awesomely useful, since the "direction" of
    where Qubits l = opU abs $ op (*) q q           -- Register is all that matters.
          d :+ _ = V.foldVector (+) 0 l             -- It's like a "normalize" function.
                                                    
  fromInteger i = qOne 2 0                          -- Useless.

-- Generates a Register of size i with 0 everywhere except in j.
qOne :: Int -> Int -> Register 
qOne i j = Qubits $ V.fromList [if (x == j) then 1 else 0 | x <- [0..(i-1)]] 

-- Generates a Register of size n, with every probability being the same,
-- the sum of their squares being 1.
qEq :: Int -> Register
qEq n = signum $ Qubits $ V.fromList $ replicate n 1

-- A Gate is a function applied to a Register.
-- A Gate is homeomorphic to a matrix of complex numbers,
-- in the {|0>, |1>...} basis.

data Gate = G (Register -> Register)
--data Gate = (Explicit Mat) | (Implicit Gate [Bool])

-- Allows a Register to be an argument of a matrix, seen as a function (like Gates).
mQM :: (M.Matrix (Complex Double)) -> Register -> Register
mQM m q = Qubits (m <> v) where Qubits v = q

-- Transforms a list of Register into a matrix, in order to construct Gates.
qToM :: [Register] -> (M.Matrix (Complex Double))
qToM l = M.fromColumns [v | (Qubits v) <- l]

-- Allows calculations done on single Qubits to be done globally on Registers
-- without having to split the Registers.

toBin :: Int -> [Bool]
toBin 0 = [False]
toBin i = reverse $ toBin' i
  where toBin' 0 = []
        toBin' i = (mod i 2 == 1):(toBin' (div i 2))

fromBin :: [Bool] -> Int
fromBin l = fromBin' 1 0 $ reverse l
  where fromBin' _ v [] = v
        fromBin' i v (x:s) = if x then (fromBin' (2*i) (v+i) s) else (fromBin' (2*i) v s)

filter' :: [Bool] -> [a] -> [a]
filter' f l = map snd $ filter fst $ zip (map ((!!) f) [0..]) l

fonction :: [Bool] -> M.Matrix (Complex Double) -> (Int,Int) -> (Complex Double)
fonction ind g (i,j) = 
  if (val == 0) then g @@> (i', j') else 0
    where val = xor i j .&. (fromBin (map not ind))
          n = length ind
          bi = toBin i
          bj = toBin j
          i' = fromBin $ filter' ind $ [False | _ <- [length bi .. n-1]] ++ bi
          j' = fromBin $ filter' ind $ [False | _ <- [length bj .. n-1]] ++ bj
             

expandMat' :: [Bool] -> M.Matrix (Complex Double) -> M.Matrix (Complex Double)
expandMat' ind g = buildMatrix n n (fonction ind g)
  where n = (^(length ind)) 2

expandMat :: Int -> (M.Matrix (Complex Double)) -> (M.Matrix (Complex Double))
expandMat 1 m = m
expandMat i m = M.fromBlocks [[mapMatrix (* (m @@> (j,k))) n | k <- [0..l]] | j <- [0..l]]
  where n = expandMat (i-1) m
        l = (M.rows m) - 1

-- hadamard is a gate that transforms |0> into (|0> + |1>) / (sqrt 2)
--                                and |1> into (|0> - |1>) / (sqrt 2)
hadamard :: Int -> Gate
hadamard i = G $ mQM $ expandMat i $ qToM [qMoy, qInv]

-- shift theta transforms |1*> into exp(i*theta)|1*>,
-- id being performed on the other Qubits.
shift :: Double -> Int -> Int -> Gate
shift theta j i = G $ mQM $ expandMat i $ qToM $ [qOne k x | x <- [0.. k-2]]++[opU (*(expC theta)) (qOne k (k-1))]
  where k = 2^j

-- cNOT inverts 2 and 3.
cNOT :: Int -> Gate
cNOT i = G $ mQM $ expandMat i $ qToM [qOne 4 0, qOne 4 1, qOne 4 3, qOne 4 2]

intToBin :: Int -> [Bool]
intToBin x = reverse $ intToBin' x
  where intToBin' 0 = []
        intToBin' y = let (a,b) = quotRem y 2 in (b == 1):intToBin' a

select :: [Int] -> [a] -> [a]
select [] _ = []
select (x:s) l = (l!!x):(select s l)

selBits :: [Int] -> Int -> [Bool] 
selBits l a = select l $ intToBin a

-- Creates a Register with only the selected Qubits.
extract :: [Int] -> Register -> Register
extract l (Qubits q) = signum $ Qubits q'
  where tmp = zip [0..((dim q)-1)] (V.toList q)
        fun = (selBits l) . fst
        v = groupBy ((==) `on` fun) (sortBy (comparing fun) tmp)
        q' = V.fromList $ map sum $ [map snd t | t <- v]

data Circuit = C [(Gate, [Int])]

--transfo :: Circuit -> QubitList -> QubitList
--transfo (C []) ql = ql
--transfo (C ((G f, l):s)) ql = transfo (C s) $ regToQL (f (qlToReg ql l)) ql l 

toffoli :: Circuit
toffoli = C [(h,[2]), (v, [1,2]), (c, [0,1]), (v', [1,2]), (c, [0,1]), (v, [0,2]), (h, [2])] 
  where h = hadamard 1
        v = shift (pi/2) 2 1
        v' = shift  (-pi/2) 2 1
        c = cNOT 1

