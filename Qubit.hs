module Qubit where

-- Some imports are done to ensure efficient matrix calculations.
import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Complex 
import Numeric.Container


-- Multiplication by a scalar.
mDC :: Double -> Complex Double -> Complex Double
mDC d (a :+ b) = (a*d) :+ (b*d)

-- expC theta = exp(i*theta) (rad).
-- Fonction classique de l'exponentielle complexe.
expC :: Double -> Complex Double
expC theta = (cos theta) :+ (sin theta)

-- Fonction qui génère des registres dans l'ordre.
-- Utile pour la fonction "show" d'un registre.
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

  abs q = qOne 2 0                                 -- Totally useless.

  signum q = opU (mDC (1/(sqrt d))) q               -- Awesomely useful, since the "direction" of
    where Qubits l = opU abs $ op (*) q q           -- Register is all that matters.
          d :+ _ = V.foldVector (+) 0 l             -- It's like a "normalize" function.
                                                    
  fromInteger i = qOne 2 0                         -- Useless.

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

-- Allows a Register to be an argument of a matrix, seen as a function (like Gates).
mQM :: (M.Matrix (Complex Double)) -> Register -> Register
mQM m q = Qubits (m <> v) where Qubits v = q

-- Transforms a list of Register into a matrix, in order to construct Gates.
qToM :: [Register] -> (M.Matrix (Complex Double))
qToM l = M.fromColumns [v | (Qubits v) <- l]

-- Allows calculations done on single Qubits to be done globally on Registers
-- without having to split the Registers.
expandMat :: Int -> (M.Matrix (Complex Double)) -> (M.Matrix (Complex Double))
expandMat 1 m = m
expandMat i m = M.fromBlocks [[mapMatrix (* (m @@> (j,k))) n | k <- [0..l]] | j <- [0..l]]
  where n = expandMat (i-1) m
        l = (M.rows m) - 1

-- hadamard is a gate that transforms |0> into (|0> + |1>) / (sqrt 2)
--                                and |1> into (|0> - |1>) / (sqrt 2)
hadamard :: Int -> Gate
hadamard i = G $ mQM $ expandMat i $ qToM [qMoy, qInv]

-- shift theta transforms |1*> into exp(i*theta)|1*>, id being performed on the other Qubits.
shift :: Double -> Int -> Int -> Gate
shift theta j i = G $ mQM $ expandMat i $ qToM $ [qOne k x | x <- [0.. k-2]]++[opU (*(expC theta)) (qOne k (k-1))]
  where k = 2^j

-- cNOT inverts 2 and 3.
cNOT :: Int -> Gate
cNOT i = G $ mQM $ expandMat i $ qToM [qOne 4 0, qOne 4 1, qOne 4 3, qOne 4 2]



-- TODO : Séparer le code en plusieurs fichiers (?).
--        Circuits.
