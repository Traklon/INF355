module Qubit where

-- Some imports are done to ensure efficient matrix calculations.
import qualified Data.Vector as V
import qualified Data.Matrix as M
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- Introduction of Complex numbers, taking 2 Doubles as parameters.
data Complex = Comp Double Double deriving (Eq,  Read)

-- Pretty printing.
instance Show Complex where
  show (Comp r i) =
    if (i == 0)
      then show r
      else if (r == 0)
        then ((show i)++"i")
        else if (i < 0)
          then ((show r)++(show i)++"i")
          else ((show r)++"+"++(show i)++"i")

-- Useful and classical calculations between Complex numbers.
instance Num Complex where
  (+) (Comp a b) (Comp c d) = Comp (a+c) (b+d)
  (-) (Comp a b) (Comp c d) = Comp (a-c) (b-d)
  (*) (Comp a b) (Comp c d) = Comp (a*c-b*d) (a*d+b*c)
  abs (Comp a b) = Comp (sqrt(a*a+b*b)) 0
  signum (Comp a b) = mDC (1/d) (Comp a b) where d = sqrt (a*a+b*b)
  fromInteger i = Comp (fromIntegral i) 0

instance Fractional Complex where
  (/) (Comp a b) (Comp c d) = Comp ((a*c+b*d)/(c*c+d*d)) ((b*c-d*a)/(c*c+d*d))
  fromRational r = Comp (fromRational r) 0

-- Multiplication by a scalar.
mDC :: Double -> Complex -> Complex
mDC d (Comp a b) = Comp (a*d) (b*d)

-- expC theta = exp(i*theta) (rad).
-- Fonction classique de l'exponentielle complexe.
expC :: Double -> Complex
expC theta = Comp (cos theta) (sin theta)

-- Fonction qui génère des registres dans l'ordre.
-- Utile pour la fonction "show" d'un registre.
genKets :: Int -> [String]
genKets i = reverse (gK i) where
  gK 0 = ["|0>"]
  gK i = ("|" ++ (showIntAtBase 2 intToDigit i "") ++ ">"):(gK (i-1))



-- Introduction of Qubits.
--
-- In quantum computing, Bits are replaced with Qubits.
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
-- Registers are concatenated Qubits. They can be seen as binary numbers.
--
-- Now, all the functions are directly done on Registers, called "Qubits".
-- In fact, a single Qubit is a register too.
-- This makes calculations easier and cleaner.
--
-- Qubits are represented by a Vector. The value in the index i represent
-- the possible value of the register i-1.
-- This is much quicker than with lists, and much more logical.
-- Furthermore, it interacts well with matrices.

data Qubits = State (V.Vector Complex)

-- Some useful Qubits
qZero :: Qubits
qZero = State $ V.fromList [1, 0]

qOne :: Qubits
qOne = State $ V.fromList [0, 1]

qMoy :: Qubits
qMoy = signum $ State $ V.fromList [1,1]

qInv :: Qubits
qInv = signum $ State $ V.fromList [1,-1]

-- Pretty printing.
instance Show Qubits where
  show (State t) = show' t' where
    t' = zip (genKets (V.length t)) (V.toList t)
    show' [] = ""
    show' [(k,c)] = "(" ++ show c ++ ")" ++ (show k)
    show' ((k,c):s) = "(" ++ show c ++ ")" ++ (show k) ++ " + " ++ show' s

-- Allows operations between 2 Qubits, useful for the instanciation of Num.
op :: (Complex -> Complex -> Complex) -> Qubits -> Qubits -> Qubits
op f (State t) (State u) = State $ V.zipWith f t u

-- Same with unary operations.
opU :: (Complex -> Complex) -> Qubits -> Qubits
opU f (State l) = State $ V.map f l

-- Some useful calculations, some are useless, and are just here for
-- the sake of completeness.
instance Num Qubits where
  (+) q1 q2 = op (+) q1 q2
  (-) q1 q2 = op (-) q1 q2
  (*) q1 q2 = op (*) q1 q2

  abs q = qZero                                     -- Totally useless.

  signum q = opU (mDC (1/(sqrt d))) q               -- Awesomely useful, since the "direction" of
    where State l = opU abs $ op (*) q q            -- Qubits is all that matters.
          Comp d _ = V.sum l                        -- It's like a "normalize" function.
                                                    
  fromInteger i = qZero                             -- Useless.


-- A Gate is a function applied to a Register (Qubits).
-- A Gate is homeomorphic to a matrix of complex numbers,
-- in the {|0>, |1>...} basis.
data Gate = G (Qubits -> Qubits)

-- Allows a Register to be an argument of a matrix, seen as a function (like Gates).
mQM :: (M.Matrix Complex) -> Qubits -> Qubits
mQM m q = State $ V.fromList [V.sum (V.zipWith (*) (M.getRow i m) v) | i <- [1.. (V.length v)]]
  where State v = q

-- Transforms a list of Qubits into a matrix, in ordre to construct Gates.
qToM :: [Qubits] -> (M.Matrix Complex)
qToM l = foldr (M.<->) (M.matrix 0 (length l) (\_ -> 0)) [M.rowVector x | (State x) <- l]

-- hadamard is a gate that transforms |0> into (|0> + |1>) / (sqrt 2)
--                                and |1> into (|0> - |1>) / (sqrt 2)
hadamard :: Gate
hadamard = G (mQM m) where
  m = qToM [qMoy, qInv] 

-- shift theta transforms |1> into exp(i*theta)|1>.
shift :: Double -> Gate
shift theta = G (mQM m) where
  m = qToM [qZero, opU (*(expC theta)) qOne]  



-- TODO : Séparer le code en plusieurs fichiers.
--        Autres portes.
--        Ecrire fonctions pour créer des registres de base (0 partout, 1 quelque part ; équilibré...)
