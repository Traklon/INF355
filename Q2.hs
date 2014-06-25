module Q2 where


-- Some imports are done to ensure efficient matrix calculations
-- and general optimisation.
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




-- Data structures.


-- A Qubit is simply composed of 2 Complex numbers,
-- representing the values on the basis {|0> ; |1>}.
data Qubit = Q (Complex Double) (Complex Double) deriving (Show)


-- Vector of Qubits. Used at the start, then transformed into a Register.
data QubitList = QL (VV.Vector (Qubit)) deriving (Show)


-- A register is a representation of one or several Qubits.
-- QubitList [(a*|0> + b*|1>),(c*|0> + d*|1>)] becomes
-- (ac*|00> + ad*|01> + bc*|10> + bd*|11>) as a Register.
-- Always of size 2^n for some integer n > 0.
-- Inplemented as a packed Vector because it isn't mutable.
data Register = Qubits (V.Vector (Complex Double))

-- Fonction that generates registers in the right order.
genKets :: Int -> [String]
genKets i = reverse (gK i) where
  gK 0 = ["|0>"]
  gK i = ("|" ++ (showIntAtBase 2 intToDigit i "") ++ ">"):(gK (i-1))

-- Pretty printing.
instance Show Register where
  show (Qubits t) = show' t' where
    t' = zip (genKets (V.dim t)) (V.toList t)
    show' [] = ""
    show' [(k,c)] = "(" ++ show c ++ ")" ++ (show k)
    show' ((k,c):s) = "(" ++ show c ++ ")" ++ (show k) ++ " + " ++ show' s

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


-- A Gate is a function that modifies Registers.
-- It is naturally defined as a packed matrix since it
-- is unmutable, allows fast Vector (Register) modification, and
-- is easily homeomorphic to a boolean function.
data Gate = G (M.Matrix (Complex Double)) deriving (Show) 

-- A Circuit is a list of Gates, with the Qubits they act on.
data Circuit = C [(Gate, [Int])]




-- Some useful conversions between data types/structures.


-- Multiplication by a scalar.
mDC :: Double -> Complex Double -> Complex Double
mDC d (a :+ b) = (a*d) :+ (b*d)


-- expC theta = exp(i*theta) (rad).
-- Classical function for complex exponentiation.
expC :: Double -> Complex Double
expC theta = (cos theta) :+ (sin theta)


-- Transforms a QubitList into a Register.
-- Doing the opposite is difficult.
qlToReg :: QubitList -> [Int] -> Register
qlToReg (QL v) l = Qubits $ V.fromList $ qlToReg' [(VV.!) v i | i <- l]
  where qlToReg' [(Q a b)] = [a,b]
        qlToReg' ((Q a b):s) = let r = (qlToReg' s) in (map (*a) r) ++ (map (*b) r)


-- Allows operations between two Qubits, useful for the instanciation of Num.
op :: (Complex Double -> Complex Double -> Complex Double) -> Register -> Register -> Register
op f (Qubits t) (Qubits u) = Qubits $ V.zipVectorWith f t u

-- Same with unary operations.
opU :: (Complex Double -> Complex Double) -> Register -> Register
opU f (Qubits l) = Qubits $ V.mapVector f l


-- Allows a Register to be an argument of a matrix, seen as a function (like Gates).
mQM :: Gate -> Register -> Register
mQM (G m) q = Qubits (m <> v) where Qubits v = q

-- Transforms a list of Register into a matrix, in order to construct Gates.
qToM :: [Register] -> (M.Matrix (Complex Double))
qToM l = M.fromColumns [v | (Qubits v) <- l]

-- Conversions between binary (seen as a boolean list) and decimal.
toBin :: Int -> [Bool]
toBin 0 = [False]
toBin i = reverse $ toBin' i
  where toBin' 0 = []
        toBin' i = (mod i 2 == 1):(toBin' (div i 2))

fromBin :: [Bool] -> Int
fromBin l = fromBin' 1 0 $ reverse l
  where fromBin' _ v [] = v
        fromBin' i v (x:s) = if x then (fromBin' (2*i) (v+i) s) else (fromBin' (2*i) v s)


-- Converts [1,3] 6 in [True, False, True, False, False, False].
intToBool :: [Int] -> Int -> [Bool]
intToBool l i = reverse $ itb (reverse l) i
  where itb [] n = [False | _ <- [1 .. n]]
        itb (x:s) n = if (x == n) then True:(itb s (n-1)) else False:(itb (x:s) (n-1))

-- Opposite function. 
boolToInt :: [Bool] -> [Int]
boolToInt l = map fst $ filter snd $ zip [1..(length l)] l


-- Creates an helpful mask to know which registers contains a specific value for one Qubit.
mask :: Int -> Int -> [Bool]
mask i n = map (\x -> ((x `div` 2^(i-1)) `mod` 2) == 1) [0..(n-1)]




-- Generation of useful classical Registers.


-- Same value for the 2 possibilities.
qMoy :: Register
qMoy = signum $ Qubits $ V.fromList [1,1]


-- Opposite value for the 2 possibilities.
-- Note that the module is the same.
qInv :: Register
qInv = signum $ Qubits $ V.fromList [1,-1]


-- Generates a Register of size i with 0 everywhere except in j.
qOne :: Int -> Int -> Register 
qOne i j = Qubits $ V.fromList [if (x == j) then 1 else 0 | x <- [0..(i-1)]] 


-- Generates a Register of size n, with every probability being the same,
-- the sum of their squares being 1.
qEq :: Int -> Register
qEq n = signum $ Qubits $ V.fromList $ replicate n 1




-- Operations on Gates.


-- Allows a calculation being done on only a subset of the Qubits
-- which belong to the Register to be done on the whole Register,
-- without modifying the Qubits which are not selected.
expandGate :: [Bool] -> Gate -> Gate
expandGate ind (G g) = G (buildMatrix n n (f ind g))
  where n = (^(length ind)) 2
        f ind g (i,j) = 
          if (val == 0) then g @@> (i', j') else 0
            where val = xor i j .&. (fromBin (map not ind))
                  n = length ind
                  bi = toBin i
                  bj = toBin j
                  filter' f l = map snd $ filter fst $ zip (map ((!!) f) [0..]) l
                  i' = fromBin $ filter' ind $ [False | _ <- [length bi .. n-1]] ++ bi
                  j' = fromBin $ filter' ind $ [False | _ <- [length bj .. n-1]] ++ bj


-- Allows a calculation on some Qubits being done independantly
-- on several blocks to be duplicated.
parallelize :: Int -> Gate -> Gate
parallelize 1 g = g
parallelize i (G m) = G (M.fromBlocks [[mapMatrix (* (m @@> (j,k))) n | k <- [0..l]] | j <- [0..l]])
  where (G n) = parallelize (i-1) (G m)
        l = (M.rows m) - 1



-- Some useful gates, which exist practically.


-- Identity function : similar to a wire.
-- Useful as being the representative Gate of a circuit without Gates.
iden :: Int -> Gate
iden i = parallelize i $ G (qToM [qOne 2 0, qOne 2 1])


-- hadamard is a gate that transforms |0> into (|0> + |1>) / (sqrt 2)
--                                and |1> into (|0> - |1>) / (sqrt 2)
hadamard :: Int -> Gate
hadamard i = parallelize i $ G (qToM [qMoy, qInv])

-- qNOT is the classical NOT function.
qNOT :: Int -> Gate
qNOT i = parallelize i $ G (qToM [qOne 2 1, qOne 2 0])

-- shift theta transforms |1*> into exp(i*theta)|1*>,
-- id being performed on the other Qubits.
shift :: Double -> Int -> Int -> Gate
shift theta j i = parallelize i $ G (qToM ([qOne k x | x <- [0.. k-2]]++[opU (*(expC theta)) (qOne k (k-1))]))
  where k = 2^j


-- cNOT inverts |10> and |11>.
-- It can be seen as a NOT function, controlled by the first Qubit.
cNOT :: Int -> Gate
cNOT i = parallelize i $ G (qToM [qOne 4 0, qOne 4 1, qOne 4 3, qOne 4 2])


-- swap simply swaps the Qubits.
swap :: Int -> Gate
swap i = parallelize i $ G (qToM [qOne 4 0, qOne 4 2, qOne 4 1, qOne 4 3])


-- Creates the Gate associatied with the Quantum function in the some algorithms.
oracle :: Int -> ([Bool] -> Int) -> Gate
oracle n f = G (buildMatrix (2^n) (2^n) g)
  where g (i,j) = if (j == 2*a+y) then 1 else 0
          where (a,b) = quotRem i 2
                tmp = toBin a
                y = f ([False | _ <- [(length tmp) .. n-2]]++tmp) `xor` b




-- Functions related to Circuits.

-- cTG stands for "Circuit to Gate".
-- It allows the compression of a Circuit in a single Gate.
cTG :: Circuit -> Int -> Int -> Gate
cTG c n i = parallelize i $ tmp c $ iden n
  where tmp (C []) g = g
        tmp (C ((g, l):s)) (G m) = let (G m') = expandGate (intToBool l n) g in tmp (C s) (G (m' <> m))


-- concat merges two citcuits.
concat :: Circuit -> Circuit -> Circuit
concat (C c) (C c') = C (c ++ c')


-- transfo applies a Circuit to a Register.
transfo :: Circuit -> QubitList -> Register
transfo c ql = transfo' c $ qlToReg ql [0.. (n-1)]
  where QL l = ql
        n = VV.length l
        transfo' (C []) r = r
        transfo' (C ((g, l):s)) r = transfo' (C s) $ mQM (expandGate (intToBool l n) g) r


-- Allows the calculation of the probability of the value of specific Qubits.
-- It usually terminates the calculations, at the end of circuits/algorithms.
measure :: Register -> [Bool] -> [Bool] -> Double
measure (Qubits r) qs v = realPart $ sum $ map (abs . (^2) . snd) (filter fst (zip select m))
  where m = V.toList r
        n = length qs
        conc n 0 _ _ = replicate (2^n) [True]
        conc n _ _ [] = replicate (2^n) [True]
        conc n i (x:s) (x':s') = if x then if x' then zipWith (:) (mask i (2^n)) l else zipWith (:) (map not (mask i (2^n))) l else l'
            where l = conc n (i-1) s s'
                  l' = conc n (i-1) s (x':s')
        select = map and $ conc n n qs v




-- Some useful Circuits.


-- The toffoli gate can be seen as a C²NOT gate.
-- It acts like CNOT, the control Qubit being the AND of the first 2 Qubits.
-- Useful because it is universal : any reversible circuit can be
-- constructed only with toffoli gates. 
toffoli :: Circuit
toffoli = C [(h,[3]), (v, [2,3]), (c, [1,2]), (v', [2,3]), (c, [1,2]), (v, [1,3]), (h, [3])] 
  where h = hadamard 1
        v = shift (pi/2) 2 1
        v' = shift (-pi/2) 2 1
        c = cNOT 1

-- adder acts like an... adder.
-- The third Qubit should always be 0 at the beginning.
-- The first Qubit stays the same.
-- The second Qubit receives the XOR of the first 2 Qubits.
-- The last Qubit receives the carry (the AND) of the first 2 Qubits.
adder :: Circuit
adder = C [(t,[1,2,3]),(c,[1,2])]
  where t = cTG toffoli 3 1
        c = cNOT 1


-- The n-Qubit QFT performs a Fourier transform on n Qubits.
qft :: Int -> Circuit
qft 1 = C [(hadamard 1, [1])]
qft n = C ((cTG c 4 1, [1 .. (n-1)]):([(s i n,[i,n]) | i <- [1 .. (n-1)]]++[(hadamard 1, [n])]))
  where c = qft (n-1)
        s i n = shift (pi/(2^(n-i-1))) 2 1
          

-- Creates the Deutsch-Josza circuit.
circuitDJ :: Int -> ([Bool] -> Int) -> Circuit
circuitDJ n f = C [(h, [1..n]),(oracle n f, [1..n]),(h', [1..(n-1)])]
  where h = hadamard n
        h' = hadamard $ n-1




-- Some algorithms that use Quantum mechanics.


-- Deutsch-Josza's algorithm examples.
--
-- Our implementation uses the most complete version of the problem, with
-- a function f : {0;1}^n -> {0;1} that is either constant or equilibrated.
-- The goal is to know with only 1 measurement if f is constant or equilibrated.
-- A deterministic algorithm that uses classical bits would need 2^(n-1)+1 measurements.
-- The value returned at the end is the probability that the first (n-1) Qubits are
-- all in the state 0. f is constant if it equals 1 (can be physically interpreted as a 
-- constructive interference), or equilibrated if it equals 0 (destructive interference).
-- For more information : http://en.wikipedia.org/wiki/Deutsch%E2%80%93Jozsa_algorithm
constantDJ :: Int -> Double
constantDJ n = measure r ((replicate (n-1) True)++[False]) (replicate (n-1) False)
  where tmp = QL (VV.fromList ((replicate (n-1) (Q 1 0))++[Q 0 1]))
        cdj = circuitDJ n (\_ -> 1)
        r = transfo cdj tmp

equilibratedDJ :: Int -> Double
equilibratedDJ n = measure r ((replicate (n-1) True)++[False]) (replicate (n-1) False)
  where tmp = QL (VV.fromList ((replicate (n-1) (Q 1 0))++[Q 0 1]))
        cdj = circuitDJ n (\l -> if (l!!0) then 1 else 0)
        r = transfo cdj tmp
