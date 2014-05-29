module Qubit where

data Ket = Zero | One deriving (Eq, Ord, Enum, Read)

instance Show Ket where
  show s = case s of
    Zero -> "|0>"
    One -> "|1>"

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
expC :: Double -> Complex
expC theta = Comp (cos theta) (sin theta)


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

data Kets = Q [Ket] deriving Eq

instance Show Kets where
  show (Q l) = "|" ++ show2 l ++ ">" where
    show2 [] = ""
    show2 (x:s) = tmp ++ (show2 s) where
      tmp = if (x == Zero) then "0" else "1"

data Qubits = State [(Kets, Complex)]

qZero :: Qubits
qZero = State([(Q [Zero], 1), (Q [One], 0)])

qOne :: Qubits
qOne = State([(Q[Zero], 0), (Q [One], 1)])

qMoy :: Qubits
qMoy = signum $ State([(Q[Zero], 1), (Q [One], 1)])

qInv :: Qubits
qInv = signum $ State([(Q[Zero], 1), (Q [One], -1)])

-- Pretty printing.
instance Show Qubits where
  show (State []) = ""
  show (State [(k,c)]) = "(" ++ show c ++ ")" ++ (show k)
  show (State ((k,c):s)) = "(" ++ show c ++ ")" ++ (show k) ++ " + " ++ show (State s)

op :: (Complex -> Complex -> Complex) -> Qubits -> Qubits -> Qubits
op f (State l) (State m) = State (zip (map fst l) (zipWith f (map snd l) (map snd m)))

opU :: (Complex -> Complex) -> Qubits -> Qubits
opU f (State l) = State [(q,f p) | (q,p) <- l]

-- Some useful calculations, some are useless, and are just here for
-- the sake of completeness.

instance Num Qubits where
  (+) q1 q2 = op (+) q1 q2
  (-) q1 q2 = op (-) q1 q2
  (*) q1 q2 = op (*) q1 q2

  abs q = State [(Q [Zero], 1)]                     -- Totally useless.

  signum q = opU (mDC (1/(sqrt d))) q               -- Awesomely useful, since the "direction" of
    where State l = opU abs $ op (*) q q            -- Qubits is all that matters.
          Comp d _ = sum $ map snd l                -- It's like a "normalize" function.
                                                    
  fromInteger i = State [(Q [Zero], 1)]             -- Useless.


-- A Gate is a function applied to a Register or a Qubit, or several Qubits.
-- A Gate is homeomorphic to a (Qubit Qubit) since it is fundamentally
-- a 2x2 matrix in the {|0>, |1>} basis.

data Gate = G Qubits Qubits

-- hadamard is a gate that transforms |0> into (|0> + |1>) / (sqrt 2)
--                                and |1> into (|0> - |1>) / (sqrt 2)
hadamard :: Gate
hadamard = G qMoy qInv

-- shift theta transforms |1> into exp(i*theta)|1>.
shift :: Double -> Gate
shift theta = G qZero $ opU (*(expC theta)) qOne  



-- TODO : Séparer le code en plusieurs fichiers.
--        Autres portes.
--        Bind.
--        Réécrire Register, Qubits, Gate...
--        Peut-être remplacer Qubit par Register prenant des Kets en entrée
--        (un Qubit est un Register de taille 2).
