module Qubit where

-- Ket notation. Probably not useful, and will be deleted quickly.
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

data Qubit = State Complex Complex  deriving (Eq, Read)

-- Pretty printing.
instance Show Qubit  where
  show (State x y) =
    if (y == 0)
      then ("|0>")                                              -- We get rid of useless scalars.
      else if (x == 0)
        then ("|1>")                                            -- Same here.
        else ("("++(show x)++") |0> + ("++(show y)++") |1>")        -- Here we don't do it, because
                                                                    -- it doesn't help clarity.

-- Some useful calculations, some are useless, and are just here for
-- the sake of completeness though.
instance Num Qubit where
  (+) (State a b) (State c d) = State (a+c) (b+d)
  (-) (State a b) (State c d) = State (a-c) (b-d)
  (*) (State a b) (State c d) = State (a*c) (b*d)   -- Maybe useful for matrix calculations.
                                                    -- Wait and see.

  abs (State a b) = State 1 0 
                                                    -- Totally useless.

  signum (State a b) = mDQ (1/(sqrt d)) (State a b) where Comp d _ = abs(a)*abs(a)+abs(b)*abs(b)
                                                    -- Awesomely useful, since the "direction" of
                                                    -- Qubits is all that matters.
                                                    -- It's like a "normalize" function.

  fromInteger i = State (fromIntegral i) 0          -- Useless.

-- Multiplication by a scalar.
mDQ :: Double -> Qubit -> Qubit
mDQ d (State a b) = State (mDC d a) (mDC d b)

-- Multiplication by a Complex.
mCQ :: Complex -> Qubit -> Qubit
mCQ c (State a b) = State (a*c) (b*c)

-- A Qubit Register is a list of Qubits.
data Register a = R [(a, Complex)]

-- Bind will come later.
instance Monad Register where
  return a = R [(a, Comp 1 0)]

-- A Register is an obvious functor.
instance Functor Register where
--fmap f (R l) = R (zip (map f (map fst l)) (map snd l))
  fmap f (R l) = R [(f(x),p) | (x,p) <- l]

-- A Gate is a function applied to a Register or a Qubit.
-- A Gate is homeomorphic to a (Qubit Qubit) since it is fundamentally
-- a 2x2 matrix in the {|0>, |1>} basis.
data Gate = G Qubit Qubit

-- hadamard is a gate that transforms |0> into (|0> + |1>) / (sqrt 2)
--                                and |1> into (|0> - |1>) / (sqrt 2)
hadamard :: Gate
hadamard = G (signum (State 1 1)) (signum (State 1 (-1)))

-- shift theta transforms |1> into exp(i*theta)|1>.
shift :: Double -> Gate
shift theta = G (State 1 0) (State 0 (expC theta)) 



-- TODO : Séparer le code en plusieurs fichiers.
--        Autres portes.
--        Bind.
