
-- -----------------------------------------------------------------------------
-- The Complex type

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
--
-- The 'Foldable' and 'Traversable' instances traverse the real part first.
--
-- Note that `Complex`'s instances inherit the deficiencies from the type
-- parameter's. For example, @Complex Float@'s 'Ord' instance has similar
-- problems to `Float`'s.
data Complex = Complex Float Float
    deriving (Eq,Show)

-- -----------------------------------------------------------------------------
-- Functions over Complex

-- | Extracts the real part of a complex number.
realPart :: Complex -> Float
realPart (Complex real _) = real

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex -> Float
imagPart (Complex _ imag) = imag

-- | The conjugate of a complex number.
conjugate :: Complex -> Complex
conjugate (Complex real imag) = Complex real (-imag)

-- | Form a complex number from polar components of magnitude and phase.
mkPolar :: Float -> Float -> Complex
mkPolar r theta = Complex (r * cos theta) (r * sin theta)

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
cis :: Float -> Complex
cis theta = Complex (cos theta) (sin theta)

plusComplex :: Complex -> Complex -> Complex
plusComplex (Complex x y) (Complex x' y') = Complex (x + x') (y + y')
minusComplex :: Complex -> Complex -> Complex
minusComplex (Complex x y) (Complex x' y') = Complex (x - x') (y - y')

sumComplex :: [Complex] -> Complex
sumComplex [] = Complex 0.0 0.0
sumComplex (x:xs) = plusComplex x (sumComplex xs)
