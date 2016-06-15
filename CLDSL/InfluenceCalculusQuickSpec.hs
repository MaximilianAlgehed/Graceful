import InfluenceCalculus
import QuickSpec
import Test.QuickCheck

data PnQ = PnQ {r :: Influence} deriving (Ord, Eq, Show)

instance Arbitrary PnQ where
    arbitrary = oneof $ map (return . PnQ) [P, M, Z]

data PnZ = PnZ {s :: Influence} deriving (Ord, Eq, Show)

instance Arbitrary PnZ where
    arbitrary = oneof $ map (return . PnZ) [P, M, Q]

sig =
    signature {
        maxTermSize = Just 7,
        instances = [
                    baseType (undefined::Influence),
                    names (NamesFor ["x", "y", "z"] :: NamesFor Influence),
                    baseType (undefined::PnQ),
                    names (NamesFor ["p"] :: NamesFor PnQ),
                    baseType (undefined::PnZ),
                    names (NamesFor ["q"] :: NamesFor PnZ)
                    ],
        constants = [
                    constant "True" True,
                    constant "False" False,
                    --constant "0" Z,
                    --constant ">" P, 
                    --constant "<" M,
                    --constant "?" Q,
                    constant "*" (.*),
                    constant "+" (.+),
                    constant "~" (.~),
                    constant "/" (./),
                    constant "r" r,
                    constant "s" s,
                    constant "!?" (/=Q),
                    constant "!0" (/=Z)
                    ]
    }

sigC = 
    signature {
        maxTermSize = Just 7,
        instances = [
                    baseType (undefined::Influence),
                    names (NamesFor ["x", "y", "z"] :: NamesFor ComplexInfluence)
                    ],
        constants = [
                    constant "True" True,
                    constant "False" False,
                    constant "0" Z,
                    constant ">" P, 
                    constant "<" M,
                    constant "?" Q,
                    constant "," ((,) :: A -> B -> (A, B)),
                    constant "*" (*.),
                    constant "+" (+.),
                    constant "r" r,
                    constant "s" s,
                    constant "inv" inv,
                    constant "/" (/.),
                    constant "!?" (/=Q),
                    constant "!0" (/=Z)
                    ]
    }
main = do
        thy <- quickSpec sig
        thy' <- quickSpec (thy `mappend` (signature {constants = [constant "0" Z,constant ">" P, constant "<" M,constant "?" Q]}))
        putStrLn "Complex numbers"
        quickSpec (thy' `mappend` sigC)
