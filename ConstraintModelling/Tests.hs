module Tests where
import Data.List
import qualified Data.Map as M
import SATSolver
import WFF
import Test.QuickCheck

-- The sat solver is correct if each model it produces for a
-- wff is actually a model
sat_correct :: WFF -> Bool
sat_correct wff = all id [test wff sol | sol <- sat $ toSat wff]

-- Check that the unassigned variables left-biased-unioned with the solution
-- are correct for every possible assignment of variables
test :: WFF -> Solution -> Bool
test wff soln = all id (map ((flip eval) wff) [M.union (solnMap soln) (M.fromList vals) | vals <- ((expand (variables wff)))])

-- Go from a solution to a map of variables
solnMap = M.fromList . (map makePair)
    where
        makePair (V s) = (s, True)
        makePair (N s) = (s, False)

expand xs = fmap (zip xs) (bools (length xs))

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = do
            b <- [True, False]
            (fmap (b:) (bools (n-1)))
