import Control.Monad.State
import Data.Map
import Data.Graph.Inductive hiding (mkNode)

type Name = Node 

data Sign = P | M | Z | Q deriving (Ord, Eq, Show)

type GraphSyntax a = State (Name, Gr Name Name, Map Name Sign) a

infixl >+>
infixl >->

mkNode :: GraphSyntax Name
mkNode = do
            (i, gr, m) <- get
            put (i+1, insNode (i, i) gr, m)
            return i

(>+>), (>->) :: GraphSyntax Name -> Name -> GraphSyntax Name
g >+> w = do
            v <- g
            (i, gr, m) <- get
            put (i+1, insEdge (v, w, i) gr, insert i P m)
            return w
g >-> w = do
            v <- g
            (i, gr, m) <- get
            put (i+1, insEdge (v, w, i) gr, insert i M m)
            return w

link = return

example = do
            a <- mkNode
            b <- mkNode
            c <- mkNode
            link a >+> b >+> c >+> a
