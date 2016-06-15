module InfluenceCalculus where
import Test.QuickCheck 

data Influence = P | Z | M | Q deriving (Ord, Eq, Show, Enum)

instance Arbitrary Influence where
    arbitrary = oneof $ map return [P, M, Z, Q]

Q .+ _ = Q
_ .+ Q = Q
Z .+ x = x
x .+ Z = x
P .+ M = Q
M .+ P = Q
M .+ M = M
P .+ P = P

Z .* _ = Z
_ .* Z = Z
Q .* _ = Q
_ .* Q = Q
M .* M = P
M .* P = M
P .* M = M
P .* P = P

(.~) P = M
(.~) M = P
(.~) x = x

x ./ Z = undefined
Z ./ x = Z
x ./ Q = Q
x ./ y = x .* y

table f = [[x `f` y | x <- [P .. Q]] | y <- [P .. Q]]

type ComplexInfluence = (Influence, Influence)

(a, b) +. (c, d) = (a .+ c, b .+ d)
(a, b) *. (c, d) = (a .* c .+ ((.~) (b .* d)), a .* d .+ c .* b) 
(a, b) /. (c, d) = (a, b) *. inv (c, d)

inv (a, b) = (a ./ ((a .* a) .+ (b .* b)), ((.~) b) ./ ((a .* a) .+ (b .* b)))
