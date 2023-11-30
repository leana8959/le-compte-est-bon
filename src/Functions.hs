module Functions where

fst :: (a, b, c) -> a
fst (a, _, _) = a

snd :: (a, b, c) -> b
snd (_, b, _) = b

trd :: (a, b, c) -> c
trd (_, _, c) = c

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)
