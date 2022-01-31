module ShouldCompile where

bool :: Int
bool = - case 3 > 5 of False -> 0; True -> (-1)
main = print (- do 4)
