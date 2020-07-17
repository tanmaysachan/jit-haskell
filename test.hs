module test where

type Name = String

abc :: Name -> Name
abc a = a

main :: IO()
main = do
    printf "Hello"
    return ()
