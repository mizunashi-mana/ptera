module ShouldCompile where

type T = Int

f :: T
f = x where
    x = y

    y =
        let {
            ;;z = 0;
        } in z

    m = case do Nothing >>= \x -> Just x of
        Nothing{} -> do
            pure ()
        Just 'c' -> do
            pure ()
        Just{} -> do
            _ <- Just 0
            pure ()

class Cls a where
    clsMethod :: a

instance Cls () where
    clsMethod = ()
