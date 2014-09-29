module Main where 

import Data.List (intercalate)

import Helper


main :: IO ()
{- main = getLine >>= outFirst3 -}
main = putStrLn (show entrepreneur)


(.)   ::            (b ->   c) -> (a ->   b) -> (a ->   c)

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)

z = (<=<) x y

z 3 


h = g . f

h x= g ( f x )


dumbCompose ::              Int ->  (Int ->   String) ->       String

(>>=) :: Monad m =>       m a ->  (a -> m b) ->       m b


ma >>= (a -> mb)


(>>=) (Just 2) (\x -> Just (x*2)) :: m a
(<=<) (\x -> Just (x*2)) (\_ -> Just 2) :: a -> m b

data Maybe = Nothing | Just a

instance Monad (Maybe a) where
  return x = Just x
  (>>=) (Just x)  f = f x
  (>>=) Nothing   _ = Nothing

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  (==) = not (/=)
  (/=) = not (==)


newtype Foo = Foo Int
instance Monoid Foo where
  mempty = Foo 0
  mappend (Foo x1) (Foo x2) = Foo (x1+x2)



{- Monad m => m a -> (a -> m b) -> m b -}


{- getLine :: IO String         putStrLn :: String -> IO () -}



{- show :: Show a => a -> String -}



investor = Investor {account = Money 1000, investorGroupName = "Dollars to Donuts"}
investor2 = Investor {account = Money 2000, investorGroupName = "Dollars to Donuts"}

entrepreneur = Entrepreneur {account = Money 0, startupName = "Haskell for food"}

data Money = Money Int
data User = Investor {account :: Money, investorGroupName :: String} | Entrepreneur { account :: Money, startupName :: String }





instance Show User where
  show (nv@Investor{}) = "Investor from InvestorGroup: " ++ (investorGroupName nv) ++ "with " ++ (show $ account nv)
  show (nt@Entrepreneur{startupName = name}) = "Entrepreneur with startup: " ++ name ++ " with " ++ (show $ account nt)

instance Show Money where
  show (Money dollars) = (show dollars) ++ " dollars"

{- outFirst3 :: String -> IO () -}
{- outFirst3 s = -}
  {- let -}
    {- f :: String -> String -}
    {- f s = take 3 s -}

    {- s' = f s -}
  {- in -}
    {- putStrLn s' -}


{- getLine :: IO String -}

{- getPureLine :: String -}
{- getPureLine = "hello" -}



{- putStrLn :: String -> IO () -}
{- putStrLn getPureLine -}

{- n = 345345 -}



