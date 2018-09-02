{-# LANGUAGE TypeFamilyDependencies, TypeFamilies, RankNTypes #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Typeable as DT

type family Foo a = r | r -> a where
  Foo Char = Bool
  Foo Int = Double
  Foo Float = String

type LHSFoo b = forall a. Foo a ~ b => a

fncA :: Foo a -> Foo a
fncA x = x

class Bar t where
  clsF :: Foo t

instance Bar Char where
   clsF = True
instance Bar Float where
   clsF = "hello"


someFunc :: IO ()
someFunc = do
  print $ (clsF :: Bool)
  print $ DT.typeOf (undefined :: LHSFoo Bool)
  print $ DT.typeOf (fncA True)
