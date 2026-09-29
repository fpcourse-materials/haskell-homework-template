{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- Выражения-эталоны записаны так, как их видит студент в условии.
{- HLINT ignore "Redundant flip" -}

-- | Образцы для тестов "TypeCheck": то, что в домашке лежало бы в модуле студента.
-- Отдельный модуль нужен Template Haskell: reify видит только уже скомпилированные имена.
module TypeCheckSamples where

import Data.Kind (Type)
import TypeCheck (Todo)

-- Синонимы: пара Чёрча в разных записях.

type Pair a b = forall c. (a -> b -> c) -> c
type PairRenamed x y = forall r. (x -> y -> r) -> r
type PairParens a b = forall c. ((a -> (b -> c)) -> c)
type PairKinded a b = forall (c :: Type). (a -> b -> c) -> c
type PairSwappedArgs a b = forall c. (b -> a -> c) -> c
type PairSwappedParams b a = forall c. (a -> b -> c) -> c
type PairExtraParam a b c = (a -> b -> c) -> c
type PairTuple a b = (a, b)

type Nat = forall a. (a -> a) -> a -> a

type Stub = Todo
type StubWithParams a b = Todo

type Name = String
type NameAsList = [Char]
type Table a = [(Int, Maybe a)]

data NotSynonym = NotSynonym

-- Значения с сигнатурами: проверяется только запись типа, поэтому тела — заглушки.

constLike :: a -> b -> a
constLike = undefined

constLikeRenamed :: x -> y -> x
constLikeRenamed = undefined

constLikeExplicit :: forall a b. a -> b -> a
constLikeExplicit = undefined

constLikeReordered :: forall b a. a -> b -> a
constLikeReordered = undefined

flipConstLike :: a -> b -> b
flipConstLike = undefined

tooSpecific :: Int -> Bool -> Int
tooSpecific = undefined

withContext :: (Show a, Eq a) => a -> String
withContext = undefined

withContextReordered :: (Eq a, Show a) => a -> String
withContextReordered = undefined

withoutContext :: a -> String
withoutContext = undefined

rankTwo :: (forall a. a -> a) -> (Int, Bool)
rankTwo _ = undefined

rankOne :: forall a. (a -> a) -> (Int, Bool)
rankOne = undefined

stubValue :: Todo
stubValue = undefined

-- Значения без сигнатур: их тип выводит GHC. Так в тесте записывается эталон задачи
-- «предскажите тип выражения» — выражением, а не ответом.

inferredConstId = const id
inferredFlipConst = flip const
inferredUncurry = uncurry (flip const)
inferredCompose f g x = f (g x)

predictedConstId :: b -> a -> a
predictedConstId = undefined

predictedFlipConst :: p -> q -> q
predictedFlipConst = undefined

predictedUncurry :: (x, y) -> y
predictedUncurry = undefined

predictedUncurryWrong :: (x, y) -> x
predictedUncurryWrong = undefined

predictedCompose :: (b -> c) -> (a -> b) -> a -> c
predictedCompose = undefined
