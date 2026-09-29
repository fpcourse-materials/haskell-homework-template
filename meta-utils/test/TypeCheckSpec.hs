-- | Тесты "TypeCheck": что форма типа считает одинаковым, что различает, как ведут себя
-- заглушка и неверный ответ.
module TypeCheckSpec (tests) where

import Control.Exception (try)
import Data.List (isInfixOf)
import Test.HUnit (Test (..), assertBool, assertEqual, assertFailure)
import Test.HUnit.Lang (HUnitFailure (..), formatFailureReason)
import TodoException (TodoException)
import TypeCheck
import TypeCheckSamples

tests :: Test
tests = TestList
  [ TestLabel "синонимы: что считается одинаковым" testSynonymSame
  , TestLabel "синонимы: что различается" testSynonymDifferent
  , TestLabel "синонимы: точный вид формы" testSynonymExact
  , TestLabel "сигнатуры" testSignatures
  , TestLabel "предсказание типа: эталон выводит GHC" testPrediction
  , TestLabel "заглушка" testStub
  , TestLabel "исход проверки" testOutcome
  , TestLabel "typeIs" testTypeIs
  ]

same, different :: String -> String -> String -> Test
same what l r = TestCase $ assertEqual what l r
different what l r = TestCase $ assertBool (what <> ": формы совпали: " <> l) $ l /= r

pair :: String
pair = $(synonymShape ''Pair)

testSynonymSame :: Test
testSynonymSame = TestList
  [ same "имена переменных" pair $(synonymShape ''PairRenamed)
  , same "лишние скобки" pair $(synonymShape ''PairParens)
  , same "аннотация кайнда Type" pair $(synonymShape ''PairKinded)
  , same "String и [Char]" $(synonymShape ''Name) $(synonymShape ''NameAsList)
  ]

testSynonymDifferent :: Test
testSynonymDifferent = TestList
  [ different "порядок аргументов функции" pair $(synonymShape ''PairSwappedArgs)
  , different "порядок параметров синонима" pair $(synonymShape ''PairSwappedParams)
  , different "лишний параметр вместо квантора" pair $(synonymShape ''PairExtraParam)
  , different "другой тип" pair $(synonymShape ''PairTuple)
  , different "пара и число" pair $(synonymShape ''Nat)
  ]

testSynonymExact :: Test
testSynonymExact = TestList
  [ same "пара Чёрча" "2 (forall v3 (-> (-> v1 (-> v2 v3)) v3))" pair
  , same "число Чёрча" "0 (forall v1 (-> (-> v1 v1) (-> v1 v1)))" $(synonymShape ''Nat)
  , same "списки, кортежи, конструкторы" "1 ([] ((,) Int (Maybe v1)))" $(synonymShape ''Table)
  , same "не синоним" "<not a type synonym>" $(synonymShape ''NotSynonym)
  ]

constLikeShape :: String
constLikeShape = $(shapeOfType 'constLike)

testSignatures :: Test
testSignatures = TestList
  [ same "точный вид" "(forall v1 v2 (-> v1 (-> v2 v1)))" constLikeShape
  , same "имена переменных" constLikeShape $(shapeOfType 'constLikeRenamed)
  , same "явный forall" constLikeShape $(shapeOfType 'constLikeExplicit)
  , same "порядок переменных под forall" constLikeShape $(shapeOfType 'constLikeReordered)
  , different "какой аргумент возвращается" constLikeShape $(shapeOfType 'flipConstLike)
  , different "частный случай общего типа" constLikeShape $(shapeOfType 'tooSpecific)
  , same "порядок ограничений" $(shapeOfType 'withContext) $(shapeOfType 'withContextReordered)
  , different "наличие ограничений" $(shapeOfType 'withContext) $(shapeOfType 'withoutContext)
  , different "вложенность квантора" $(shapeOfType 'rankTwo) $(shapeOfType 'rankOne)
  ]

-- | Эталон — выражение без сигнатуры, его тип выводит GHC; ответ студента — сигнатура
-- при заглушке. Формы сравниваются, сам ответ в тесте не записан.
testPrediction :: Test
testPrediction = TestList
  [ same "const id" $(shapeOfType 'inferredConstId) $(shapeOfType 'predictedConstId)
  , same "flip const" $(shapeOfType 'inferredFlipConst) $(shapeOfType 'predictedFlipConst)
  , same "uncurry (flip const)" $(shapeOfType 'inferredUncurry) $(shapeOfType 'predictedUncurry)
  , same "композиция" $(shapeOfType 'inferredCompose) $(shapeOfType 'predictedCompose)
  , different "неверное предсказание" $(shapeOfType 'inferredUncurry) $(shapeOfType 'predictedUncurryWrong)
  , same "const id и flip const — один тип" $(shapeOfType 'inferredConstId) $(shapeOfType 'inferredFlipConst)
  ]

testStub :: Test
testStub = TestList
  [ TestCase $ assertBool "синоним-заглушка" $ isTodo $(synonymShape ''Stub)
  , TestCase $ assertBool "синоним-заглушка с параметрами" $ isTodo $(synonymShape ''StubWithParams)
  , TestCase $ assertBool "сигнатура-заглушка" $ isTodo $(shapeOfType 'stubValue)
  , TestCase $ assertBool "ответ — не заглушка" $ not $ isTodo pair
  , TestCase $ assertBool "тип с Todo внутри — не заглушка" $ not $ isTodo "1 (Maybe Todo)"
  ]

data Outcome = Passed | Failed String | NotStarted deriving (Eq, Show)

outcome :: Test -> IO Outcome
outcome = \case
  TestCase action -> try (try action) >>= \case
    Left (_ :: TodoException) -> pure NotStarted
    Right (Left (HUnitFailure _ reason)) -> pure $ Failed $ formatFailureReason reason
    Right (Right ()) -> pure Passed
  _ -> assertFailure "ожидался TestCase" >> pure Passed

testOutcome :: Test
testOutcome = TestList
  [ TestCase $ outcome (assertShape "задача" pair $(synonymShape ''PairRenamed)) >>=
      assertEqual "верный ответ" Passed
  , TestCase $ outcome (assertShape "задача" pair $(synonymShape ''Stub)) >>=
      assertEqual "заглушка — не провал, а «не начато»" NotStarted
  , TestCase $ outcome (assertShape "задача" pair $(synonymShape ''PairTuple)) >>= \case
      Failed message -> do
        assertBool "в сообщении есть имя задачи" $ "задача" `isInfixOf` message
        assertBool "в сообщении нет ожидаемого ответа" $ not $ pair `isInfixOf` message
      other -> assertFailure $ "неверный ответ должен быть провалом, а получено " <> show other
  , TestCase $ outcome (assertShapeMarked "задача" "Todo" pair pair) >>=
      assertEqual "маркер-заглушка важнее совпадения формы" NotStarted
  ]

testTypeIs :: Test
testTypeIs = TestList
  [ TestCase $ outcome (typeIs @(Maybe Int) @(Maybe Int) "задача") >>= assertEqual "тот же тип" Passed
  , TestCase $ outcome (typeIs @Name @NameAsList "задача") >>= assertEqual "синонимы раскрываются" Passed
  , TestCase $ outcome (typeIs @Int @Todo "задача") >>= assertEqual "заглушка" NotStarted
  , TestCase $ outcome (typeIs @Int @Bool "задача") >>= \case
      Failed message -> assertBool "в сообщении нет ожидаемого ответа" $ not $ "Int" `isInfixOf` message
      other -> assertFailure $ "неверный тип должен быть провалом, а получено " <> show other
  ]
