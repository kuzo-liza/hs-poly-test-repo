module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 7 |*|

-- операторам +/- присвоен приоритет 6, а оператору * - 7, поэтому приоритет был расставлен по этой аналогии
-- ассоциативность у операторов +/-/* - левая

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
                                                 IntConstant _ -> expression
                                                 Variable variable | variable == varName -> replacement
                                                                   | otherwise -> expression
                                                 BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
                           IntConstant intValue -> expression
                           Variable varName -> expression
                           BinaryTerm op lhv rhv ->
                                      case (left, right) of
                                           (IntConstant leftValue, IntConstant rightValue) ->
                                                    case op of
                                                         Plus -> IntConstant (leftValue + rightValue)
                                                         Minus -> IntConstant (leftValue - rightValue)
                                                         Times -> IntConstant (leftValue * rightValue)
                                           _ -> expression
                                      where
                                            left = evaluate lhv
                                            right = evaluate rhv

-- проверяем, что левая и правая ветки после расчëтов - const, если нет, то Term не состоит только из констант