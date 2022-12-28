module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist list =
    reversed (reverse list)
    where reversed [] = REmpty
          reversed (h:t) = ((reversed t):<h)

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ x s = show x ++ s
    show REmpty = "[]"
    show (t:<h) = showTail t ++ show h ++ "]"
                        where showTail REmpty = "["
                              showTail (t:<h) = showTail t ++ show h ++ ","

-- showsPrec, show, showList
-- min определение: show или showPrec

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) REmpty _ = False
    (==) _ REmpty = False
    (==) (tailList1:<headList1) (tailList2:<headList2) = if (headList1 == headList2) && (tailList1 == tailList2)
                                                             then True
                                                             else False
    (/=) REmpty REmpty = False
    (/=) REmpty _ = True
    (/=) _ REmpty = True
    (/=) (tailList1:<headList1) (tailList2:<headList2) = if (headList1 /= headList2) || (tailList1 /= tailList2)
                                                             then True
                                                             else False

-- ==, /=
-- min определение: == или /=
-- закон: a == b <=> not (a /= b)

myFoldl :: (b -> a -> b) -> b -> (ReverseList a) -> b
myFoldl f z REmpty = z
myFoldl f z (t:<h) = myFoldl f (f z h) t

myReverse :: (ReverseList a) -> (ReverseList a)
myReverse list = myFoldl (\y x -> y:<x) REmpty list

instance Semigroup (ReverseList a) where
    (<>) REmpty REmpty = REmpty
    (<>) REmpty list = list
    (<>) list REmpty = list
    (<>) list1 list2 = myFoldl (\y x -> y:<x) list1 (myReverse list2)

-- <>, sconcat, stimes
-- min определение: <>
-- <> позволяет объединить любые 2 значения типа в 1 + выполняется ассоциативность (a <> b) <> c == a <> (b <> c)

instance Monoid (ReverseList a) where
    mempty = REmpty

-- mempty, mappend, mconcat
-- min определение: mempty
-- закон: mappend x mzero === mappend mzero x === x
-- mappend = (<>)

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f list = myFoldl (\y x -> y:<(f x)) REmpty (myReverse list)

-- fmap, <$
-- min определение: fmap
-- закон: fmap id x === x

-- fmap :: (a -> b) -> f a -> f b
-- <$ :: a -> f b -> f a

instance Applicative ReverseList where
    pure list = REmpty :< list
    (<*>) REmpty REmpty = REmpty
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty
    (<*>) listF list = myFoldl (\y x -> y <> (fmap x list)) REmpty (myReverse listF)

-- pure, <*>, liftA2, *>, <*
-- min определение: pure, <*> или liftA2
-- закон: pure f <*> x === f <$> x

-- pure - оборачивание
-- <*> - последовательное применение

instance Monad ReverseList where
    (>>=) REmpty _ = REmpty
    (>>=) list f = myFoldl (\y x -> y <> (f x)) REmpty (myReverse list)

-- >>=, >>, return, fail
-- min определение: >>=
-- законы: return x >>= f === f x
--         m >>= return === m
--         (m >>= f) >>= g === m >>= (\x -> f x >>= g)

-- return = pure
-- (>>) = (*>)
-- >>= последовательно составляет 2 действия, передавая любое значение полученное 1 аргументом в качестве аргумента 2