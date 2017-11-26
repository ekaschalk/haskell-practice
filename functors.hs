-- Functors

-- # LANGUAGE FlexibleContexts #
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr

-- 16.7 Lifting Exercises

a = (+1) <$> read "[1]" :: [Int]
b = fmap (++ "lol") <$> Just ["Hi", "Hello"]
c = (* 2) <$> (\x -> x - 2)
d = (return '1' ++) . show <$> (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        iostr = show <$> ioi :: IO [Char]
        ioconcat = ("123" ++) <$> iostr :: IO [Char]
        changed = read <$> ioconcat :: IO Integer
    in (* 3) <$> changed

-- 16.10 Intermission
--- given example

functorCompose' :: (Eq (f c), Functor f) =>
  f a -> Fun a b -> Fun b c -> Bool

functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

--- Ex1

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

type IDFC = Identity Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

--- Ex2

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

--- Ex3

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

type StringToInt = Fun String Int
type StringToString = Fun String String
type TwoFC = Two Int String -> StringToString -> StringToInt -> Bool
type TwoFC' = Two Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

--- Ex4

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeFC = Three Int Int String -> StringToString -> StringToInt -> Bool
type ThreeFC' = Three Int Int Int -> IntToInt -> IntToInt -> Bool

--- Ex5

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

type Three'FC = Three' Int String -> StringToString -> StringToInt -> Bool
type Three'FC' = Three' Int Int -> IntToInt -> IntToInt -> Bool

--- Ex6-7 same as above

--- Ex8

data Trivial = Trivial
  deriving (Eq, Show)

-- instance Functor Trivial where
--   fmap f Trivial = Trivial

-- Answer is NO, Trivial has no Functor instance

-- A functor is of kind (* -> *)
-- but Trivial is of kind *


-- 16.11

data Possibly a =
    No
  | Yep a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yep a) = Yep $ f a
  fmap f No = No


data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second $ f b

-- 16.17 CHAPTER EXERCISES

-- Ex1-3

data BoolElse a =
  False' a | True' a

instance Functor BoolElse where
  fmap f (False' a) = False' $ f a
  fmap f (True' a) = True' $ f a

-- Ex4

newtype Mu f =
  InF { outF :: f (Mu f) }
  -- deriving (Eq, Show)

-- instance (Functor f) => Functor (Mu) where
-- instance Mu (Functor f) where
-- instance Functor f => Functor (Mu f) where
-- instance Functor Mu where
--   fmap func (InF a) = Inf $ func a

-- :k Mu
-- Mu :: (* -> *) -> *

-- Since Functor doesn't work on (* -> *) -> * nor * there is no Functor instance
-- I don't understand *why* Mu is (* -> *) -> *
-- I *think* it is because f takes Mu f and returns an f (that is f :: Mu f -> f)

-- Ex5

data D =
  D (Array Word Word) Int Int

-- Definitely no Functor instance since kind  D :: *

-- (Second Section starts)
-- Ex1 - Ex2 are trivial
-- Ex3

data More a b =
  --   L a b a
  -- | R b a b
    L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


-- (Third Section starts)

-- Ex1 - trivial
-- Ex2

data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a


-- Ex3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = do
    a <- arbitrary
    return $ Flip (K a)

type FlipFC = Flip K Int Int -> IntToInt -> IntToInt -> Bool

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-- Ex4

data Evil a b =
  EvilConst b

instance Functor (Evil a) where
  fmap f (EvilConst b) = EvilConst (f b)

-- Ex5

data LiftOut f a =
  LiftOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftOut f) where
  fmap f (LiftOut fa) = LiftOut $ f <$> fa

instance (Arbitrary a) => Arbitrary (LiftOut [] a) where
  arbitrary = do
    a <- arbitrary
    return $ LiftOut [a]  -- LiftOut [a, a] also works

type LiftFC = LiftOut [] Int -> IntToInt -> IntToInt -> Bool

-- Ex6

data Parappa f g a =
  Parappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (Parappa fa ga) = Parappa (f <$> fa) (f <$> ga)

instance (Arbitrary a) => Arbitrary (Parappa [] [] a) where
  arbitrary = do
    a <- arbitrary
    return $ Parappa [a] [a]  -- LiftOut [a, a] also works

type ParappaFC = Parappa [] [] Int -> IntToInt -> IntToInt -> Bool

-- Ex7

data IgnoreOne f g a b =
  IgnoreOne (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreOne fa gb) = IgnoreOne fa (f <$> gb)  -- note has to be on gb


-- Ex8

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

-- Ex9

data List a =
    Nil
  | Cons a (List a)


instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a list) = Cons (f a) (f <$> list)


-- Ex10

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)


instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g g' g'') = MoreGoats (f <$> g) (f <$> g') (f <$> g'')


-- Ex11

data TalkToMe a =
    Halt
  | Print  String a
  | Read  (String -> a)


instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print str a) = Print str (f a)
  fmap f (Read g) = Read (f <$> g)

---

main :: IO ()
main = do
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorCompose' :: IDFC)
  quickCheck (functorCompose' :: PairFC)
  quickCheck (functorCompose' :: TwoFC)
  quickCheck (functorCompose' :: TwoFC')
  quickCheck (functorCompose' :: ThreeFC)
  quickCheck (functorCompose' :: ThreeFC')
  quickCheck (functorCompose' :: Three'FC)
  quickCheck (functorCompose' :: Three'FC')
  quickCheck (functorCompose' :: FlipFC)
  quickCheck (functorCompose' :: LiftFC)
  quickCheck (functorCompose' :: ParappaFC)
