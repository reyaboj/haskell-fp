import Test.QuickCheck ( quickCheck, Arbitrary(..), frequency, (.&&.) )
import Chapter15 ( Optional(Yep, Nop) )
import Data.Monoid ( Sum, Product )


main :: IO ()
main = do return ()

{-
    Tests for Chapter 15
      Semigroup operator associativity
      Monoid identity
-}
isAssociative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
isAssociative (<>) x y z =
    x <> (y <> z) == (x <> y) <> z

isSemigroup :: (Eq a, Semigroup a) => a -> a -> a -> Bool
isSemigroup = isAssociative (<>)

hasLeftIdentity :: (Eq a, Monoid a) => a -> Bool
hasLeftIdentity m = mempty <> m == m

hasRightIdentity :: (Eq a, Monoid a) => a -> Bool
hasRightIdentity m = m <> mempty == m

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = frequency [(1, return Nop), (1, Yep <$> arbitrary)]

type OptionalSum = Optional (Sum Integer)
type OptionalPro = Optional (Product Integer)
type OptionalStr = Optional String

testOptionalSemigroup :: IO ()
testOptionalSemigroup = do
    quickCheck (isSemigroup :: OptionalSum -> OptionalSum -> OptionalSum -> Bool)
    quickCheck (isSemigroup :: OptionalPro -> OptionalPro -> OptionalPro -> Bool)
    quickCheck (isSemigroup :: OptionalStr -> OptionalStr -> OptionalStr -> Bool)

testOptionalMonoid :: IO ()
testOptionalMonoid = do
    testOptionalSemigroup
    quickCheck ()
