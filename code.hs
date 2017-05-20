-- chp15

import Data.Monoid hiding ((<>))
import Data.Semigroup


data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

-- instance Monoid a => Monoid (Optional a) where
--   -- mempty (Only a) = mempty a
--   mempty = Nada
--   mappend (Only a) (Only b) = Only $ a <> b
--   mappend (Only a) Nada = Only $ a <> mempty a
--   mappend _ (Only b) = Only $ b <> mempty b
--   mappend _ _ = Nada

-- Only (Sum 1) <> Only (Sum 2)
