-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- the Maybe type is already declared as an instance of the Monad class
-- in the standard prelude, so we don't actually need to define it here.
-- just remember that it looks something like this:
-- instance Monad Maybe where
--    Nothing  >>= f = Nothing
--    (Just x) >>= f = f x
--    return         = Just

-- we can use do-notation to build complicated sequences
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = do m <- mother s
                           father m

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do f  <- father s
                                  gm <- mother f
				  mother gm

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m  <- mother s
                                  gf <- father m
				  father gf

-- this builds our sheep family tree
breedSheep :: Sheep
breedSheep = let adam   = Sheep "Adam" Nothing Nothing
                 eve    = Sheep "Eve" Nothing Nothing
		 uranus = Sheep "Uranus" Nothing Nothing
		 gaea   = Sheep "Gaea" Nothing Nothing
		 kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                 holly  = Sheep "Holly" (Just eve) (Just adam)
	         roger  = Sheep "Roger" (Just eve) (Just kronos)
	         molly  = Sheep "Molly" (Just holly) (Just roger)
	     in Sheep "Dolly" (Just molly) Nothing

-- print Dolly's maternal grandfather
main :: IO ()
main = let dolly = breedSheep
       in do print (maternalGrandfather dolly)


-- Exercise 1: Do notation

-- Rewrite the 
--     1.1) maternalGrandfather
--     1.2) fathersMaternalGrandmother, 
--     1.3) mothersPaternalGrandfather 
-- functions in above example 
-- using the monadic operators return and >>=, 
-- without using any do-notation syntactic sugar. 

-- 1.1) orig:
-- maternalGrandfather :: Sheep -> Maybe Sheep
-- maternalGrandfather s = do m <- mother s
--                            father m
maternalGrandfather2 :: Sheep -> Maybe Sheep
maternalGrandfather2 s = (return s) >>= mother >>= father -- or mother s >>= father

-- 1.2) orig:
-- fathersMaternalGrandmother :: Sheep -> Maybe Sheep
-- fathersMaternalGrandmother s = do f  <- father s
--                                   gm <- mother f
-- 				  mother gm
fathersMaternalGrandmother2 :: Sheep -> Maybe Sheep
fathersMaternalGrandmother2 s = (return s) >>= father >>= mother >>= mother -- or father 2 >>= mother ...

-- 1.3) orig:
-- mothersPaternalGrandfather :: Sheep -> Maybe Sheep
-- mothersPaternalGrandfather s = do m  <- mother s
--                                   gf <- father m
-- 				  father gf
mothersPaternalGrandfather2 :: Sheep -> Maybe Sheep
mothersPaternalGrandfather2 s = (return s) >>= mother >>= father >>= father -- or mother s >>= father ...

-- Exercise 2: Combining monadic values

-- Write functions parent and grandparent with signature Sheep -> Maybe Sheep. 
-- They should return one sheep selected from all sheep matching the description, 
-- or Nothing if there is no such sheep. Hint: the mplus operator is useful here. 
parent :: Sheep -> Maybe Sheep
parent s = (mother s) `mplus` (father s)

grandParent :: Sheep -> Maybe Sheep
grandParent s = (mother s >>= parent) `mplus` (father s >>= parent) 

-- Exercise 3: Using the List monad

-- Write functions parent and grandparent with signature Sheep -> [Sheep]. 
-- They should return all sheep matching the description, or the empty list if there is no such sheep. 
-- Hint: the mplus operator in the List monad is useful here. 
-- Also the maybeToList function in the Maybe module can be used to convert a value from the Maybe monad to the List monad.

parentL :: Sheep -> [Sheep]
parentL s = (maybeToList (mother s)) `mplus` (maybeToList (father s))

grandParentL :: Sheep -> [Sheep]
grandParentL s = do p <- parent s
                   parent p

-- Exercise 4: Using the Monad class constraint

-- Write functions parent and grandparent with signature (MonadPlus m) => Sheep -> m Sheep.

maybeToMonad :: (MonadPlus m) => Maybe a -> m a
maybeToMonad Nothing = mzero
maybeToMonad Just x = return x

parentM :: (MonadPlus m) => Sheep -> m Sheep
parentM s = (maybeToMonad (mother s)) `mplus` (maybeToMonad (father s))

grandParentM :: (MonadPlus m) => Sheep -> m Sheep
grandParentM s =  (maybeToMonad (mother s) >>= parentM) `mplus`
                  (maybeToMonad (father s) >>= parentM)
