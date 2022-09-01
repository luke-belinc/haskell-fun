data Maybe a = Nothing | Just a

fmap :: (a -> b) -> (Maybe a -> Maybe b)
fmap f Nothing = Nothing
fmap f (Just x) = Just (f x)

fmap id = id