{-# LANGUAGE NoImplicitPrelude, NoStarIsType #-}

type left * right = (left, right)

data Box = Box

value :: Box * Box
value = (Box, Box)
