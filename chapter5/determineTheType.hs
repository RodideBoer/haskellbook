{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

a = (* 9) 6
b = head [(0,"doge"),(1,"kitteh")]
c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
d = if False then True else False
e = length [1, 2, 3, 4, 5]
ff = (length [1, 2, 3, 4]) > (length "TACOCAT")

x = 5
y = x + 5
w = y * 10
z y = y * 10
f = 4 / y

xx = "Julie"
yy = "<3"
zz = "Haskell"
fxyz = xx ++ yy ++ zz
