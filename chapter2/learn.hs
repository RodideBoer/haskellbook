module Learn where
-- here is a comment

x :: Integer
x = 10 * 5 + y

myResult :: Integer
myResult = x * 5

y :: Integer
y = 10

sayHello :: String -> IO ()
sayHello z = putStrLn ("Hello, " ++ z ++ "!")

mult1 :: Integer
mult1 = x1 * y1
    where
        x1 = 5
        y1 = 6
