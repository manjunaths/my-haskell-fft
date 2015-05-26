
#summary How to do common things in Haskell for newbies.


#### Convert Int to Float. ####
```
fromIntegral 1
```

#### Get access to whatever is inside a Maybe. ####
```
import Data.Maybe

a = Just 2
fromJust a
```

#### Print an entire list/How to use map with IO ####
This works generically for a list of Actions.
```
mapM_ print [0,1,2,3,4,5,6]
```

#### How to print a string ####
```
putStrLn "Haskell is cool!"
```

#### How to print a non-string (Int, Float, Maybe etc.,) ####
```
a = 5
print a
```

#### Convert an Int, Float, etc., to a String ####
```
a = 5
putStrLn . show $ 5
```

#### Convert a string to Float, Int, etc., ####
```
a="5"
read::(String -> Int) a
```

#### Assignment inside ghci fails ####
Use let.
```
let a = 5
```

#### How to assign something inside a do block ####
Use let.
```
main = do
  -- a = 5 -- This won't work.
  let a = 5
  print a
```

#### How to generate random integers ####
```
let makeNewInt = (getStdRandom (random)):: IO Int
myRandInt <- makeNewInt
```

#### How to generate a random float between 0.0 and 1.0 ####
```
let makeNewFloat = (getStdRandom (random))::IO Float
myRandFloat <- makeNewFloat
```

#### How to generate a random Int inside a range ####
```
let makeNewInt = (getStdRandom (randomR(0,255)))::IO Int
myRandColor = makeNewInt
```

#### How to generate a random Float inside a range ####
```
let makeNewFloat = (getStdRandom (randomR(1.0,3.0)))::IO Float
myRandFloat = makeNewFloat
```