
module Rewrite where
s1 = readRString "aba"
s2 = readRString "a"

myStringSignature :: Signature
myStringSignature = [("b",0),("w",0),("r",0)]

patatas :: [(Position, String)]
patatas = [([3,4], []), ([1,2], []),([5,6], []),([0,4], [])]

patata :: (Position, String)
patata = ([0,4], [])

reglaSuelta = Rule (readRString "wb" , readRString  "bw" )

myStringSystem = [
  Rule (readRString "wb" , readRString  "bw" ),
  Rule (readRString "rb" , readRString "br" ),
  Rule (readRString "rw" , readRString "wr" )
  ]

myRString = readRString "wrrbbwrrwwbbwrbrbww"

-- | Rule (DTerm, DTerm)
--data RewriteSystem = RewriteSystem [Rule]

--data Rule = Rule (RString, RString)
--type Substitution = [(RString,RString)]

type RewriteSystem = [Rule]

data RString = RString [String]

type Signature = [(String, Int)]
type Position = [Int]

instance Show (Rule) where show r = printRule r
printRule (Rule (RString x ,RString y)) =  printRString (RString x) ++ " --> " ++ printRString (RString y)

rewrite :: RewriteSystem -> RString -> ([(Position, RString)] -> [(Position, RString)]) -> RString
rewrite rs a strategy
                     | isEmpty m = a
                     | otherwise = rewrite rs (replace a (strategy m)) strategy
                    where m = (matchAll a rs)

isEmpty [] = True
isEmpty _ = False

oneStepRewrite :: RewriteSystem -> RString -> ([(Position, RString)] -> [(Position, RString)]) -> RString
oneStepRewrite rs a strategy = b
                where b = replace a (strategy m)
                      m = matchAll a rs

matchAll :: RString -> RewriteSystem -> [(Position, RString)]
matchAll a [] = []
matchAll b ((Rule (s, t)):xs) = (createMatch (match s b) t) ++ (matchAll b xs)

createMatch :: [(Position, Substitution)] -> a -> [(Position, a)]
createMatch [] s = []
createMatch ((p,a):xs) s = (p,s) : createMatch xs s

validRule :: Signature -> Rule -> Bool
validRule sig (Rule (s, t)) = valid sig s && valid sig t

validRewriteSystem :: Signature -> RewriteSystem -> Bool
validRewriteSystem sig [] = True
validRewriteSystem sig ((Rule x):xs) = validRule sig (Rule x) && validRewriteSystem sig xs

leftmost :: [(Position, a)] -> [(Position, a)]
leftmost l = strategy (sortLeft l)

rightmost :: [(Position, a)] -> [(Position, a)]
rightmost l = strategy (sortRight l)

strategy :: [(Position, a)] -> [(Position, a)]
strategy [] = []
strategy (x:xs) = x : strategy ((removeConflicts x xs))

removeConflicts :: (Position, a) -> [(Position, a)] -> [(Position, a)]
removeConflicts _ [] = []
removeConflicts ((x:xs:xss), a) (((y:ys:yss), b):ps) = if ((x <= y) && (xs >= y)) || ((x <= ys) && (xs >= ys))
                                                          then removeConflicts ((x:xs:xss), a) ps
                                                          else ((y:ys:yss), b) : removeConflicts ((x:xs:xss), a) ps

sortLeft :: [(Position, a)] -> [(Position, a)]
sortLeft [] = []
sortLeft (l:ls) = m : sortLeft (remove m (l:ls)) where m = (minim l ls)

sortRight :: [(Position, a)] -> [(Position, a)]
sortRight [] = []
sortRight (l:ls) = m : sortRight (remove m (l:ls)) where m = (maxim l ls)

remove :: (Position, a) -> [(Position, a)] -> [(Position, a)]
remove _ [] = []
remove ((x:xs:xss), a) (((y:ys:yss), b):ps) = if (x == y) && (xs == ys) then remove ((x:xs:xss), a) ps
                                              else ((y:ys:yss), b) : remove ((x:xs:xss), a) ps

minim :: (Position, a) -> [(Position, a)] -> (Position, a)
minim p [] = p
minim ((x:xs), a) (((y:ys), b):ps) = if x <= y then minim ((x:xs), a) ps
                                   else minim ((y:ys), b) ps

maxim :: (Position, a) -> [(Position, a)] -> (Position, a)
maxim p [] = p
maxim ((x:xs:xss), a) (((y:ys:yss), b):ps) = if xs >= ys then maxim ((x:xs:xss), a) ps
                                   else  maxim ((y:ys:yss), b) ps

--sortRight :: [(Position, a)] -> [(Position, a)]

--strategy :: [(Position, a)] -> [(Position, a)]

--
type Substitution = [(RString,RString)]
data Rule = Rule (RString, RString)

-- REWRITE CLASS --
class Rewrite a where
  --data Rule
  --type Substitution
  getVars :: a -> [a]
  valid :: Signature -> a -> Bool
  match :: a -> a -> [(Position, Substitution)]
  apply :: a -> Substitution -> a
  replace :: a -> [(Position, a)] -> a
  evaluate :: a -> a

-- RSTRING IMPLEMENTATION --
instance Rewrite RString where
  --data Rule = Rule (RString, RString)
  --type Substitution = [(RString,RString)]
  getVars rs = [] -- RString no te variables

  valid s (RString []) = True
  valid s (RString (x:xs)) = pert x s && (valid s (RString xs))

  --match pattern text = myMatch pattern text 0
  match p t = []

  apply rs sub = rs

  replace x [] = x
  replace x ((p, t):ps) = replace (myReplace x (p,t)) ps

  evaluate rs = rs
instance Show RString where show s = printRString s
printRString (RString []) = ""
printRString (RString (x:xs)) = x ++ printRString (RString xs)
--print (DTerm, DTerm) =


--instance Show (RewriteSystem) where show rs = print rs
--print rs = print rs ++ "-->" ++ rs


-- RSTRING INPUT --
readRString :: String -> RString
readRString s = RString (myReadRString s)

--toString :: RString -> String
--toString (RString s) = s

myReadRString ::String-> [String]
myReadRString [] = []
myReadRString (x:[]) = if isDigit x then [] else [x:""]
myReadRString (x:xs)
                  | isDigit x = myReadRString xs
                  | otherwise = [x:y] ++ myReadRString xs where y = firstDigits xs

-- AUXILIARS --
isDigit :: Char -> Bool
isDigit c =  c >= '0' && c <= '9'

firstDigits :: String -> String
firstDigits [] = []
firstDigits (x:xs)
                  | isDigit x = x : firstDigits xs
                  | otherwise = ""

pert :: String -> Signature -> Bool
pert x [] = False
pert x ((y,_):ys) = (x == y) || pert x ys

myMatch :: RString -> RString -> Int -> [(Position, Substitution)]
myMatch (RString []) _ _ = []
myMatch _ (RString []) _ = []
myMatch (RString pattern)  (RString (y:ys)) n = if isPrefixOf (RString pattern) (RString (y:ys))
                                                  then ([n, (n+length pattern - 1)],[]) : myMatch (RString pattern)  (RString ys) (n+1)
                                                  else myMatch (RString pattern)  (RString ys) (n+1)

isPrefixOf              :: RString -> RString -> Bool
isPrefixOf (RString []) _         =  True
isPrefixOf _  (RString [])        =  False
isPrefixOf (RString (x:xs)) (RString (y:ys)) =  x == y && isPrefixOf (RString xs) (RString ys)

myReplace :: RString -> (Position, RString) -> RString
myReplace (RString []) (pos,RString r) = RString []
myReplace (RString (x:xs)) ((ip:fp:ps),RString r)
                                                  | fp == 0 = concatRString (RString r) (RString xs)
                                                  | ip <= 0 = myReplace (RString xs) ((ip-1:fp-1:ps), RString r)
                                                  | otherwise = concatRString (RString [x])  (myReplace (RString xs) ((ip-1:fp-1:ps),RString r))

--printFirst :: RString -> String
--printFirst (RString (x:xs)) = x
concatRString :: RString -> RString -> RString
concatRString (RString []) t = t
concatRString s (RString []) = s
concatRString (RString s) (RString t) = (RString (s++t))
