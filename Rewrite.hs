module Rewrite where

data Rule a = Rule (a,a)

type RewriteSystem a = [Rule a]

data RString = RString [String]
data RTerm = Symbol (String, Int) [RTerm]
            | Variable (String, Int)
            | Number (String, Int)
            | EmptyTerm

type Signature = [(String, Int)]
type Position = [Int]
type Substitution a = [(a,a)]

instance (Rewrite a) => Show (Rule a) where show r = printRule r
printRule :: (Rewrite a) => (Rule a)-> String
printRule (Rule (x, y)) =   (printRw x) ++ " --> " ++ (printRw y)

-- nrewrite : Given a Rewrite Systen, an object and a strategy, applies the rewrite n times
nrewrite :: (Rewrite a) => (RewriteSystem a) -> a -> ([(Position, a)] -> [(Position, a)]) -> Int -> a
nrewrite rs a strategy 0 = a
nrewrite rs a strategy n = nrewrite rs (oneStepRewrite rs a strategy) strategy (n-1)

-- rewrite : Given a Rewrite Systen, an object and a strategy, applies the rewrite until it cannot be applied more. This method might not end
rewrite :: (Rewrite a) => (RewriteSystem a)-> a -> ([(Position, a)] -> [(Position, a)]) -> a
rewrite rs a strategy
                     | isEmpty m = a
                     | otherwise = rewrite rs (replace a (strategy m)) strategy
                    where m = (matchAll a rs)

-- rewrite : Given a Rewrite Systen, an object and a strategy, applies the rewrite one time
oneStepRewrite :: (Rewrite a) => (RewriteSystem a)-> a -> ([(Position, a)] -> [(Position, a)]) -> a
oneStepRewrite rs a strategy = b
                where b = replace a (strategy m)
                      m = matchAll a rs

-- matchAll : Given an object and a Rewrite System, it returns the positions where the substitutions can be applied
matchAll :: (Rewrite a) => a -> (RewriteSystem a)-> [(Position, a)]
matchAll b [] = []
matchAll b ((Rule (s, t)):xs) = (createMatch (match s b) t) ++ (matchAll b xs)

-- createMatch : Given a list of pairs position-substitution and an object a, it returns a list of pairs of those positions and the object
createMatch :: (Rewrite a) => [(Position, (Substitution a))] -> a -> [(Position, a)]
createMatch [] s = []
createMatch ((p,a):xs) s = (p,s) : createMatch xs s

-- validRule : Given a signature and a rule, returns if it's a valid one
validRule :: (Rewrite a) => Signature -> (Rule a) -> Bool
validRule sig (Rule (s, t)) = valid sig s && valid sig t

-- validRewriteSystem : Given a signature and a rewrite system, returns if it's a valid one
validRewriteSystem :: (Rewrite a) => Signature -> (RewriteSystem a) -> Bool
validRewriteSystem sig [] = True
validRewriteSystem sig ((Rule x):xs) = validRule sig (Rule x) && validRewriteSystem sig xs

-- leftmost : Given a list of positions, returns the non overlapping positions giving priority to the leftmost ones
leftmost :: [(Position, a)] -> [(Position, a)]
leftmost l = strategy (sortLeft l)

-- rightmost : Given a list of positions, returns the non overlapping positions giving priority to the rightmost ones
rightmost :: [(Position, a)] -> [(Position, a)]
rightmost l = strategy (sortRight l)

-- strategy : Given a list of positions, returns a list of positions without any overlapping conflicts
strategy :: [(Position, a)] -> [(Position, a)]
strategy [] = []
strategy (x:xs) = x : strategy ((removeConflicts x xs))

-- REWRITE CLASS --
class Rewrite a where
  getVars :: a -> [a]
  valid :: Signature -> a -> Bool
  match :: a -> a -> [(Position, (Substitution a))]
  apply :: a -> (Substitution a) -> a
  replace :: a -> [(Position, a)] -> a
  evaluate :: a -> a

  printRw :: a -> String

-- RSTRING --
instance Rewrite RString where
  getVars rs = [] -- RString has no variables

  valid s (RString []) = True
  valid s (RString (x:xs)) = pert x s && (valid s (RString xs))

  match pattern text = myMatch pattern text 0

  apply rs sub = rs -- N/A

  replace x [] = x
  replace x ((p, t):ps) = replace (myReplace x (p,t)) ps

  evaluate rs = rs -- N/A

  printRw (RString []) = ""
  printRw (RString (x:xs)) = x ++ printRw (RString xs)

-- RSTRING IO --
instance Show RString where show s = printRw s

readRString :: String -> RString
readRString s = RString (myReadRString s)

readRStringSystem :: [(String,String)] -> [Rule RString]
readRStringSystem [] = []
readRStringSystem ((x,y):xs) = (Rule (readRString x, readRString y)) : readRStringSystem xs

myReadRString ::String-> [String]
myReadRString [] = []
myReadRString (x:[]) = if isDigit x then [] else [x:""]
myReadRString (x:xs)
                  | isDigit x = myReadRString xs
                  | otherwise = [x:y] ++ myReadRString xs where y = firstDigits xs

-- RSTRING HELPERS --
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

-- Given two RStrings it returns the positions where the two match, represented by a list of two ints, the start and the end positions in the substring
myMatch :: RString -> RString -> Int -> [(Position, (Substitution a))]
myMatch (RString []) _ _ = []
myMatch _ (RString []) _ = []
myMatch (RString pattern)  (RString (y:ys)) n = if isPrefixOf (RString pattern) (RString (y:ys))
                                                  then ([n, (n+length pattern - 1)],[]) : myMatch (RString pattern)  (RString ys) (n+1)
                                                  else myMatch (RString pattern)  (RString ys) (n+1)

isPrefixOf :: RString -> RString -> Bool
isPrefixOf (RString []) _         =  True
isPrefixOf _  (RString [])        =  False
isPrefixOf (RString (x:xs)) (RString (y:ys)) =  x == y && isPrefixOf (RString xs) (RString ys)

-- Replaces a subRString with the given RString in a certain position
myReplace :: RString -> (Position, RString) -> RString
myReplace (RString []) (pos,RString r) = RString []
myReplace (RString (x:xs)) ((ip:fp:ps),RString r)
                                                  | fp == 0 = concatRString (RString r) (RString xs)
                                                  | ip <= 0 = myReplace (RString xs) ((ip-1:fp-1:ps), RString r)
                                                  | otherwise = concatRString (RString [x])  (myReplace (RString xs) ((ip-1:fp-1:ps),RString r))

concatRString :: RString -> RString -> RString
concatRString (RString []) t = t
concatRString s (RString []) = s
concatRString (RString s) (RString t) = (RString (s++t))

isEmpty [] = True
isEmpty _ = False

-- removeConflicts :  Given a position and a list of positions, it removes the positions in the list that overlap with the first position
removeConflicts :: (Position, a) -> [(Position, a)] -> [(Position, a)]
removeConflicts _ [] = []
removeConflicts ((x:xs:xss), a) (((y:ys:yss), b):ps) = if ((x <= y) && (xs >= y)) || ((x <= ys) && (xs >= ys))
                                                          then removeConflicts ((x:xs:xss), a) ps
                                                          else ((y:ys:yss), b) : removeConflicts ((x:xs:xss), a) ps

-- sortLeft: Sorts a list of positions, giving priority to the leftmost ones
sortLeft :: [(Position, a)] -> [(Position, a)]
sortLeft [] = []
sortLeft (l:ls) = m : sortLeft (remove m (l:ls)) where m = (minim l ls)

-- sortRight: Sorts a list of positions, giving priority to the rightmost ones
sortRight :: [(Position, a)] -> [(Position, a)]
sortRight [] = []
sortRight (l:ls) = m : sortRight (remove m (l:ls)) where m = (maxim l ls)

-- remove: removes a position from a given list
remove :: (Position, a) -> [(Position, a)] -> [(Position, a)]
remove _ [] = []
remove ((x:xs:xss), a) (((y:ys:yss), b):ps) = if (x == y) && (xs == ys) then remove ((x:xs:xss), a) ps
                                              else ((y:ys:yss), b) : remove ((x:xs:xss), a) ps

-- minim: returns the minimum position of a list
minim :: (Position, a) -> [(Position, a)] -> (Position, a)
minim p [] = p
minim ((x:xs), a) (((y:ys), b):ps) = if x <= y then minim ((x:xs), a) ps
                                   else minim ((y:ys), b) ps

-- maxim: returns the maximum position of a list
maxim :: (Position, a) -> [(Position, a)] -> (Position, a)
maxim p [] = p
maxim ((x:xs:xss), a) (((y:ys:yss), b):ps) = if xs >= ys then maxim ((x:xs:xss), a) ps
                                   else  maxim ((y:ys:yss), b) ps

-- RTERM --
arithmeticSymbols :: Signature
arithmeticSymbols = [("+",0),("*",0)]

instance Rewrite RTerm where
  getVars EmptyTerm = []
  getVars (Number (s, _)) = []
  getVars (Symbol (s, x) t) = foldr (++) [] (map getVars t)
  getVars (Variable (s, x)) = [Variable (s, x)]

  valid sig EmptyTerm = True
  valid sig (Number (s, _)) = True
  valid sig (Symbol (s, x) t) =  foldr (&&) (pert s arithmeticSymbols || pertRTerm (s, x) sig) (map (valid sig) t)
  valid sig (Variable (s, x)) = True

  match a b = [] -- stub

  apply rs sub = rs -- stub

  replace x y = x -- stub

  evaluate rs = rs -- stub

  printRw EmptyTerm = ""
  printRw (Symbol (s, 0) t) = s ++ (printAll t)
  printRw (Symbol (s, x) t) = s ++ "( " ++ (printAll t) ++ " )"
  printRw (Variable (s, _)) = s
  printRw (Number (s, _)) = s

-- RTERM IO --
instance Show RTerm where show s = printRw s

printAll :: [RTerm] -> String
printAll [] = ""
printAll (t:[]) = (printRw t)
printAll (t:ts) = (printRw t) ++ " , " ++ (printAll ts)

readRTermSystem :: [([(String,Int)],[(String,Int)])] -> (RewriteSystem RTerm)
readRTermSystem [] = []
readRTermSystem ((s,t):xs) = (Rule ((readRTree s), (readRTree t))) : (readRTermSystem xs)

readRTree :: [(String, Int)] -> RTerm
readRTree [] = EmptyTerm
readRTree ((s,-1):xs) = (Variable (s,-1))
readRTree ((s,-2):xs) = (Number (s,-2))
readRTree ((s,n):xs) = (Symbol (s,n) (readAllSons 0 n xs))

readAllSons :: Int -> Int -> [(String, Int)] -> [RTerm]
readAllSons _ 0 _ = []
readAllSons _ _ [] = []
readAllSons 0 n ((s,t):xs) = if t < 0 then (readRTree ((s,t):xs)) : (readAllSons 0 (n-1) xs)
                                      else (readRTree ((s,t):xs)) : (readAllSons t (n-1) xs)
readAllSons x n ((s,t):xs) = if t < 0 then readAllSons (x-1) n xs
                                      else readAllSons (x-1+t) n xs

-- RTERM AUX --
pertRTerm :: (String, Int) -> Signature -> Bool
pertRTerm x [] = False
pertRTerm (s, t) ((x,y):ys) = ((s == x) && (t == y))|| pertRTerm (s, t) ys



-- TEST DATA --
s1 = readRString "aba"
s2 = readRString "a"

myStringSignature :: Signature
myStringSignature = [("b",0),("w",0),("r",0)]

pairs :: [(Position, String)]
pairs = [([3,4], []), ([1,2], []),([5,6], []),([0,4], [])]

pair :: (Position, String)
pair = ([0,4], [])

randomRule = Rule (readRString "wb" , readRString  "bw" )

myStringSystem = readRStringSystem [
  ("wb" , "bw" ),
  ("rb" , "br" ),
  ("rw" , "wr" )
  ]

myRString = readRString "wrrbbwrrwwbbwrbrbww"

myTermSignature :: Signature
myTermSignature = [("len",1),("con",2),("emp",0),("list1",0)]

trs1 = readRTermSystem [
  ([("len",1),("emp",0)] , [("0",-2)]),
  ([("len",1),("con",2),("x",-1),("xs",-1)] , [("+",2),("1",-2),("len",1),("xs",-1)]),
  ([("list1",0)] , [("con",2),("1",-2),("emp",0)])
  ]

term1 = readRTree [("+",2),("len",1),("con",2),("x",-2),("con",2),("emp",0),("emp",0),("len",1),("emp",0)]
term2 = readRTree [("+",2),("len",1),("con",2),("a",0),("con",2),("a",0),("emp",0),("len",1),("list1",0)]