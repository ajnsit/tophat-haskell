module Main

import Task
import Task.Universe
import Task.Event

%default total


-- Tests -----------------------------------------------------------------------
--
-- NOTE: Tasks ending in `'` need user input
--


-- Helpers --

edit : Int -> Task (BasicTy IntTy)
edit x = Edit (Just x)

ask : Task (BasicTy IntTy)
ask = Edit Nothing


-- Basic --

fourtytwo : Task (BasicTy IntTy)
fourtytwo = Done 42

hello : Task (BasicTy StringTy)
hello = Done "Hello"

inc : Int -> Task (BasicTy IntTy)
inc x = Done (x + 1)

add : Int -> Int -> Task (BasicTy IntTy)
add x y = Done (x + y)

append : String -> String -> Task (BasicTy StringTy)
append x y = Done (x ++ y)


-- Steps --

pureStep : Task (BasicTy IntTy)
pureStep = do
    x <- fourtytwo
    inc x

pureStep' : Task (BasicTy IntTy)
pureStep' =
    fourtytwo >>? \x =>
    inc x

oneStep : Task (BasicTy IntTy)
oneStep = do
    x <- edit 0
    inc x

oneStep' : Task (BasicTy IntTy)
oneStep' =
    edit 0 >>? \x =>
    inc x

-- oneStep'' : Task (BasicTy IntTy)
-- oneStep'' =
--     edit 0 >>*
--         [ \x => ( True, inc x ) ]

twoSteps : Task (BasicTy IntTy)
twoSteps = do
    x <- edit 1
    y <- edit 2
    add x y

twoSteps' : Task (BasicTy IntTy)
twoSteps' =
    edit 1 >>? \x =>
    edit 2 >>? \y =>
    add x y


-- Parallel --

parallel : Task (PairTy (BasicTy IntTy) (BasicTy StringTy))
parallel = Edit Nothing |*| Edit (Just "Hello")

parallelStep : Task (BasicTy StringTy)
parallelStep = do
    ( n, m ) <- parallel
    Done (unwords $ replicate (cast n) m)

parallelStep' : Task (BasicTy StringTy)
parallelStep' =
    parallel >>? \( n, m ) =>
    Done (unwords $ replicate (cast n) m)


-- Normalisation --
--
-- FIXME: should these automatically simplify?

stablise : Int -> Task (BasicTy IntTy)
stablise x = do
    edit x >>? \y =>
    Done y

pair : Task (PairTy (BasicTy IntTy) (BasicTy IntTy))
pair = Done 3 |*| Done 8

inner : Task (BasicTy IntTy)
inner = do
    ( x, y ) <- pair
    add x y

inner' : Task (BasicTy IntTy)
inner' =
    pair >>? \( x, y ) =>
    add x y


-- Shared Data --

update : Task UnitTy
update =
    Get >>= \x =>
    edit x >>? \y =>
    Put y

--FIXME: help!!!
update2 : Task UnitTy
update2 = do
    Get >>= \x =>
    edit (x+1) >>? \y =>
    Put y >>= \() =>
    Get >>= \u =>
    edit (u+2) >>? \v =>
    Put v

watch : Show (typeOf a) => Task a -> Task (PairTy a StateTy)
watch t = t |*| Watch

parallelWatch : Task (PairTy StateTy StateTy)
parallelWatch = watch Watch


-- Choices --

choice : Task (BasicTy IntTy)
choice = edit 1 |+| edit 2

choice3 : Task (BasicTy IntTy)
choice3 = choice |+| edit 3

choice1 : Task (BasicTy IntTy)
choice1 = edit 2 |+| Fail

auto : Task (BasicTy StringTy)
auto = do
    x <- edit 0
    if x >= 10 then Done "large" else Fail

actions : Task (BasicTy StringTy)
actions =
    edit 0 >>? \x =>
    (Done "first" |+| Done "second first" |+| Done "second second")

guards : Task (BasicTy StringTy)
guards =
    edit 0 >>? \x =>
    ((if x >= 10 then Done "large" else Fail) |+| (if x >= 100 then Done "very large" else Fail))

partial -- due to `mod` on `0`
branch : Task (BasicTy StringTy)
branch =
    edit 1 >>? \x =>
    if x `mod` 3 == 0 then
        Done "multiple of 3"
    else if x `mod` 5 == 0 then
        Done "multiple of 5"
    else
        Fail


-- Empty edit --

test : Task (BasicTy IntTy)
test = do
    ( x, y ) <- ask |*| ask
    pure (x + y)


-- Running ---------------------------------------------------------------------

%default covering

get : IO Event
get = do
    putStr "> "
    input <- getLine
    case Event.parse (words input) of
        Right event => do
            pure event
        Left msg => do
            putStrLn msg
            get

loop : Show (typeOf a) => Task a -> State -> IO ()
loop task state = do
    putStrLn $ ui task state
    putStrLn $ "Possibilities: " ++ show (options task state)
    event <- get
    let ( nextTask, nextState ) = handle task event state
    loop nextTask nextState

run : Show (typeOf a) => Task a -> IO ()
run t = uncurry loop $ init t (state 0)

main : IO ()
main = run test
