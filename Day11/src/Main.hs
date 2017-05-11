{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Main where

import Debug.Trace
import Data.Word
import Data.Bits
import Data.Sequence ((><), viewl, Seq, ViewL(..))
import qualified Data.Sequence as Sq
import Data.Set (Set, union, (\\))
import qualified Data.Set as S
import Data.Traversable (for)
import Control.Monad.Writer
import Data.Array.IArray
import Data.List (intercalate)
import Data.Foldable (foldl', all, find, toList)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

main :: IO ()
main = do
  [lim] <- getArgs
  --print $ traverseBreadthFirst (read lim) requiredState realInitialState
  print $ traverseBreadthFirstOpt (read lim) requiredStateOpt (fromState realInitialState)

-- | So that all floors can fit into 1 64-bit word (since 1 floor takes 14 bits at most)
type Component = Word64

generatorsShift :: Int
generatorsShift = 7
floorShift :: Int
floorShift = 14

-- | Type of generator/chip
data Type = Thulium
          | Plutonium
          | Strontium
          | Promethium
          | Ruthenium
          | Elerium
          | Dilithium
          deriving (Show, Eq, Ord, Enum)

-- | All types as list
allTypes :: [Type]
allTypes = [Thulium .. Dilithium]

-- | Chip definition
data Chip = Chip Type
          deriving (Eq, Ord)

instance Show Chip where
  show (Chip Thulium)    = "CT"
  show (Chip Plutonium)  = "CP"
  show (Chip Strontium)  = "CS"
  show (Chip Promethium) = "Cp"
  show (Chip Ruthenium)  = "CR"
  show (Chip Elerium)    = "CE"
  show (Chip Dilithium)  = "CD"

-- | List of all chips
allChipsList :: [Chip]
allChipsList = Chip <$> allTypes

-- | Position-indpendent set of chips
allChips :: Set Chip
allChips = S.fromList allChipsList

-- | Converts chip to its bit representation
chipBit :: Chip -> Component 
chipBit (Chip t) = bit (fromEnum t)

-- | Returns Chip if its present in given bit representation
maybeChip :: Component -> Type -> Maybe Chip
maybeChip b t = if testBit b (fromEnum t) then Just (Chip t) else Nothing

-- | Mask for all chips
chipsMask :: Component 
chipsMask = foldl' (.|.) 0 (chipBit <$> allChipsList)

-- | Generator definition
data Generator = Generator Type
               deriving (Eq, Ord)

instance Show Generator where
  show (Generator Thulium)    = "GT"
  show (Generator Plutonium)  = "GP"
  show (Generator Strontium)  = "GS"
  show (Generator Promethium) = "Gp"
  show (Generator Ruthenium)  = "GR"
  show (Generator Elerium)    = "GE"
  show (Generator Dilithium)  = "GD"

-- | List of all generators
allGeneratorsList :: [Generator]
allGeneratorsList = Generator <$> allTypes 

-- | Position-independents set of all generators
allGenerators :: Set Generator
allGenerators = S.fromList allGeneratorsList

-- | Converts generator to its bit representation.
generatorBit :: Generator -> Component
generatorBit (Generator t) = bit (generatorsShift + fromEnum t)

-- | Returns true if bit representing generators is present in bits
maybeGenerator :: Component -> Type -> Maybe Generator
maybeGenerator b t = if testBit b (generatorsShift + fromEnum t) then Just (Generator t) else Nothing

-- | Bit mask for all generators
generatorMask :: Component 
generatorMask = foldl' (.|.) 0 (generatorBit <$> allGeneratorsList)

-- | Current state of the floor.
data Floor = Floor { chips :: Set Chip 
                   , generators :: Set Generator }
           deriving (Show, Eq, Ord)

-- | Optimized version of the floor - stores all chips and generators as bits in Int
type FloorOpt = Component 

-- | Returns normal representation of the floor from optimized one.
toFloor :: FloorOpt -> Floor
toFloor b = let cps = S.fromList $ catMaybes $ maybeChip b <$> allTypes
                gens = S.fromList $ catMaybes $ maybeGenerator b <$> allTypes
            in Floor cps gens

-- | Converts unoptimized floor to optimized
fromFloor :: Floor -> FloorOpt
fromFloor (Floor cps gens) = foldl' (.|.) 0 $ (chipBit <$> S.toList cps) ++ (generatorBit <$> S.toList gens)

floorMask :: Component
floorMask = chipsMask .|. generatorMask

-- | Returns true if chip is safe on the floor
chipSafe :: FloorOpt -> Component -> Bool
chipSafe b i = (b .&. generatorMask == 0) || (b .&. (i `shiftL` generatorsShift) /= 0)

-- | Enumeration for possible floors.
data CurrentFloor = FFirst
                  | FSecond
                  | FThird
                  | FFourth
                  deriving (Show, Eq, Ord, Ix, Enum)

--instance Show (Array CurrentFloor Floor) where
--  show arr = intercalate "\n" (show <$> elems arr)

-- | Current state of the generators
data State = State CurrentFloor (Array CurrentFloor Floor)
           deriving (Show, Eq, Ord)

-- | Optimized state
-- data StateOpt = StateOpt CurrentFloor (Array CurrentFloor FloorOpt)
--               deriving (Show, Eq, Ord)
--type StateOpt = Word

-- Since 64 bit can fit 14*4 = 56 bits worth of data + 3 bits for floor index we can store whole state in one Word64
type StateOpt = Component

-- | 3 == 111 in binary - enough to store indices from 0 to 3
currentFloorMask :: Component
currentFloorMask = 3 `rotateR` 2 

stateFloor :: StateOpt -> Int -> FloorOpt
stateFloor s i = (s .&. (floorMask `shiftL` (i * floorShift))) `shiftR` (i * floorShift)

setFloor :: StateOpt -> Int -> FloorOpt -> StateOpt
setFloor s i f = let s' = s `xor` (stateFloor s i `shiftL` (i * floorShift)) -- clean floor
                 in s' .|. (f `shiftL` (i * floorShift))

setFloorIndex :: StateOpt -> Int -> StateOpt
setFloorIndex s i = let i' = fromIntegral i `rotateR` 2
                    in (s .&. complement currentFloorMask) .|. i'

floors :: StateOpt -> [FloorOpt]
floors w = stateFloor w <$> [0..3] 

currentFloorIndex :: StateOpt -> Int
currentFloorIndex s = fromIntegral $ (s .&. currentFloorMask) `rotateL` 2

currentFloor :: StateOpt -> FloorOpt
currentFloor s = stateFloor s (currentFloorIndex s) 

-- | Converts state into opt state.
fromState :: State -> StateOpt
fromState (State e fls) = let curFlr = (fromIntegral $ fromEnum e :: Component) `rotateR` 2
                              fls' = zip (fromFloor <$> toList fls) [0..3]
                              fls'' = (\(f, s) -> f `shiftL` (s * floorShift)) <$> fls'
                              fls''' = foldl' (.|.) 0 fls''
                          in fls''' .|. curFlr

-- | Converts from state opt into state.
toState :: StateOpt -> State
toState s = State (toEnum $ currentFloorIndex s) (listArray (FFirst, FFourth) $ toFloor <$> floors s)

-- | Getter for neighbours 
neighbourFloors :: CurrentFloor -> [CurrentFloor] 
neighbourFloors FFirst  = [FSecond]
neighbourFloors FSecond = [FThird, FFirst]
neighbourFloors FThird  = [FFourth, FSecond]
neighbourFloors FFourth = [FThird]

-- | Returns true if floor contains generator
hasGenerator :: Floor -> Generator -> Bool
hasGenerator (Floor _ gens) gen = gen `elem` gens

-- | Returns generator required to power the chip
requiredGenerator :: Chip -> Generator
requiredGenerator (Chip t) = Generator t

-- | Quick check if floor is valid
floorValidOpt :: FloorOpt -> Bool
floorValidOpt b = (gns == 0) || (cps .&. gns' == cps)
  where cps = b .&. chipsMask
        gns = b .&. generatorMask
        gns' = gns `shiftR` generatorsShift

-- | Returns true if current floor configuration is valid
floorValid :: Floor -> Bool
floorValid fl@(Floor cs gens) = null gens || all (hasGenerator fl . requiredGenerator) cs

-- | Returns a list of all possible payloads to be taken from this floor
possiblePayload :: Floor -> [([Chip], [Generator])]
possiblePayload (Floor cs gens) = let onechip = (\c -> ([c], [])) <$> S.toList cs
                                      onegen = (\g -> ([], [g])) <$> S.toList gens
                                      twochip = (\tc -> (tc, [])) <$> pairs (S.toList cs)
                                      twogen = (\tg -> ([], tg)) <$> pairs (S.toList gens)
                                      chipgen = take 1 $ (\(c,g) -> ([c], [g])) <$> combine (S.toList cs) (S.toList gens)
                                  in onechip ++ onegen ++ twochip ++ twogen ++ chipgen

-- | Generate all possible payloads from this floor
possiblePayloadOpt :: FloorOpt -> [Component] 
possiblePayloadOpt flr = toWord <$> possiblePayload (toFloor flr)
  where toWord (cps, gens) = foldl' (.|.) 0 $ (chipBit <$> cps) ++ (generatorBit <$> gens)

-- | Takes chips and generators from floor and checks if it's valid
addToFloorOpt :: FloorOpt -> Component -> Maybe FloorOpt
addToFloorOpt flr b = let f = flr .|. b
                      in if floorValidOpt f then Just f else Nothing

-- | Adds chips and generators to floor and checks if it is valid
takeFromFloorOpt :: FloorOpt -> Component -> Maybe FloorOpt
takeFromFloorOpt flr b = let f = flr `xor` b
                         in if floorValidOpt f then Just f else Nothing

-- | Generates all possible states from given one
possibleStatesOpt :: StateOpt -> [StateOpt]
possibleStatesOpt s = catMaybes states
  where curFloor = currentFloor s 
        curFloorIndex = currentFloorIndex s
        neighbours = fromEnum <$> neighbourFloors (toEnum curFloorIndex)
        payloads = possiblePayloadOpt curFloor
        states = maybeState <$> combine neighbours payloads

        maybeState :: (Int, Component) -> Maybe StateOpt
        maybeState (cur', payload) = let curFloor' = stateFloor s cur' in
                                       do fl <- takeFromFloorOpt curFloor payload
                                          fl' <- addToFloorOpt curFloor' payload
                                          return $ let s' = setFloor s cur' fl'
                                                       s'' = setFloor s' curFloorIndex fl
                                                   in setFloorIndex s'' cur'
                                            
traverseBreadthFirstOpt :: Int -> (StateOpt -> Bool) -> StateOpt -> Maybe (Int, StateOpt)
traverseBreadthFirstOpt limit pr state = if pr state
                                         then Just (0, state)
                                         else go (0, state) Sq.empty (S.singleton state)
  where go :: (Int, StateOpt) -> Seq (Int, StateOpt) -> Set StateOpt -> Maybe (Int, StateOpt)
        go (n, st) acc seen | pr st = Just (n, st)
                            | n >= limit = Nothing
                            | otherwise = let (next, seen') = generateSubstates n st seen
                                              acc' = acc >< next
                                          in case find (pr . snd) next of
                                               res@(Just _) -> res
                                               Nothing -> case viewl acc' of
                                                            (a :< acc'') -> go a acc'' seen'
                                                            _            -> Nothing
        generateSubstates :: Int -> StateOpt -> Set StateOpt -> (Seq (Int, StateOpt), Set StateOpt)
        generateSubstates n st seen = let cands = filter (not . flip S.member seen) $ possibleStatesOpt st
                                          cands' = Sq.fromList cands
                                      in (nextInd n <$> cands', S.union seen $ S.fromList cands)
        nextInd :: Int -> StateOpt -> (Int, StateOpt)
        nextInd n s = (n + 1, s)

initialStateOpt :: StateOpt
initialStateOpt = fromState initialState

firstThreeFloorsMask :: Component
firstThreeFloorsMask = floorMask .|. (floorMask `shiftL` floorShift) .|. (floorMask `shiftL` (floorShift * 2))

requiredStateOpt :: StateOpt -> Bool
requiredStateOpt s = s .&. firstThreeFloorsMask == 0
--                                    let fourth = (fls ! FFourth)
--                                        mask = (chipsMask .|. generatorMask)
--                                    in fourth .&. mask == mask


  








  

-- | Generates list of all pairs possible in list
pairs :: [a] -> [[a]]
pairs []     = []
pairs [_]    = []
pairs (x:xs) = ((:[x]) <$> xs) ++ pairs xs

-- | Generated cross product of two lists
combine :: [a] -> [b] -> [(a, b)]
combine [] _  = []
combine _  [] = []
combine (x:xs) ys = ((\b -> (x, b)) <$> ys) ++ combine xs ys

-- | Takes chips and generators from floor and checks if it's valid
takeFromFloor :: Floor -> ([Chip], [Generator]) -> Maybe Floor
takeFromFloor (Floor cs gs) (cs', gs') = let f = Floor (cs \\ S.fromList cs') (gs \\ S.fromList gs')
                                         in if floorValid f then Just f else Nothing

-- | Adds chips and generators to floor and checks if it is valid
addToFloor :: Floor -> ([Chip], [Generator]) -> Maybe Floor
addToFloor (Floor cs gs) (cs', gs') = let f = Floor (cs `union` S.fromList cs') (gs `union` S.fromList gs')
                                      in if floorValid f then Just f else Nothing

-- | Returns a list of possible states from this one.
possibleStates :: State -> [State]
possibleStates (State cur flrs) = catMaybes states
  where curFloor = flrs ! cur
        neighbours = neighbourFloors cur
        payloads = possiblePayload curFloor
        states = toState <$> combine neighbours payloads

        toState :: (CurrentFloor, ([Chip], [Generator])) -> Maybe State
        toState (cur', payload) = let curFloor' = flrs ! cur' in
                                    do fl <- takeFromFloor curFloor payload
                                       fl' <- addToFloor curFloor' payload
                                       return $ State cur' (flrs // [(cur, fl), (cur', fl')])

-- | Returns true if floor is empty.
floorEmpty :: Floor -> Bool
floorEmpty (Floor cps gens) = S.null cps && S.null gens

-- | Returns true if floor has all generators and chips
floorFull :: Floor -> Bool 
floorFull (Floor cs gs) = cs == allChips && gs == allGenerators

-- | Predicate matching required state
requiredState :: State -> Bool
requiredState (State _ flrs) = floorEmpty (flrs ! FFirst) &&
                               floorEmpty (flrs ! FThird) &&
                               floorEmpty (flrs ! FSecond)

-- | Convinience constructor
mkFloor :: [Chip] -> [Generator] -> Floor
mkFloor cps gens = Floor (S.fromList cps) (S.fromList gens)

-- | Initial state
initialState :: State
initialState = State FFirst (listArray (FFirst, FFourth) [mkFloor [Chip Thulium] [Generator Thulium, Generator Plutonium, Generator Strontium],
                                                           mkFloor [Chip Plutonium, Chip Strontium] [],
                                                           mkFloor [Chip Promethium, Chip Ruthenium] [Generator Promethium, Generator Ruthenium],
                                                           mkFloor [] []])

realInitialState :: State
realInitialState = State FFirst (listArray (FFirst, FFourth) [mkFloor [Chip Thulium, Chip Elerium, Chip Dilithium] [Generator Thulium, Generator Plutonium, Generator Strontium, Generator Elerium, Generator Dilithium],
                                                           mkFloor [Chip Plutonium, Chip Strontium] [],
                                                           mkFloor [Chip Promethium, Chip Ruthenium] [Generator Promethium, Generator Ruthenium],
                                                           mkFloor [] []])

testingState :: State
testingState = State FFirst (listArray (FFirst, FFourth) [mkFloor [Chip Thulium, Chip Plutonium] [],
                                                           mkFloor [] [Generator Thulium],
                                                           mkFloor [] [Generator Plutonium],
                                                           mkFloor [] []])

idealTestingState :: State 
idealTestingState = State FFirst (listArray (FFirst, FFourth) [mkFloor [] [],
                                                               mkFloor [] [],
                                                               mkFloor [] [],
                                                               mkFloor [Chip Thulium, Chip Plutonium] [Generator Plutonium, Generator Thulium]])
idealState :: State
idealState = State FFourth (listArray (FFirst, FFourth) [mkFloor [] [],
                                                          mkFloor [] [],
                                                          mkFloor [] [],
                                                          mkFloor
                                                          [Chip Plutonium, Chip Strontium, Chip Thulium, Chip Promethium, Chip Ruthenium]
                                                          [Generator Promethium, Generator Ruthenium, Generator Thulium, Generator Plutonium, Generator Strontium]])

-- | Traverses domain space breadth first 
traverseBreadthFirst :: Int -> (State -> Bool) -> State -> Maybe (Int, State)
traverseBreadthFirst limit pr state = if pr state
                                      then Just (0, state)
                                      else go (0, state) Sq.empty (S.singleton state)
  where go :: (Int, State) -> Seq (Int, State) -> Set State -> Maybe (Int, State)
        go (n, st) acc seen | pr st = Just (n, st)
                            | n >= limit = Nothing
                            | otherwise = let (next, seen') = generateSubstates n st seen
                                              acc' = acc >< next
                                          in case find (pr . snd) next of
                                               res@(Just _) -> res
                                               Nothing -> case viewl acc' of
                                                            (a :< acc'') -> go a acc'' seen'
                                                            _            -> Nothing
        generateSubstates :: Int -> State -> Set State -> (Seq (Int, State), Set State)
        generateSubstates n st seen = let cands = filter (not . flip S.member seen) $ possibleStates st
                                          cands' = Sq.fromList cands
                                      in (nextInd n <$> cands', S.union seen $ S.fromList cands)
        nextInd :: Int -> State -> (Int, State)
        nextInd n s = (n + 1, s)


-- | Displays the floor as a single string.
displayFloor :: Floor -> String
displayFloor (Floor cps gens) = let cs = unwords $ showChip <$> allChipsList
                                    gs = unwords $ showGen <$> allGeneratorsList
                                in cs ++ " " ++ gs
  where showChip c = if c `elem` cps then show c else ".."
        showGen g = if g `elem` gens then show g else ".."

-- | Writer wrapper to allow easier collection of floors
storeFloor :: (MonadWriter [String] m) => Floor -> m Floor
storeFloor flr = writer (flr, [displayFloor flr])

-- | Renders state of the building
renderState :: State -> String
renderState (State cur flrs) = "Current floor: " ++ show cur ++ "\n" ++ (intercalate "\n" . snd $ runWriter $ for flrs storeFloor)
