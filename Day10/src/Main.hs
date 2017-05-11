{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Writer
import Data.List (partition, foldl', find)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  instructions <- parseInput path
  print $ (findOutputs [0, 1, 2] . fst . fst) . runWriter . simulate . initialState <$> instructions
           

-- | Type for bot
data Bot = Bot Int
  deriving (Show, Eq, Ord)

-- | Type for output
data Output = Output Int
  deriving (Show, Eq, Ord)

-- | Holder is either a bot or an output.
data Holder = BotHolder Bot
            | OutputHolder Output
            deriving (Show, Eq, Ord)

-- | Chip type - ID of the chip
type Chip = Int

-- | Rule describing transfer of chips
data TransferRule = TR Bot Holder Holder
  deriving (Show, Eq)

-- | Instruction is either initial assignment or transfer description
data Instruction = Assignment { getChip :: Chip, getHolder :: Bot }
                 | Transfer { getRule :: TransferRule }
                 deriving (Show, Eq)

-- | Current state of bot's inventory
data BotInventory = HasOne Chip
                  | HasTwo Chip Chip
                  deriving (Show, Eq)

-- | Current state of the holder
data HolderState = BotState Bot BotInventory
                 | OutputState Output [Chip]
                 deriving (Show, Eq)

-- | Type synonim for single step of the iteration.
type FlowState = Map Holder HolderState

-- | Map of bot -> rules
type FlowRules = Map Bot TransferRule

-- | Comparison we are interested in
data Comparison = Comp Bot Chip Chip
  deriving (Show, Eq)

-- | Generates initial state from instructions.
initialState :: [Instruction] -> (FlowState, FlowRules)
initialState instr = let (assigns, flows) = partition isAssignment instr
                         flowsMap = M.fromList $ (\r@(TR h _ _) -> (h, r)) . getRule <$> flows
                     in (create assigns, flowsMap)
  where create = foldl' addAssignment M.empty
        addAssignment m (Assignment chip bot) = M.alter (up chip (BotHolder bot)) (BotHolder bot) m
        addAssignment _ _ = undefined
        up c (BotHolder bot) (Just (BotState _ (HasOne chip))) = Just (BotState bot (HasTwo c chip))
        up c (BotHolder bot) Nothing = Just (BotState bot (HasOne c))
        up _ _ _      = undefined
                                                   

-- | Writer for comparisons
logComparison :: (MonadWriter [Comparison] m) => Comparison -> m ()
logComparison c = writer ((), [c])

-- | Returns next bot that has both chips and is ready to compare them
nextReadyBot :: FlowState -> Maybe Bot
nextReadyBot state = bot <$> find ready (M.elems state)
  where ready (BotState _ (HasTwo _ _)) = True
        ready _                         = False
        bot (BotState b _)              = b
        bot _                           = undefined

-- | Simulates a single transition-of-chips step recording all comparisons underway
simulate :: (FlowState, FlowRules) -> Writer [Comparison] (FlowState, FlowRules)
simulate (state, rules) = case simulationState state rules of
                            Just (bot, bs, br) -> do
                              logComparison $ comparison bot bs
                              simulate $ applyRule bot bs br
                            Nothing -> return (state, rules)
  where simulationState :: FlowState -> FlowRules -> Maybe (Bot, HolderState, TransferRule)
        simulationState st rls = do
          bot <- nextReadyBot st
          rule <- M.lookup bot rls
          bst <- M.lookup (BotHolder bot) st
          return (bot, bst, rule)

        comparison :: Bot -> HolderState -> Comparison
        comparison bot (BotState _ (HasTwo chipA chipB)) = Comp bot chipA chipB
        comparison _   _                                 = undefined

        applyRule :: Bot -> HolderState -> TransferRule -> (FlowState, FlowRules)
        applyRule bot (BotState _ (HasTwo chipA chipB)) (TR _ hldrA hldrB) = let (mx, mn) = (max chipA chipB, min chipA chipB)
                                                                                 st  = M.delete (BotHolder bot) state
                                                                                 st' = addChip st hldrA mn
                                                                                 st'' = addChip st' hldrB mx
                                                                             in (st'', rules)
        applyRule _ _ _                                                    = undefined
        addChip :: FlowState -> Holder -> Chip -> FlowState
        addChip st holder chip = M.alter add holder st
          where add :: Maybe HolderState -> Maybe HolderState
                add Nothing = case holder of
                                BotHolder bot    -> Just (BotState bot (HasOne chip))
                                OutputHolder out -> Just (OutputState out [chip])
                add (Just (OutputState out chips)) = Just (OutputState out (chip:chips))
                add (Just (BotState bot (HasOne c))) = Just (BotState bot (HasTwo c chip))
                add _ = undefined

-- | Finds bot responsible for comparison of two chips
findComparison :: Chip -> Chip -> [Comparison] -> Maybe Comparison
findComparison chipA chipB = find finder 
  where finder (Comp _ cA cB) = (cA == chipA && cB == chipB) || (cA == chipB && cB == chipA)

-- | Finds all holders in given state mathing predicate.
findHolders :: (HolderState -> Bool) -> FlowState -> [HolderState]
findHolders pr state = filter pr (M.elems state)

-- | Finds all outputs in state with given ids
findOutputs :: [Int] -> FlowState -> [HolderState]
findOutputs ids = findHolders pr
  where pr (OutputState (Output out) _) = out `elem` ids
        pr _                           = False

-- | Parses bot entry
parseBot :: Parser Holder
parseBot = do
  _ <- string "bot "
  i <- many1 digit
  return $ BotHolder (Bot (read i))

-- | Parses output entry.
parseOutput :: Parser Holder
parseOutput = do
  _ <- string "output "
  i <- many1 digit
  return $ OutputHolder (Output (read i))

-- | Parses any holder of the chip
parseHolder :: Parser Holder
parseHolder = parseBot <|> parseOutput

-- | Parses assignment instruction.
parseAssignment :: Parser Instruction
parseAssignment = do
  _ <- string "value "
  val <- read <$> many1 digit
  _ <- string " goes to "
  (BotHolder bot) <- parseBot
  return $ Assignment val bot 

-- | Parses transfer instruction.
parseTransfer :: Parser Instruction
parseTransfer = do
  (BotHolder source) <- parseBot
  _ <- string " gives low to "
  tlow <- parseHolder
  _ <- string " and high to "
  thigh <- parseHolder
  return $ Transfer (TR source tlow thigh)

-- | Parses an instruction
parseInstruction :: Parser Instruction
parseInstruction = parseAssignment <|> parseTransfer

-- | Parses input as a list of 
parseInput :: FilePath -> IO (Either ParseError [Instruction])
parseInput path = do
  text <- IO.readFile path
  return $ sequence $ parse parseInstruction path <$> T.lines text

isAssignment :: Instruction -> Bool
isAssignment (Assignment _ _) = True
isAssignment _                = False 

