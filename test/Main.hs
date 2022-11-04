{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TBQueue
import           Data.Proxy
import           Data.Traversable               (for)
import           GHC.Generics
import           IOQueue
import           RingBuffer
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testProperty "TBQueue model" (prop_model (Proxy @RingBuffer) (Proxy @TBQueueN))

-- | Operations to use to exercise the queues
data Op e = Push e | Pop | PushVec [e] | PopVec Int
    deriving (Show, Eq, Ord, Generic)

-- We won't use this for generation but it's handy to have the generic
-- 'shrink' function available.
instance Arbitrary e => Arbitrary (Op e) where
    arbitrary = oneof [genPush, genPop]
    shrink = shrinkOp

genOp :: Arbitrary e => Int -> Gen (Op e)
genOp cap = oneof [genPush, genPop, genPushVec cap, genPopVec cap]

genPush :: Arbitrary e => Gen (Op e)
genPush = Push <$> arbitrary

genPop :: Arbitrary e => Gen (Op e)
genPop = pure Pop

genPushVec :: Arbitrary e => Int -> Gen (Op e)
genPushVec cap = do
    es <- scale (\sz -> min sz cap) (listOf arbitrary)
    pure $ PushVec es

genPopVec :: Arbitrary e => Int -> Gen (Op e)
genPopVec cap = do
    toPop <- chooseInt (0, cap)
    pure $ PopVec toPop

shrinkOp :: (Arbitrary e) => Op e -> [Op e]
shrinkOp = genericShrink

-- Annoyingly TBQueue doesn't let you determine the capacity or free space!
-- This wrapper just remembers the capacity so you can calculate that.
data TBQueueN e = TBQueueN Int (TBQueue e)

instance IOQueue TBQueueN where
    newQueue i _ =  do
        q <- atomically $ newTBQueue (fromIntegral i)
        pure $ TBQueueN i q
    capacity (TBQueueN i _) =  pure i
    usedSpace (TBQueueN _ q) = fromIntegral <$> atomically (lengthTBQueue q)
    push e (TBQueueN _ q) = atomically $ do
        full <- isFullTBQueue q
        if full
        then pure False
        else writeTBQueue q e >> pure True
    pop (TBQueueN _ q) = atomically $ tryReadTBQueue q

checkConsistent :: (IOQueue q1, IOQueue q2) => q1 e -> q2 e -> IO Property
checkConsistent q1 q2 = do
    caps <- (===) <$> capacity q1 <*> capacity q2
    used <- (===) <$> usedSpace q1 <*> usedSpace q2
    free <- (===) <$> freeSpace q1 <*> freeSpace q2
    pure $
        counterexample "capacity differs" caps
        .&&. counterexample "used space differs" used
        .&&. counterexample "free space differs" free

prop_model :: forall q1 q2 . (IOQueue q1, IOQueue q2) => Proxy q1 -> Proxy q2 -> Property
prop_model _ _ = property $ \(NonNegative (i :: Int)) -> forAllShrink (listOf (genOp i)) shrink $ \(ops :: [Op Int]) -> ioProperty $ do
    (q1 :: q1 Int) <- newQueue i 0
    (q2 :: q2 Int) <- newQueue i 0
    opConsistent <- for ops $ \o -> do
        resConsistent <- case o of
            Push e -> (===) <$> push e q1 <*> push e q2
            Pop -> (===) <$> pop q1 <*> pop q2
            PushVec es -> (===) <$> pushVecFromList es q1 <*> pushVecFromList es q2
            PopVec n -> (===) <$> popVecToList n q1 <*> popVecToList n q2

        stateConsistent <- checkConsistent q1 q2
        pure $ counterexample ("after operation " ++ show o) $
            counterexample "result differs" resConsistent
            .&&. counterexample "state differs" stateConsistent

    pure $ conjoin opConsistent



