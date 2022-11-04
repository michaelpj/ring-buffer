module RingBuffer (RingBuffer) where

import           IOQueue

import           Data.Functor
import           Data.IORef
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as MVector
import           System.IO.Unsafe    (unsafePerformIO)


data RingBuffer e = RingBuffer
    { readPtr      :: IORef Int
    , writePtr     :: IORef Int
    -- | Has one extra element to distinguish empty vs full
    , backingArray :: MVector.IOVector e
    }

showRingBuffer :: Show e => RingBuffer e -> IO String
showRingBuffer rb = do
    r <- readIORef $ readPtr rb
    w <- readIORef $ writePtr rb
    es <- Vector.toList <$> Vector.freeze (backingArray rb)
    pure $ "RingBuffer (" ++ show r ++ ", " ++ show w ++ ", " ++ show es ++ ")"

instance Show e => Show (RingBuffer e) where
    show rb = unsafePerformIO $ showRingBuffer rb

instance IOQueue RingBuffer where
    newQueue = newRingBuffer
    capacity = capacityRB
    freeSpace = freeSpaceRB
    usedSpace = usedSpaceRB
    push = pushRB
    pushVec = pushVecRB
    pop = popRB
    popVec = popVecRB

newRingBuffer :: Int -> e -> IO (RingBuffer e)
-- c+1 rather than c because one extra element
newRingBuffer c def = RingBuffer <$> newIORef 0 <*> newIORef 0 <*> MVector.replicate (c+1) def

capacityRB :: RingBuffer e -> IO Int
capacityRB rb = pure $ arraySize rb - 1

arraySize :: RingBuffer e -> Int
arraySize rb = MVector.length $ backingArray rb

usedAndFreeSpace :: RingBuffer e -> IO (Int, Int)
usedAndFreeSpace rb = do
    r <- readIORef $ readPtr rb
    w <- readIORef $ writePtr rb
    c <- capacityRB rb
    let s = arraySize rb
        used =
            case compare r w of
                EQ -> 0
                LT -> w-r
                GT -> (w+s) - r
    pure (used, c-used)

{-# INLINE freeSpaceRB #-}
freeSpaceRB :: RingBuffer e -> IO Int
freeSpaceRB rb = snd <$> usedAndFreeSpace rb

{-# INLINE usedSpaceRB #-}
usedSpaceRB :: RingBuffer e -> IO Int
usedSpaceRB rb = fst <$> usedAndFreeSpace rb

incrBufPtr :: Int -> IORef Int -> RingBuffer e -> IO ()
incrBufPtr i ptr rb = modifyIORef ptr $ \v -> (v + i) `mod` arraySize rb

pushRB :: e -> RingBuffer e -> IO Bool
pushRB e rb = do
    free <- freeSpaceRB rb
    if free > 0
    then do
        w <- readIORef $ writePtr rb
        MVector.write (backingArray rb) w e
        incrBufPtr 1 (writePtr rb) rb
        pure True
    else
        pure False

pushVecRB :: Vector.Vector e -> RingBuffer e -> IO Bool
pushVecRB src rb = do
    let toWrite = Vector.length src
    free <- freeSpaceRB rb
    let s = arraySize rb
    if free >= toWrite
    then do
        w <- readIORef $ writePtr rb
        if w + toWrite <= s
        then do
            let writeWindow =  MVector.slice w toWrite (backingArray rb)
            Vector.copy writeWindow src
            incrBufPtr toWrite (writePtr rb) rb
        else do
            -- Doesn't fit in the segment to the end. Split the input
            -- into a segment that will exactly fit -- and the rest,
            -- recurse on each. Both will succeed, because toWrite
            -- can't be bigger than the capacity othewise we wouldn't have got here.
            let (v1, v2) = Vector.splitAt (s-w) src
            void $ pushVecRB v1 rb
            void $ pushVecRB v2 rb
        pure True
    else
        pure False

popRB :: RingBuffer e -> IO (Maybe e)
popRB rb = do
    used <- usedSpaceRB rb
    if used > 0
    then do
        r <- readIORef $ readPtr rb
        e <- MVector.read (backingArray rb) r
        incrBufPtr 1 (readPtr rb) rb
        pure $ Just e
    else
        pure Nothing

popVecRB :: MVector.IOVector e -> RingBuffer e -> IO Bool
popVecRB dst rb = do
    let toRead = MVector.length dst
    used <- usedSpaceRB rb
    let s = arraySize rb
    if used >= toRead
    then do
        r <- readIORef $ readPtr rb
        if r + toRead <= s
        then do
            let readWindow =  MVector.slice r toRead (backingArray rb)
            MVector.copy dst readWindow
            incrBufPtr toRead (readPtr rb) rb
        else do
            -- Not enough in the segment to the end. Split the input
            -- into a segment that will exactly fit -- and the rest,
            -- recurse on each. Both will succeed, because toRead
            -- can't be bigger than the capacity othewise we wouldn't have got here.
            let (v1, v2) = MVector.splitAt (s-r) dst
            void $ popVecRB v1 rb
            void $ popVecRB v2 rb
        pure True
    else
        pure False
