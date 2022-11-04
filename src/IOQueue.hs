module IOQueue where

import           Data.Foldable
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as MVector

class IOQueue q where
    newQueue :: Int -> e -> IO (q e)
    capacity :: q e -> IO Int
    capacity q = (+) <$> freeSpace q <*> usedSpace q
    freeSpace :: q e -> IO Int
    freeSpace q = (-) <$> capacity q <*> usedSpace q
    usedSpace :: q e -> IO Int
    usedSpace q = (-) <$> capacity q <*> freeSpace q
    -- | Push a single element onto the queue
    push :: e -> q e -> IO Bool
    -- | Push a vector of elements onto the queue
    pushVec :: Vector.Vector e -> q e -> IO Bool
    pushVec src q = do
        let toWrite = Vector.length src
        free <- freeSpace q
        if free >= toWrite
        then do
            Vector.forM_ src $ \e -> push e q
            pure True
        else
            pure False

    -- | Pop a single element from the queue
    pop :: q e -> IO (Maybe e)
    -- | Pop a vector of elements off the queue
    popVec :: MVector.IOVector e -> q e -> IO Bool
    popVec dst q = do
        let toRead = MVector.length dst
        used <- usedSpace q
        if used >= toRead
        then do
            for_ [0..(toRead-1)] $ \i -> do
                r <- pop q
                case r of
                    Just res -> MVector.write dst i res
                    Nothing -> error "impossible, faild to pop when there should have been something"
            pure True
        else
            pure False

popVecToList :: (IOQueue q) => Int -> q e -> IO (Maybe [e])
popVecToList i q = do
    dst <- MVector.new i
    success <- popVec dst q
    if success
    then Just . Vector.toList <$> Vector.freeze dst
    else pure Nothing

pushVecFromList :: (IOQueue q) => [e] -> q e -> IO Bool
pushVecFromList es = pushVec (Vector.fromList es)
