module Y2024.Day9.Day9 (getDaySolutions) where

import Data.Char (digitToInt)


getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)


solve1 :: String -> String
solve1 = show . checksum 0 . compactDisk . addIds 0 . parseDisk

solve2 :: String -> String
solve2 = concatMap show . compactEfficientDisk . toEfficientDisk . addIds 0 . parseDisk


-- | A file block has an ID
type ID = Int


-- | A single file block can be a data block or an empty block
data Block = DataBlock ID | Empty | Nop
    deriving (Eq)


-- | DataBlocks and empty blocks grouped together, with the size of the blocks
data Block2 = Datablocks ID Int | EmptySpace Int
    deriving (Eq)


instance Show Block where
    show (DataBlock n) = show n
    show Empty = "."
    show Nop = ""


instance Show Block2 where
    show (Datablocks n m) = replicate m (head $ show n)
    show (EmptySpace m) = replicate m '.'


-- | A disk contains a list of blocks
type Disk = [Block]


-- | A disk with efficient representation
type EfficientDisk = [Block2]


-- | Build a Disk from a compact String representation
parseDisk :: String -> Disk
parseDisk [] = []
parseDisk [x] = [DataBlock 0 | _ <- [0..(digitToInt x - 1)]]
parseDisk (x:'0':ys) = [DataBlock 0 | _ <- [0..(digitToInt x - 1)]] ++ [Nop] ++ parseDisk ys
parseDisk (x:y:xs) = [DataBlock 0 | _ <- [0..(digitToInt x - 1)]] ++ [Empty | _ <- [0..(digitToInt y - 1)]] ++ parseDisk xs


-- | Give all DataBlocks that are adjacent the same ID
addIds :: Int -> Disk -> Disk
addIds _ [] = []
addIds n [DataBlock _] = [DataBlock n]
addIds n (DataBlock _:xs) = DataBlock n : addIds n xs
addIds n (Empty:x@(DataBlock _):xs) = Empty : addIds (n + 1) (x:xs)
addIds n (Empty:xs) = Empty : addIds n xs
addIds n (Nop:xs) = addIds (n + 1) xs


-- | Count all non-empty blocks in a disk
countBlocks :: Disk -> Int
countBlocks = length . filter (/= Nop) . filter (/= Empty)


-- | Find the first Empty block and insert a given DataBlock
insertBlock :: Block -> Disk -> Disk
insertBlock _ [] = []
insertBlock b (Empty:xs) = b : xs
insertBlock b (x:xs) = x : insertBlock b xs


-- | Filter DataBlocks, reverse the list and insert all DataBlocks in the first Empty block until no Empty block is left
compactDisk :: Disk -> Disk
compactDisk disk = take (countBlocks disk) $ foldr insertBlock disk $ filter (/= Empty) disk


-- | Compute the checksum of the disk
checksum :: Int -> Disk -> Int
checksum _ [] = 0
checksum n (Empty:xs) = checksum (n + 1) xs
checksum _ (Nop:_) = error "Nop blocks should not be in the disk"
checksum n (DataBlock m:xs) = n * m + checksum (n + 1) xs


-- | Convert a Disk to an EfficientDisk
toEfficientDisk :: Disk -> EfficientDisk
toEfficientDisk [] = []
toEfficientDisk (Nop:_) = error "Nop blocks should not be in the disk"
toEfficientDisk l@(DataBlock n:_) = Datablocks n (length $ takeWhile (== DataBlock n) l) : toEfficientDisk (dropWhile (== DataBlock n) l)
toEfficientDisk l@(Empty:_) = EmptySpace (length $ takeWhile (== Empty) l) : toEfficientDisk (dropWhile (== Empty) l)


insertBlock2 :: Block2 -> EfficientDisk -> EfficientDisk
insertBlock2 _ [] = []
insertBlock2 (EmptySpace _) xs = xs
insertBlock2 (Datablocks n m) (EmptySpace e:xs) | m == e = Datablocks n m : clearPrevious (Datablocks n m) xs
    | m < e = [Datablocks n m] ++ [EmptySpace (e - m)] ++ clearPrevious (Datablocks n m) xs
    | otherwise = EmptySpace e : insertBlock2 (Datablocks n m) xs
insertBlock2 b (x:xs) = x : insertBlock2 b xs


clearPrevious :: Block2 -> EfficientDisk -> EfficientDisk
clearPrevious _ [] = []
clearPrevious (Datablocks id' len') (Datablocks id'' len'':xs) | id' == id'' && len' == len'' = EmptySpace len' : xs
    | otherwise = Datablocks id'' len'' : clearPrevious (Datablocks id' len') xs
clearPrevious b (x:xs) = x : clearPrevious b xs


compactEfficientDisk :: EfficientDisk -> EfficientDisk
compactEfficientDisk disk = foldr insertBlock2 disk $ filter (/= EmptySpace 0) disk