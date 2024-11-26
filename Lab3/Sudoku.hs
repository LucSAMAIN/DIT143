module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

newtype Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle

isValidCell :: Maybe Int -> Bool
isValidCell Nothing  = True
isValidCell (Just n) = n >= 1 && n <= 9

isSudoku :: Sudoku -> Bool
isSudoku sud = 
    length (rows sud) == 9 &&
    all (\row -> length row == 9) (rows sud) && -- all is and a map
    all isValidCell (concat (rows sud))


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sud = all (/= Nothing) (concat (rows sud))
------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud | null (rows sud) = putStr""
printSudoku sud = putStr(concatMap printRow (rows sud))
  where printRow :: Row -> String 
        printRow [] =  "\n"
        printRow (Nothing:cells) =  "." ++ printRow cells 
        printRow (Just n:cells) = show n ++ printRow cells


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do
  contents <- readFile filePath
  let sudokuRows = map parseRow (lines contents)
  let sudoku = Sudoku sudokuRows  -- a list of rows is a Sudoku !
  if isSudoku sudoku              -- check the validity of the Sudoku
    then return sudoku
    else error "Program Error: Not a Sudoku!"

parseRow :: String -> Row
parseRow = map parseCell -- a list of cell is a row !

parseCell :: Char -> Cell
parseCell '.' = Nothing
parseCell c   = Just (digitToInt c)



------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(9, return Nothing), (1, fmap Just (choose (1, 9)))]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- vectorOf 9 (vectorOf 9 cell)
    return $ Sudoku rows
 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
isOkayBlock :: Block -> Bool
isOkayBlock myList = length (nub $ filter (/= Nothing) myList)
    == (length $ filter (/= Nothing) myList)


-- * D2
blocks :: Sudoku -> [Block]
blocks sud = getSquares (rows sud)

-- Helper function to extract 3x3 sub-grids
getSquares :: [Row] -> [Block]
getSquares rows = 
  [concat [take 3 (drop c row) | row <- take 3 (drop r rows)] | r <- [0, 3, 6], c <- [0, 3, 6]]

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud =  (length blocksSud) == 9 && all (\row -> length row == 9) blocksSud
                  where blocksSud = blocks sud


-- * D3

isOkay :: Sudoku -> Bool -- block is alias for list of cell which is an alias for maybe int so usable for everything
isOkay sud = all isOkayBlock (transpose $ rows sud) && --column
             all isOkayBlock (rows sud) && --rows
             all isOkayBlock (blocks sud) -- blokcs
  
  


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
