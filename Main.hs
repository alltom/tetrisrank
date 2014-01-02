import Data.Map (Map, empty)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import Debug.Trace (trace)
import qualified Data.HashTable.IO as H
import Data.Hashable
import System.Random
import Control.Monad

data Piece = Piece [(Int, Int)]
data Surface = Surface [Int] deriving Eq
type HashTable = H.BasicHashTable Surface Float

numColumns = 10
maxHeight = 6

instance Show Piece where
	show piece = unlines $ map (showRow piece) (rows piece) where
		rows (Piece cols) = reverse [minimum colMins .. maximum colMaxes - 1] where
			colMins = map fst cols
			colMaxes = map snd cols
		showRow (Piece cols) col = map (\(min, max) -> if col >= min && col < max then '*' else ' ') cols

instance Show Surface where
	show (Surface []) = "** empty surface **"
	show surface = unlines $ map (showRow surface) (rows surface) where
		rows (Surface cols) = reverse [0 .. maximum cols - 1]
		showRow (Surface cols) col = map (\c -> if c > col then '*' else ' ') cols

instance Hashable Surface where
	hashWithSalt salt (Surface cols) = hashWithSalt salt cols

iblock :: [Piece]                                 --      *
iblock = [ Piece [(0, 1), (0, 1), (0, 1), (0, 1)] --      *
         , Piece [(0, 4)] ]                       --      *
                                                  -- **** *

oblock :: [Piece]                                 -- **
oblock = [ Piece [(0, 2), (0, 2)] ]               -- **

tblock :: [Piece]                                 --     *       *
tblock = [ Piece [(0, 1), (0, 2), (0, 1)]         --  *  ** *** **
         , Piece [(0, 3), (1, 2)]                 -- *** *   *   *
         , Piece [(1, 2), (0, 2), (1, 2)]
         , Piece [(1, 2), (0, 3)] ]

sblock :: [Piece]                                 --     *
sblock = [ Piece [(0, 1), (0, 2), (1, 2)]         --  ** **
         , Piece [(1, 3), (0, 2)] ]               -- **   *

zblock :: [Piece]                                 --      *
zblock = [ Piece [(1, 2), (0, 2), (0, 1)]         -- **  **
         , Piece [(0, 2), (1, 3)] ]               --  ** *

lblock :: [Piece]                                 --     *      **
lblock = [ Piece [(0, 1), (0, 1), (0, 2)]         --   * *  ***  *
         , Piece [(0, 3), (0, 1)]                 -- *** ** *    *
         , Piece [(0, 2), (1, 2), (1, 2)]
         , Piece [(2, 3), (0, 3)] ]

lblock2 :: [Piece]                                --      *     **
lblock2 = [ Piece [(1, 2), (1, 2), (0, 2)]        -- ***  * *   *
          , Piece [(0, 1), (0, 3)]                --   * ** *** *
          , Piece [(0, 2), (0, 1), (0, 1)]
          , Piece [(0, 3), (2, 3)] ]

pieceClasses = [iblock, oblock, tblock, sblock, zblock, lblock, lblock2]
pieces = iblock ++ oblock ++ tblock ++ sblock ++ zblock ++ lblock ++ lblock2
emptySurface = Surface $ map (\_ -> 0) [1..numColumns]

-- find resting baseline of the piece if it can stack with no gaps
findOffset :: Surface -> Piece -> Int -> Maybe Int
findOffset (Surface (scol:scols)) (Piece ((pmin, pmax):[])) 0 =
	Just (scol - pmin)
findOffset (Surface (scol:scols)) (Piece ((pmin, pmax):pcols)) 0 =
	let offset = scol - pmin in
	case findOffset (Surface scols) (Piece pcols) 0 of
		Just followingOffset | followingOffset == offset -> Just offset
		_ -> Nothing
findOffset (Surface (_:scols)) piece colOffset =
	findOffset (Surface scols) piece (colOffset - 1)

-- surface after stacking the given piece (assumes it fits with no gaps)
placePiece :: Surface -> Piece -> Int -> Surface
placePiece surface (Piece []) colOffset = surface
placePiece (Surface (scol:scols)) (Piece ((pmin, pmax):pcols)) 0 =
	Surface ((scol + pmax - pmin) : followingCols) where
		Surface followingCols = placePiece (Surface scols) (Piece pcols) 0
placePiece (Surface (scol:scols)) piece n =
	Surface (scol : followingCols) where
		Surface followingCols = placePiece (Surface scols) piece (n - 1)

-- like placePiece, but first checks that it can be placed with no gaps
safePlacePiece :: Surface -> Piece -> Int -> Maybe Surface
safePlacePiece surface piece colOffset =
	case findOffset surface piece colOffset of
		Just _ -> Just (placePiece surface piece colOffset)
		_ -> Nothing

-- tries to place the piece in every column, returning surfaces where it fits
placePieceAnywhere :: Surface -> Piece -> [Surface]
placePieceAnywhere (Surface scols) (Piece pcols) =
	mapMaybe (safePlacePiece (Surface scols) (Piece pcols)) [0 .. (length scols) - (length pcols)]

-- tries to place every kind of piece in every column, returning the resulting surfaces
nextSurfaces :: Surface -> [Surface]
nextSurfaces surface = concatMap (\piece -> placePieceAnywhere surface piece) pieces

-- "adds one" to a surface, returning Nothing if every column is full
incrementSurface :: Surface -> Maybe Surface
incrementSurface (Surface (col:cols)) =
	if col < maxHeight then Just (Surface ((col + 1):cols))
	else (case cols of
		[] -> Nothing
		otherwise -> case incrementSurface (Surface cols) of
			Nothing -> Nothing
			Just (Surface rest) -> Just (Surface (0:rest)))

-- list of every possible surface starting with emptySurface
allSurfaces :: [Surface]
allSurfaces = rest emptySurface where
	rest surface = surface : next where
		next = case incrementSurface surface of
			Just surface' -> rest surface'
			Nothing -> []

-- makes the lowest height 1 and clamps the tallest column
canonicalizeSurface :: Surface -> Surface
canonicalizeSurface surface = helper surface where
	minHeight (Surface cols) = minimum cols
	baseline = minHeight surface
	helper (Surface []) = Surface []
	helper (Surface (col:cols)) = Surface ((clamp (col - baseline) 0 maxHeight):rest) where
		Surface rest = helper (Surface cols)

clamp :: Int -> Int -> Int -> Int
clamp i min max | i < min = min | i > max = max | otherwise = i

rand :: (Num a, Random a) => IO a
rand = newStdGen >>= return . fst . randomR (0,1)

randFloat :: IO Float
randFloat = rand

rands :: (Num a, Random a) => IO [a]
rands = newStdGen >>= return . randomRs (0,1)

randFloats :: IO [Float]
randFloats = rands

initialize :: IO HashTable
initialize = do
	floats <- randFloats
	ht <- H.fromListWithSizeHint ((maxHeight + 1)^(numColumns)) $ zip allSurfaces floats
	return ht

iteration :: HashTable -> IO HashTable
iteration ht = do
	ht' <- copy ht
	forM_ allSurfaces (rescore ht ht')
	return ht'

nIterations :: Int -> HashTable -> IO HashTable
nIterations n = foldr (<=<) return (replicate n iteration)

copy :: HashTable -> IO HashTable
copy ht = H.toList ht >>= H.fromList

rescore :: HashTable -> HashTable -> Surface -> IO ()
rescore ht ht' surface = do
	-- TODO: calculate rank of surface
	score <- rankSurface ht surface
	H.insert ht' surface score

-- Returns the average rankPiece(iteration, stack, piece) of all 7 pieces
rankSurface :: HashTable -> Surface -> IO Float
rankSurface ht surface = do
	scores <- mapM (rankPieces ht surface) pieceClasses
	return $ mean scores

-- Returns the best rankOrientation(iteration, stack, piece, orientation) of all orientations of a piece
rankPieces :: HashTable -> Surface -> [Piece] -> IO Float
rankPieces ht surface pieces = do
	scores <- mapM (rankPiece ht surface) pieces
	return $ maximum scores

-- Attempts to place the given piece in the given orientation on the given stack, in all possible locations on the stack where it can fit without creating holes beneath the stack's surface. A set of new stack surfaces is generated from these placements. This function returns the highest previously computed rank(iteration-1, new_stack_surface) of all of the new stack surfaces
rankPiece :: HashTable -> Surface -> Piece -> IO Float
rankPiece ht surface piece = do
	let surfaces = placePieceAnywhere surface piece
	maybeScores <- mapM (H.lookup ht) surfaces
	let scores = mapMaybe id maybeScores
	case scores of
		[] -> return 0.0
		otherwise -> return $ maximum scores

-- why can't I find this in a library? it calculates the mean of a list
mean = uncurry (/) . foldr (\e (s,c) -> (e+s,c+1)) (0,0)
