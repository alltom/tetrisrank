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

numColumns = 9
maxHeight = 4
numPossibleSurfaces = (maxHeight + 1)^(numColumns)

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

-- initialize a hash table with random scores for all surfaces
initialize :: IO HashTable
initialize = do
	putStrLn "initialize..."
	floats <- randFloats
	ht <- H.fromListWithSizeHint numPossibleSurfaces $ zip allSurfaces floats
	return ht

-- perform one iteration of the ranking algorithm
iteration :: HashTable -> IO HashTable
iteration ht = do
	putStrLn "iteration..."
	ht' <- H.newSized numPossibleSurfaces
	forM_ allSurfaces (rescore ht ht')
	return ht'

-- perform the given number of iterations
nIterations :: Int -> HashTable -> IO HashTable
nIterations n = foldr (<=<) return (replicate n iteration)

-- calculates the score of the given surface given ht and puts it in ht'
rescore :: HashTable -> HashTable -> Surface -> IO ()
rescore ht ht' surface = do
	score <- rankSurface (H.lookup ht) surface
	H.insert ht' surface score

-- returns the average score of placing each of the 7 types of pieces on the given surface
rankSurface :: (Surface -> IO (Maybe Float)) -> Surface -> IO Float
rankSurface lookupLastSurfaceScore surface = do
	scores <- mapM (rankPieces lookupLastSurfaceScore surface) pieceClasses
	return $ mean scores

-- returns the highest score of any placement of any of the given pieces
rankPieces :: (Surface -> IO (Maybe Float)) -> Surface -> [Piece] -> IO Float
rankPieces lookupLastSurfaceScore surface pieces = do
	scores <- mapM (rankPiece lookupLastSurfaceScore surface) pieces
	return $ maximum scores

-- returns highest score for the placement of the given piece on the given surface
rankPiece :: (Surface -> IO (Maybe Float)) -> Surface -> Piece -> IO Float
rankPiece lookupLastSurfaceScore surface piece = do
	let surfaces = placePieceAnywhere surface piece
	maybeScores <- mapM (lookupLastSurfaceScore . canonicalizeSurface) surfaces
	let scores = mapMaybe id maybeScores
	case scores of
		[] -> return 0.0
		otherwise -> return $ maximum scores

-- why can't I find this in a library? it calculates the mean of a list
mean = uncurry (/) . foldr (\e (s, c) -> (e+s, c+1)) (0, 0)

main = do
	ht <- initialize >>= nIterations 50
	items <- H.toList ht
	forM_ items (\(surface, score) -> print score)
