import Data.Map (Map, empty)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import Debug.Trace (trace)

data Piece = Piece [(Int, Int)]
data Surface = Surface [Int]

instance Show Surface where
	show (Surface []) = "** empty surface **"
	show surface = unlines $ map (showRow surface) (rows surface) where
		rows (Surface cols) = reverse [0 .. maximum cols - 1]
		showRow (Surface cols) col = map (\c -> if c > col then '*' else ' ') cols

instance Show Piece where
	show piece = unlines $ map (showRow piece) (rows piece) where
		rows (Piece cols) = reverse [minimum colMins .. maximum colMaxes - 1] where
			colMins = map fst cols
			colMaxes = map snd cols
		showRow (Piece cols) col = map (\(min, max) -> if col >= min && col < max then '*' else ' ') cols

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

pieces = iblock ++ oblock ++ tblock ++ sblock ++ zblock ++ lblock ++ lblock2

emptySurface = Surface (0:0:0:0:0:0:0:0:0:0:[])
testSurface = Surface (1:0:0:0:0:0:0:0:0:0:[])
testSurface2 = Surface (1:1:2:0:0:0:0:0:0:0:[])
testSurface3 = Surface (1:1:2:1:1:1:1:1:1:5:[])

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
nextSurfaces surface = foldl (\surfaces -> \piece -> surfaces ++ (placePieceAnywhere surface piece)) [] pieces

incrementSurface :: Int -> Int -> Surface -> Maybe Surface
incrementSurface minHeight maxHeight (Surface (col:cols)) =
	if col < maxHeight then Just (Surface ((col + 1):cols))
	else (case cols of
		[] -> Nothing
		otherwise -> case incrementSurface minHeight maxHeight (Surface cols) of
			Nothing -> Nothing
			Just (Surface rest) -> Just (Surface (minHeight:rest)))

allSurfaces = rest emptySurface where
	rest surface = surface : next where
		next = case incrementSurface 0 2 surface of
			Just surface -> rest surface
			Nothing -> []

normalizeSurface surface = helper surface where
	minHeight (Surface cols) = minimum cols
	baseline = minHeight surface
	helper (Surface []) = Surface []
	helper (Surface (col:cols)) = Surface ((clamp (col - baseline) 0 4):rest) where
		Surface rest = helper (Surface cols)

clamp i min max | i < min = min | i > max = max | otherwise = i

--hashSurface :: Surface -> Int
--hashSurface (Surface cols) = foldl combine 0 cols where
--	combine hash col = hash + (clamp col 0 4)
