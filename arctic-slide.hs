import Data.List
import Data.Array

-- The arctic slide game logic is based on the Macintosh Polar
-- shareware game by Go Endo. The game is a simple 4x24 grid
-- where a penguin can walk and push objects around. The goal
-- is to place all the hearts on the board into houses. Objects
-- slide frictionlessly until stopped. Bombs can be used to
-- blow up mountains. Ice blocks can be slid around or crushed.
data Tile = Empty | Tree | Mountain | House | Ice_Block |
    Bomb | Heart deriving (Show, Eq)

-- In an object-oriented implementation, Walkable, Blocking,
-- Movable, and Fixed are subclasses. For the Haskell version
-- we just make them predicates and do dispatch that way.
-- Different types of tiles have different behaviors depending
-- on what they interact with -- for example, the penguin can
-- taverse trees but trees will block any sliding objects.
walkable :: Tile -> Bool
walkable t = ( t == Empty ) || ( t == Tree )

blocking :: Tile -> Bool
blocking t = ( t /= Empty )

movable :: Tile -> Bool
movable t = ( t == Bomb ) || ( t == Heart ) || ( t == Ice_Block )

fixed :: Tile -> Bool
fixed t = ( t == House ) || ( t == Mountain )

-- Tile interactions: create a new list from the old list
-- representing the pushed object and tiles ahead of it
slide :: [ Tile ] -> [ Tile ]
slide ( Ice_Block : ts ) | ( null ts ) || ( blocking $ head ts ) = ( Ice_Block : ts )
slide ( t : Empty : ts ) = ( Empty : ( slide ( t : ts ) ) )
slide ( t : ts ) | ( null ts ) || ( blocking $ head ts ) = collide ( t : ts )

collide :: [ Tile ] -> [ Tile ]
collide [] = []
collide ( t : ts ) | fixed t = ( t : ts )
collide ( Bomb : Mountain : ts) = [ Empty, Empty ] ++ ts
collide ( Heart : House : ts ) = [ Empty, House ] ++ ts 
collide ( Ice_Block : ts ) | ( null ts ) || ( blocking $ head ts ) = ( Empty : ts )
collide ( t : ts ) | ( movable t ) && ( ( null ts ) || ( blocking $ head ts ) ) = ( t : ts )
collide ( t : Empty : ts ) | movable t = ( Empty : ( slide( t : ts ) ) )

-- Dir represents the orientation of the penguin
data Dir = North | East | South | West
    deriving ( Show, Eq )

-- Pos represents a location on the board 
data Pos = Pos { posY :: Int, posX :: Int } 
    deriving (Show, Eq)

-- I am comparing two implementations for the board: a Haskell
-- array (which is immutable, and generates a new one when we
-- update it with as association list of elements to change),
-- and a nested list (also immutable, but we can explicitly
-- share structure as we build an updated list).
type BoardArray = Array ( Int, Int ) Tile
type BoardList = [ [ Tile ] ]

-- Our array bounds
max_row :: Int
max_row = 3

max_col :: Int
max_col = 23

-- A list of tuples of array indices and tiles, used for generating
-- the updated array with (//)
type TileAssocList = [ ( ( Int, Int ), Tile ) ]

-- View functions return a list of board tiles ahead of the given
-- position, in the given direction, up to the edge of the board.
-- This is "what the penguin sees."

-- Here is a version for the array implementation. This utility
-- function just tuples up the parameters to range, for clarity
make_2d_range :: Int -> Int -> Int -> Int -> [ ( Int, Int ) ]
make_2d_range y0 x0 y1 x1 = range ( ( y0, x0 ), ( y1, x1 ) )

-- The penguin can't see the tile it is standing on, so
-- we return [] if the penguin is on max_col or max_row
-- otherwise we return a range, reversing it for the
-- west and north case. To return an the association
-- list form usable by the array (//) function I zip
-- up the key/value pairs with the list of board values
-- accessed by (!) 
view_array :: BoardArray -> Pos -> Dir -> TileAssocList 
view_array board pos dir =
    let row = ( posY pos )
        col = ( posX pos )
        coord_list = case dir of
            East  -> if ( col == max_col )     
                     then []
                     else make_2d_range row ( col + 1 ) row max_col 
            South -> if ( row == max_row )
                     then []
                     else make_2d_range ( row + 1 ) col max_row col 
            West ->  if ( col == 0 )
                     then []
                     else make_2d_range row 0 row ( col - 1 ) 
            North -> if ( row == 0 )
                     then []
                     else make_2d_range 0 col ( row - 1 ) col
        tile_assoc = zip coord_list ( map ( (!) board ) 
                                           coord_list )
    in case dir of
        East -> tile_assoc
        South -> tile_assoc
        West -> reverse tile_assoc
        North -> reverse tile_assoc 

next_board_array :: BoardArray -> Pos -> Dir -> ( Bool, BoardArray )
next_board_array board pos dir =
    let ( penguin_moved, updated_view ) = step_array $ view_array board pos dir
    in ( penguin_moved, board // updated_view )

-- Get a list of tiles in the form of 2-tuples containing
-- coordinates and tiles. Unzip, and forward a list of tiles
-- to the collision logic, then zip up with coordinates again
-- to be used for making an updated game board array.
step_array :: TileAssocList -> ( Bool, TileAssocList )
step_array [] = ( False, [] )
step_array tile_assoc = if ( walkable $ head tile_list )
                        then ( True, tile_assoc )
                        else ( False, zip coord_list
                                          ( collide tile_list ) )
    where ( coord_list, tile_list ) = unzip tile_assoc

-- Here is a version for the list implementation
nest :: [a] -> [[a]]
nest xs = [xs]

view_list :: BoardList -> Pos -> Dir -> [Tile]
view_list board pos dir =
    let row = ( posY pos )
        col = ( posX pos )
        transposed = elem dir [ South, North ]
        reversed = elem dir [ West, North ]
        orient | reversed = reverse
               | otherwise = id
        trim = case dir of
            East -> drop ( col + 1 )
            South -> drop ( row + 1 )
            West -> take col 
            North -> take row
        extract | transposed = ( transpose board ) !! col
                | otherwise = board !! row 
    in orient $ trim $ extract

step_list :: [ Tile ] -> ( Bool, [ Tile ] )
step_list [] = ( False, [] )
step_list ts = if walkable ( head ts ) then ( True, ts )
                                       else ( False, collide ts )

-- Credit is due to Jeff Licquia for some refactoring of
-- my original next_board method for the list implementation
next_board_list :: BoardList -> Pos -> Dir -> ( Bool, BoardList )
next_board_list board pos dir = 
    let ( penguin_moved, updated_view_list ) = step_list $ view_list board pos dir
    in ( penguin_moved, update_board_from_view_list board pos dir updated_view_list )

apply_view_list_to_row :: [ Tile ] -> Int -> Bool -> [ Tile ] -> [Tile]
apply_view_list_to_row orig pos True update = 
    take ( pos + 1 ) orig ++ ( init update )
apply_view_to_row orig pos False update = 
    ( reverse ( init update ) ) ++ ( drop pos orig )

apply_view_list_to_rows :: BoardList -> Int -> Int -> Bool -> [ Tile ] -> BoardList
apply_view_list_to_rows orig row pos is_forward update =
    take row orig ++
    nest ( apply_view_to_row ( orig !! row ) pos is_forward update ) ++
    drop ( row + 1 ) orig

update_board_from_view_list :: BoardList -> Pos -> Dir -> [ Tile ] -> BoardList
update_board_from_view_list board pos dir updated_view_list 
    | is_eastwest = apply_view_list_to_rows board ( posY pos ) ( posX pos )
                                            is_forward updated_view_list
    | otherwise = transpose ( apply_view_list_to_rows ( transpose board )
                              ( posX pos ) ( posY pos ) is_forward updated_view_list )
    where is_forward = elem dir [ East, South ]
          is_eastwest = elem dir [ East, West ]

-- Our world contains both a BoardArray and BoardList so
-- we can compare them
data World = World { wBoardList  :: BoardList,
                     wBoardArray :: BoardArray, 
                     wPenguinPos :: Pos,
                     wPenguinDir :: Dir,
                     wHeartCount :: Int } deriving ( Show )

init_board_list :: BoardList 
init_board_list = [[Tree,Empty,Empty,Empty,Empty,Empty,
               Empty,Empty,Empty,Empty,Empty,Empty,
               Empty,Empty,Empty,Tree,Empty,Empty,
               Empty,Empty,Empty,Ice_Block,Empty,Empty],
              [Tree,Empty,Bomb,Empty,Mountain,Empty,
               Heart,Ice_Block,Heart,Empty,Empty,Empty,
               Empty,Empty,Empty,Empty,Empty,Empty,
               Tree,Empty,Empty,Tree,Empty,Empty],
              [Tree,Empty,Empty,Empty,Empty,Empty,
               Empty,Empty,Empty,Empty,Empty,Empty,
               Empty,Empty,Empty,Empty,Heart,Empty,
               Empty,Empty,Mountain,House,Empty,Empty],
              [Tree,Tree,Empty,Empty,Empty,Empty,
               Tree,Empty,Empty,Empty,Empty,Empty,
               Empty,Empty,Empty,Empty,Empty,Empty,
               Empty,Empty,Empty,Empty,Empty,Empty]]

init_board_array :: BoardArray
init_board_array = array dimensions $ zip full_range $ concat init_board_list 
    where dimensions = ( ( 0, 0 ), ( max_row, max_col ) )
          full_range = range dimensions 

init_world :: World
init_world = ( World
    init_board_list
    init_board_array 
    ( Pos 0 0 )
    South
    3 )

next_penguin_pos :: Pos -> Dir -> Pos
next_penguin_pos pos dir = Pos ( posY pos + fst step ) ( posX pos + snd step )
    where step = delta dir
          delta East = ( 0, 1 )
          delta South = ( 1, 0 )
          delta West = ( 0, -1 )
          delta North = ( -1, 0 )
 
next_world :: World -> Dir -> World 
next_world old_world move_dir =
    if ( move_dir /= wPenguinDir old_world )
    then ( World ( wBoardList old_world ) ( wBoardArray old_world ) 
                 ( wPenguinPos old_world ) move_dir ( wHeartCount old_world ) )
    else ( World board_list board_array  
                 ( if penguin_moved_array then next_penguin_pos ( wPenguinPos old_world ) ( wPenguinDir old_world )
                                          else ( wPenguinPos old_world ) )
                 ( wPenguinDir old_world )
                 ( wHeartCount old_world ) )
    where ( penguin_moved_array, board_array ) = next_board_array ( wBoardArray old_world ) ( wPenguinPos old_world ) ( wPenguinDir old_world )
          ( unused_penguin_moved_list, board_list ) = next_board_list ( wBoardList old_world ) ( wPenguinPos old_world ) ( wPenguinDir old_world )

pretty_tiles :: [Tile] -> String
pretty_tiles [] = "\n"
pretty_tiles (t:ts) = case t of
                 Empty     -> "___"
                 Mountain  -> "mt "
                 House     -> "ho "
                 Ice_Block -> "ic "
                 Heart     -> "he "
                 Bomb      -> "bo "
                 Tree      -> "tr "
             ++ pretty_tiles ts

pretty_board_list :: BoardList -> String
pretty_board_list [] = ""
pretty_board_list (ts:tss) = pretty_tiles ts ++ pretty_board_list tss

split_tile_list :: [ Tile ] -> [ [ Tile ] ]
split_tile_list [] = []
split_tile_list ts = [ take tiles_in_row ts ] ++
                     ( split_tile_list $ ( drop tiles_in_row ) ts )
    where tiles_in_row = max_col + 1
 
pretty_board_array :: BoardArray -> String 
pretty_board_array board = pretty_board_list split_tiles 
    where full_range = make_2d_range 0 0 max_row max_col
          all_tiles = map ( (!) board ) full_range
          split_tiles = split_tile_list all_tiles

pretty_world :: World -> String
pretty_world world =
    "penguin @: " ++ show ( wPenguinPos world ) ++
    ", facing: "  ++ show ( wPenguinDir world ) ++
    ", hearts: "  ++ show ( wHeartCount world ) ++
    -- list implementation is currently broken
    --"\n" ++ pretty_board_list ( wBoardList world ) ++ 
    "\n" ++ pretty_board_array ( wBoardArray world )

moves_to_dirs :: [(Dir, Int)] -> [Dir]
moves_to_dirs [] = []
moves_to_dirs (m:ms) = replicate ( snd m ) ( fst m ) ++ moves_to_dirs ms 

moves_board_1 = [(East,21),(South,2),(East,3),(North,2),(West,2)
                 ,(South,4),(West,7),(North,2)
                 ,(West,14),(North,3),(West,2),(North,2),(West,3),(South,2),(West,2),(South,3),(East,2)
                 ,(East,5),(North,3),(East,3),(South,2)
                 ,(East,3),(South,2),(West,2),(North,2),(West,3),(South,2),(West,3),(South,3),(East,3)
                 ,(East,11),(North,2),(West,11),(North,2),(West,2),(South,2),(West,3),(South,3),(East,3)
                 ,(West,2),(North,3),(East,2),(South,2),(West,2),(South,3),(East,2)
    ]

move_sequence :: [(Dir,Int)] -> [World]
move_sequence repeats = scanl next_world init_world steps
    where steps = moves_to_dirs repeats 

move_sequence' :: [(Dir,Int)] -> World
move_sequence' repeats = foldl next_world init_world steps
    where steps = moves_to_dirs repeats

main :: IO ()
main = do
    mapM_ putStrLn pretty_worlds
    -- putStrLn pretty_final_world 
    where worlds = move_sequence moves_board_1
          --final_world = move_sequence' moves_board_1
          pretty_worlds = map pretty_world worlds
          -- pretty_final_world = pretty_world final_world
