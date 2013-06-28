import Data.List

data Dir = North | East | South | West
    deriving (Show, Eq)

data Pos = Pos { posY :: Int, posX :: Int } 
    deriving (Show, Eq)

data Tile = Empty | Tree | Mountain | House | Ice_Block |
    Bomb | Heart | Edge deriving (Show, Eq)

-- All the tutorials say "don't use arrays, don't use arrays,
-- don't use arrays," at least not until you've worked out what
-- lists can do, and need to optimize the implementation. So,
-- fine. Let's try lists -- a list of rows of tiles -- and some
-- functions to extract "slices" in 4 different directions. 
type Board = [[Tile]] 

data World = World { board :: Board, penguinPos :: Pos, penguinDir :: Dir,
                     heartCount :: Int } deriving (Show)

-- Different types of tiles have different properties in
-- different interaction contexts: 

-- The penguin can walk through empty tiles or trees (forest)
walkable :: Tile -> Bool
walkable t = ( t == Empty ) || ( t == Tree )

-- But everything except empty tiles will block sliding objects
blocking :: Tile -> Bool
blocking t = ( t /= Empty )

-- A subset of tiles are movable (and will slide until blocked)
movable :: Tile -> Bool
movable t = ( t == Bomb ) || ( t == Heart ) || ( t == Ice_Block )

-- A subset of tiles aren't movable; note that this set
-- overlaps blocking and that Tree is both walkable and fixed
fixed :: Tile -> Bool
fixed t = ( t == House ) || ( t == Mountain ) || ( t == Edge )

-- Interaction logic operates on a list of tiles extrated from
-- the board, starting at a given pos and going in the given
-- dir, until the edge of the board is reached.
slice :: Board -> Pos -> Dir -> [Tile]
slice board pos East = ( drop ( posX pos ) ( board !! ( posY pos ) ) ) ++ [Edge]
slice board pos South = ( drop ( posY pos ) $ ( transpose board ) !! ( posX pos ) ) ++ [Edge]
slice board pos West = ( reverse $ take ( posX pos + 1 ) $ board !! ( posY pos ) ) ++ [Edge]
slice board pos North = ( reverse $ take ( posY pos + 1 ) $ ( transpose board ) !! ( posX pos ) ) ++ [Edge]
 
slide :: [Tile] -> [Tile]
slide [] = error "slide empty list!"
slide [t] = error "single item list!"
slide [t, Edge] = [t, Edge] 
--slide (t:(Empty:ts)) = if movable t && ( head ts == Empty )
--    then Empty ++ slide t ++ rest ts
--    else  

collide :: [Tile] -> [Tile]
collide (t:(Empty:ts)) | movable t =
    [Empty] ++ collide (t:ts)
collide (Bomb:(Mountain:ts)) = [Empty, Empty] ++ ts 
collide (Heart:House:ts) = [Empty, House] ++ ts
collide (_) = error "collide: unexpected case!"

-- Step should never be given an empty list; the edge case is
-- [Edge]. Return true if the penguin can move onto the tile
-- at the head. If the head tile is not walkable, it may be
-- pushable, so delegate to collide, which may return a new
-- list. 
step :: [Tile] -> ( Bool, [Tile] )
step [] = error "step: empty list!"
step ts = if walkable (head ts) then ( True, ts )
                                else ( False, collide ts ) 

get_initial_board :: [[Tile]]
get_initial_board = [[Tree,Empty,Empty,Empty,Empty,Empty,
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

penguin_view :: Board -> Pos -> Dir -> [Tile]
penguin_view board pos dir = drop 1 ( slice board pos dir )
 
get_initial_world :: World
get_initial_world = ( World get_initial_board ( Pos 0 0 ) East 3 )

move :: World -> Dir -> World 
move world move_dir =
    if move_dir /= penguinDir world 
    then ( World get_initial_board ( penguinPos world ) 
           move_dir ( heartCount world ) )
    else ( World get_initial_board ( penguinPos world )
           ( penguinDir world ) ( heartCount world ) )
 
main :: IO ()
main = do
--    if ( ( move True, (Pos 0 0) East West ) /= ( True, ( Pos 0 0 ), East, East ) )
--        then error "move: failed test 1"
--        else putStrLn "Move test" 
--    let tiles = [Empty, Empty, Empty, Mountain]
    putStrLn "ArcticSlide start"

