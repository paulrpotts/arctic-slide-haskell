data Dir = North | East | South | West
    deriving (Show, Eq)

data Pos y x = Pos Int Int 
    deriving (Show, Eq)

-- N.B.: capitalization of initial letters in posY, posX is
-- semantically important!
posY ( Pos y x ) = y 
posX ( Pos y x ) = x

data Tile = Empty | Tree | Mountain | House | Ice_Block |
    Bomb | Heart | Edge deriving (Show, Eq)

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

-- All the tutorials say "don't use arrays, don't use arrays,
-- don't use arrays," at least not until you've worked out what
-- lists can do, and need to optimize the implementation. So,
-- fine. Let's try lists -- a list of rows of tiles -- and some
-- functions to extract "slices" in 4 different directions. 
type Board = [[Tile]] 

-- Interaction logic operates on a list of tiles extrated from
-- the board, starting at a given pos and going in the given
-- dir, until the edge of the board is reached.
-- TODO: how do I declare nice Haskell constants?
-- Also, these parens seem excessive... 
slice :: Board -> Pos y x -> Dir -> [Tile]
slice board pos East = drop ( posX pos ) 
    $ head $ drop ( posY pos ) board 
slice _ _ _ = error "slice: not handled yet!" 
 
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

-- Move gets the board, the penguin position, the penguin
-- direction, and the direction of movement. If the penguin
-- is moved in a new direction, it just turns to face the
-- new direction and nothing else changes. If the penguin
-- is moved in the direction it is facing, then we attempt
-- to step and pass a slice of the board 
-- the tiles in the direction the penguin is facing
move :: Board -> Pos y x -> Dir -> Dir -> 
    ( [Tile], Pos y x, Dir, Dir )
move board pos move_dir penguin_dir =
    if move_dir /= penguin_dir
    then ( head board, pos, move_dir, move_dir )
    else ( collide $ slice board (Pos 1 0) penguin_dir, 
        pos, penguin_dir, penguin_dir ) 

main :: IO ()
main = do
--    if ( ( move True, (Pos 0 0) East West ) /= ( True, ( Pos 0 0 ), East, East ) )
--        then error "move: failed test 1"
--        else putStrLn "Move test" 
--    let tiles = [Empty, Empty, Empty, Mountain]
    putStrLn "ArcticSlide start"

