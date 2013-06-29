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

data World = World { wBoard :: Board, wPenguinPos :: Pos,
                     wPenguinDir :: Dir, wHeartCount :: Int }
                     deriving (Show)

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
view :: Board -> Pos -> Dir -> [Tile]
view board pos East = ( drop ( posX pos + 1 ) $
    board !! ( posY pos ) ) ++ [Edge]
view board pos South = ( drop ( posY pos + 1 ) $
    ( transpose board ) !! ( posX pos ) ) ++ [Edge]
view board pos West = ( reverse $ take ( posX pos ) $
    board !! ( posY pos ) ) ++ [Edge]
view board pos North = ( reverse $ take ( posY pos ) $
    ( transpose board ) !! ( posX pos ) ) ++ [Edge]
 
slide :: [Tile] -> [Tile]
slide (Ice_Block:t:ts) | blocking t = (Ice_Block:t:ts)
slide (t:Empty:ts) | movable t = (Empty:(collide(t:ts)))

collide :: [Tile] -> [Tile]
collide (Bomb:Mountain:ts) = [Empty, Empty] ++ ts 
collide (Heart:House:ts) = [Empty, House] ++ ts
collide (Ice_Block:t:ts) | blocking t = (Empty:t:ts) 
collide (t:Empty:ts) | movable t = (Empty:(slide(t:ts)))
collide (Edge:_) = [Edge] 

-- Step should never be given an empty list; the edge case is
-- [Edge]. Return true if the penguin can move onto the tile
-- at the head. If the head tile is not walkable, it may be
-- pushable, so delegate to collide, which may return a new
-- list. 
step :: [Tile] -> ( Bool, [Tile] )
step [] = error "step: empty list!"
step ts = if walkable (head ts) then ( True, ts )
                                else ( False, collide ts ) 

init_board :: [[Tile]]
init_board = [[Tree,Empty,Empty,Empty,Empty,Empty,
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

init_world :: World
init_world = ( World init_board ( Pos 0 0 ) South 3 )

nest :: [a] -> [[a]]
nest xs = [xs]

-- This is overly complicated -- basically, we build a new board
-- out of portions of the starting board wrapped around a call
-- to generate an updated view. It becomes particularly ugly
-- when we're undoing the appending of Edge using init, and
-- undoing the reversing that view has done when looking North
-- and West, and working with the transposed board for North and
-- South. 
--
-- Can I please replace this with a real 2-D array now? Unless
-- there is a radical simplification possible here?
--
-- I will say of Haskell, and working functionally with lists --
-- it was FAR easier to get this debugged and working than it
-- would have been with an imperative language. The question is
-- whether avoiding mutation is worth all this.
next_board :: Board -> Pos -> Dir -> ( Bool, Board )
next_board board pos East =
    let ( penguin_could_move, updated_view ) = step $ view board pos East
    in (
        penguin_could_move, 
        take ( posY pos ) board ++
        nest (
            ( take ( posX pos + 1 )
                ( board !! ( posY pos ) ) ) ++ 
            ( init updated_view ) ) ++
        drop ( posY pos + 1 ) board )
next_board board pos South =
    let ( penguin_could_move, updated_view ) = step $ view board pos South
    in (
        penguin_could_move, 
        transpose (
            take ( posX pos ) ( transpose board ) ++
            nest (
                ( take ( posY pos + 1 )
                    ( ( transpose board ) !! ( posX pos ) ) ) ++ 
                ( init updated_view ) ) ++
        drop ( posX pos + 1 ) ( transpose board ) ) )
next_board board pos West =
    let ( penguin_could_move, updated_view ) = step $ view board pos West 
    in (
        penguin_could_move,
        take ( posY pos ) board ++
        nest (
            ( reverse ( init updated_view ) ) ++
            ( drop ( posX pos )
                ( board !! ( posY pos ) ) ) ) ++
        drop ( posY pos + 1 ) board )
next_board board pos North =
    let ( penguin_could_move, updated_view ) = step $ view board pos North 
    in (
        penguin_could_move,
            transpose (
            take ( posX pos ) ( transpose board ) ++
            nest (
                ( reverse ( init updated_view ) ) ++
                ( drop ( posY pos )
                    ( ( transpose board ) !! ( posX pos ) ) ) ) ++
            drop ( posX pos + 1 ) ( transpose board ) ) )

next_ppos :: Pos -> Dir -> Pos
next_ppos pos East = ( Pos ( posY pos ) ( posX pos + 1 ) )
next_ppos pos South = ( Pos ( posY pos + 1 ) ( posX pos ) )
next_ppos pos West = ( Pos ( posY pos ) ( posX pos - 1 ) )
next_ppos pos North = ( Pos ( posY pos - 1 ) ( posX pos ) )
 
next_world :: World -> Dir-> World 
next_world old_world move_dir =
    let ( can_move, board ) = next_board ( wBoard old_world ) ( wPenguinPos old_world ) ( wPenguinDir old_world )
    in
        if ( move_dir /= wPenguinDir old_world )
        then ( World ( wBoard old_world ) ( wPenguinPos old_world )
                   move_dir ( wHeartCount old_world ) )
        else ( World board 
                   ( next_ppos ( wPenguinPos old_world )
                               ( wPenguinDir old_world ) )
                   ( wPenguinDir old_world )
                   ( wHeartCount old_world ) )

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
                 Edge      -> "###"
             ++ pretty_tiles ts

pretty_board :: Board -> String
pretty_board [] = "" 
pretty_board (ts:tss) = pretty_tiles ts ++ pretty_board tss

pretty_world :: World -> String
pretty_world world =
    "penguin @: " ++ show ( wPenguinPos world ) ++
    ", facing: "  ++ show ( wPenguinDir world ) ++
    ", hearts: "  ++ show ( wHeartCount world ) ++ 
    "\n" ++ pretty_board ( wBoard world )

main :: IO ()
main = do
    putStrLn "ArcticSlide start"
    let world0 = init_world
    putStrLn $ pretty_world world0

    -- 21 East
    let world5  = next_world ( next_world ( next_world ( next_world ( next_world world0  East ) East ) East ) East ) East
    let world10 = next_world ( next_world ( next_world ( next_world ( next_world world5  East ) East ) East ) East ) East
    let world15 = next_world ( next_world ( next_world ( next_world ( next_world world10 East ) East ) East ) East ) East
    let world20 = next_world ( next_world ( next_world ( next_world ( next_world world15 East ) East ) East ) East ) East
    let world21 = next_world world20 East 
    putStrLn $ pretty_world world21
    -- 2 South
    let world23 = next_world ( next_world world21 South ) South
    putStrLn $ pretty_world world23
    -- 3 East
    let world26 = next_world ( next_world ( next_world world23 East ) East ) East
    putStrLn $ pretty_world world26
    -- 2 North
    let world28 = next_world ( next_world world26 North ) North
    putStrLn $ pretty_world world28
    -- 2 West
    let world30 = next_world ( next_world world28 West ) West
    putStrLn $ pretty_world world30
