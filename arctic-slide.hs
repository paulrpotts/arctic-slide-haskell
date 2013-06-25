doubleMe x = x + x

data Tile = Empty | Tree | Mountain | House | Ice_Block | Bomb | Heart | Edge
    deriving (Show)

class Walkable a where
    walkable :: a -> Bool

instance Walkable Tile where
    walkable Empty = True
    walkable Tree = True
    walkable _ = False
 
-- data Walkable = anEmpty Empty | aTree Tree
-- data Blocking = aTree Tree | aMountain Mountain | aHouse House
--    | anIce_Block Ice_Block | aBomb Bomb | aHeart Heart | anEdge Edge

slide :: [Tile] -> [Tile]
slide [] = error "slide empty list!"
slide (t) = error "single item list!" 
slide (Empty:ts) = ts ++ slide ts

traverse :: [Tile] -> [Tile]
traverse [] = error "traverse empty list!" 
traverse [Edge] = [Edge]
traverse (Empty:ts) = ts
traverse (Bomb:Heart:ts) = [Empty, Empty] ++ ts 
traverse (Heart:House:ts) = [Empty, House] ++ ts

step :: [Tile] -> Bool
step [] = error "penguinStep: empty list!"
step (t:_) = if walkable t then True else False 

main :: IO ()
main = do
   let tiles = [Empty, Empty, Empty, Mountain]
   putStrLn "ArcticSlide start"

