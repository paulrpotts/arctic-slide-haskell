{-----------------------------------
wxHaskell GUI for arctic-slide; lay
out a 4x24 grid with 4 controls for
penguin movement and support for
arrow keys. Based on layout demo.
-----------------------------------}

module Main where

import Control.Monad
import Graphics.UI.WX
import ArcticSlidePure

bomb = bitmap "bomb.png"
heart = bitmap "heart.png"
house = bitmap "house.png"
ice = bitmap "ice_block.png"
tree = bitmap "tree.png"

penguin_e = bitmap "penguin_east.png"
penguin_s = bitmap "penguin_south.png"
penguin_w = bitmap "penguin_west.png"
penguin_n = bitmap "penguin_north.png"

ice_break = bitmap "ice_block_breaking.png"
bomb_explode = bitmap "bomb_exploding.png"

posToPoint :: Pos -> Point
posToPoint pos = ( Point ( posX pos * 24 ) ( posY pos * 24 ) )

drawBmp dc bmp pos = drawBitmap dc bmp point True []
    where point = posToPoint pos

drawTile dc tile pos = drawBmp dc bmp pos
    where bmp = case tile of Bomb  -> bomb
                             Heart -> heart
                             House -> house
                             Ice   -> ice
                             Tree  -> tree 

draw dc view
    = do
        drawTile dc Bomb        ( Pos 0 0  )
        drawTile dc Heart       ( Pos 0 1  )
        drawTile dc House       ( Pos 0 2  )
        drawTile dc Ice         ( Pos 0 3  )
        drawTile dc Tree        ( Pos 0 4  )
        drawBmp dc penguin_e    ( Pos 1 0  )
        drawBmp dc penguin_s    ( Pos 1 1  )
        drawBmp dc penguin_w    ( Pos 1 2  )
        drawBmp dc penguin_n    ( Pos 1 3  )
        drawBmp dc ice_break    ( Pos 0 23 )
        drawBmp dc bomb_explode ( Pos 3 23 )

main :: IO ()
main
  = start gui

gui :: IO ()
gui
    = do f <- frame [text := "Arctic Slide"]
         t <- timer f [ interval := 250
                       -- ,on command := nothing
                      ]
         set f [ layout      := grid 0 0 $ take 4 $ repeat $ take 24 $ repeat $ space 24 24 
                ,bgcolor    := white
                ,on paint   := draw
                -- ,on leftKey :=
                -- ,on rightKey :=
                -- ,on upKey :=
                -- ,on downKey :=
               ]
         return ()
