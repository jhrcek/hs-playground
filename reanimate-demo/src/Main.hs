module Main where

import Data.Function ((&))
import Reanimate
import Reanimate.Builtin.Documentation (drawCircle, drawProgress)


main :: IO ()
main = reanimate anim


anim :: Animation
anim =
    playThenReverseA $
        addStatic (mkBackground "white") $
            mapA
                ( withFillColor "none"
                    . withStrokeColor "black"
                    . withFillOpacity 0
                )
                rotatingSquare


rotatingSquare :: Animation
rotatingSquare =
    mkAnimation 1 $ \time ->
        mkGroup
            [ rotate (180 * time) $ mkRect 1 1
            ]
