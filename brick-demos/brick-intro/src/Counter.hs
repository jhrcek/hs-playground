module Counter (main) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Graphics.Vty

main :: IO ()
main = do
    let initState = 1
    endState <- defaultMain app initState
    print endState

type Model = Int

app :: App Model event ()
app =
    App
        { appDraw = draw
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure ()
        , appAttrMap = const styles
        }

handleEvent :: BrickEvent () event -> EventM () Model ()
handleEvent e = case e of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey (KChar '+') []) -> modify (+ 1)
    VtyEvent (EvKey (KChar '-') []) -> modify (subtract 1)
    _ -> pure ()

draw :: Model -> [Widget ()]
draw i =
    [ center $
        vBox
            [ border $ str "+"
            , padLeft (Pad 1) $ str $ show i
            , border $ str "-"
            ]
    ]

styles :: AttrMap
styles = attrMap defAttr []
