module HelloWorld where

import Brick

main :: IO ()
main = simpleMain ui

ui :: Widget ()
ui = str "Hello, World!"
