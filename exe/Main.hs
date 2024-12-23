module Main where

import BattleShip

main :: IO ()
main = do
  -- 1 x 4
  -- 2 x 3
  -- 3 x 2
  -- 4 x 1

  let secretBoard = BoardSecret { board = [ Ship 1 0 1 4
                                          , Ship 4 4 4 6, Ship 7 5 7 7
                                          , Ship 4 2 5 2, Ship 0 6 1 6 --, Ship 7 1 7 2
                                          -- , Ship 0 7 0 7, Ship 1 5 1 5, Ship 7 0 7 0, Ship 9 0 9 0
                                          ]
                                , history = []
                                }
  putStrLn "Solving..."
  sinkAllShips secretBoard 8 8
