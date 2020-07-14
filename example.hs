import DrawingTrees

-- A simple tree with the following structure
--       A
--    ┌──┴──┐
--    B     D
--    │  ┌──┼──┐
--    C  E  F  G
exampleTree :: Tree String
exampleTree =
  Node "A" [
    Node "B" [
      Node "C" []
    ],
    Node "D" [
      Node "E" [],
      Node "F" [],
      Node "G" []
    ]
  ]

main = putStrLn $ show $ design exampleTree
