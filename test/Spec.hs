import Items
import PredicateParser (run, fromEvalResult)
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad

type ExpectedResult = [(Object, [Predicate])]

test :: String -> ExpectedResult -> IO ()
test input expected =
  let actual = canonicalize $ M.toList $ fromEvalResult $ run input
      expected' = canonicalize expected
  in when (actual /= expected')
     (fail ("not equal:\nexpected: " ++ show expected' ++ "\nactual: " ++ show actual))
  where canonicalize = L.sort . map (\(o, ps) -> (o, L.sort ps))

main :: IO ()
main = do
  test
    "neighbor lives"
    [(Neighbor, [Lives])]

  test
    "my famous neighbor lives on a boat"
    [ (Neighbor, [ My
                 , Famous
                 , On Boat Lives
                 ])
    , (Boat, [A])
    ]

  test
    "my door [lives in a house] on the boat"
    [ (Door, [ My
             , On Boat (In House Lives)
             ])
    , (Boat, [The])
    , (House, [A])
    ]
