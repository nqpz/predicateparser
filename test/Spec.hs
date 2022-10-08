import Items
import PredicateParser (run, fromEvalResult)
import qualified Data.Map as M
import Control.Monad

type ExpectedResult = [(Object, [Predicate])]

test :: String -> ExpectedResult -> IO ()
test input expected =
  let actual = M.toList $ fromEvalResult $ run input
  in when (actual /= expected)
     (fail ("not equal:\nexpected: " ++ show expected ++ "\nactual: " ++ show actual))

main :: IO ()
main = do
  test
    "neighbor lives"
    [(Neighbor, [Lives])]

  test
    "my famous neighbor lives on a boat"
    [ (Neighbor, [On Boat Lives, Famous, My])
    , (Boat, [A])
    ]

  test
    "my door [lives in a house] on the boat"
    [ (Boat, [The])
    , (House, [A])
    , (Door, [On Boat (In House Lives),My])
    ]
