import PredicateParser
import qualified Data.Map as M
import Control.Monad

type ExpectedResult = [(String, [String])]

test :: String -> ExpectedResult -> IO ()
test input expected =
  let actual = map (\(o, ps) -> (show o, map show ps)) $ M.toList $ fromEvalResult $ run input
  in when (actual /= expected)
     (fail ("not equal:\nexpected: " ++ show expected ++ "\nactual: " ++ show actual))

main :: IO ()
main = do
  test
    "neighbor lives"
    [("Neighbor", ["Lives"])]

  test
    "my famous neighbor lives on a boat"
    [ ("Neighbor", ["On Boat Lives", "Famous", "My"])
    , ("Boat", ["A"])
    ]
