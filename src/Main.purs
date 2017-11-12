module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Foreign (ForeignError)
import Data.List.NonEmpty (NonEmptyList)
import Data.Record (delete, get, insert)
import Simple.JSON (readJSON)
import Type.Prelude (class IsSymbol, class RowLacks, SProxy(..))

rename :: forall prev next ty input inter output
   . IsSymbol prev
  => IsSymbol next
  => RowCons prev ty inter input
  => RowLacks prev inter
  => RowCons next ty inter output
  => RowLacks next inter
  => SProxy prev
  -> SProxy next
  -> Record input
  -> Record output
rename prev next record =
  insert next value inter
  where
    value = get prev record
    inter :: Record inter
    inter = delete prev record

type MyThing =
  { "fieldA" :: String
  , "fieldB" :: Int
  }

decodeMyThingFromDirtyJSON :: String -> Either (NonEmptyList ForeignError) MyThing
decodeMyThingFromDirtyJSON s = do
  parsed <- readJSON s
  pure $ rename
    (SProxy :: SProxy "MY_FIELD_A")
    (SProxy :: SProxy "fieldA")
    parsed

testJSON :: String
testJSON = """
{
  "MY_FIELD_A": "asdf",
  "fieldB": 123
}
"""

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case decodeMyThingFromDirtyJSON testJSON of
    Right {fieldA} -> log $ "fieldA: " <> fieldA
    Left e -> log $ "error: " <> show e
  -- output:
  -- fieldA: asdf
