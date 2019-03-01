module Model where

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)

import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign (Foreign, unsafeToForeign, readNumber)
import Foreign as Foreign
import Prelude
import Simple.JSON as JSON
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Time.Duration (Milliseconds(..))

-- TODO REMEMBER TO HAVE A MIGRATION PLAN FOR USERS DATA WHEN UPDATES OCCUR!

type AppState =
  { nextId :: Int
  , textVal :: Maybe String
  , items :: Array Item
  , currentScreen :: Screen
  }
-- TODO convert state to be a made up of a list of changes (event source-y)

-- TODO add more data here: esp date added, date modified
type Item =
  { key :: String
  , val :: String
  , createdAt :: CreatedAtInst
  }

newtype CreatedAtInst = CreatedAtInst Instant

data Screen
  = MenuScreen
  | AddItemScreen
  | DeveloperScreen
  | ViewLogScreen

initialState :: AppState
initialState = { nextId: 0
               , textVal: Nothing
               , items: []
               , currentScreen: MenuScreen
               }

derive instance genericCreatedAtInst :: Generic CreatedAtInst _

instance createdAtInstShow :: Show CreatedAtInst where
  show = genericShow

instance createdAtInstReadForeign :: JSON.ReadForeign CreatedAtInst where
  readImpl fnum = do
    parsed <- readNumber fnum
    let milp = Milliseconds parsed
    let minstant = instant milp
    case minstant of
      Just inst ->
        pure $ CreatedAtInst inst
      Nothing ->
        throwError <<< pure <<< Foreign.ForeignError $ "Could not parse Instant"

instance createdAtInstWriteForeign :: JSON.WriteForeign CreatedAtInst where
  writeImpl (CreatedAtInst inst) =
    let
      Milliseconds milsNum = unInstant inst
    in
     unsafeToForeign milsNum

-- based off of
-- https://github.com/justinwoo/purescript-simple-json/blob/master/test/EnumSumGeneric.purs

enumReadForeign :: forall a rep
   . Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> Foreign.F a
enumReadForeign f =
  to <$> enumReadForeignImpl f

-- type class for "enums", or nullary sum types
class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> Foreign.F rep

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (Sum a b) where
  enumReadForeignImpl f
      = Inl <$> enumReadForeignImpl f
    <|> Inr <$> enumReadForeignImpl f

instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (Constructor name NoArguments) where
  enumReadForeignImpl f = do
    s <- JSON.readImpl f
    if s == name
       then pure $ Constructor NoArguments
       else throwError <<< pure <<< Foreign.ForeignError $
            "Enum string " <> s <> " did not match expected string " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)

derive instance genericScreen :: Generic Screen _

instance screensReadForeign :: JSON.ReadForeign Screen where
  readImpl = enumReadForeign

instance screensWriteForeign :: JSON.WriteForeign Screen where
  writeImpl = unsafeToForeign <<< show

instance screensShow :: Show Screen where
  show = genericShow
