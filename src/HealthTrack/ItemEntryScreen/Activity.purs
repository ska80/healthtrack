module HealthTrack.ItemEntryScreen.Activity where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String as Str
import Effect (Effect)
import HealthTrack.AutoComplete as AC
import HealthTrack.CommonViews as CV
import HealthTrack.ItemEntryScreen.Util as IU
import HealthTrack.Model (ItemEntry(..), Item, ItemName(..), ItemNote(..))
import HealthTrack.ModelUtil as MU
import HealthTrack.Util as Util
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button, string, text, view)

comp :: Component Props
comp = createComponent "AddActivityEntryScreen"

data Action
  = SaveItem
  | ChangeName
  | TextSelected AC.Entry

type Props =
  { onEntryComplete :: ItemEntry -> Effect Unit
  , onEntryUpdate :: Item -> ItemEntry -> Effect Unit
  , key :: String
  , items :: Array Item
  , item :: Maybe Item
  }

type State =
  { itemName :: Maybe String
  , note :: Maybe String
  , changeName :: Boolean
  }

initialState :: Props -> State
initialState props =
  let
    maybeItemEntry :: Maybe ItemEntry
    maybeItemEntry = props.item <#> _.entry
    itemName = maybeItemEntry >>= MU.itemEntryName
    note :: Maybe String
    note = (maybeItemEntry <#> MU.itemEntryNote)
  in
   { itemName
   , note
   , changeName : false
   }

-- TODO finish this?
-- newOrEdit ::
--   (ItemEntry -> Event Unit) ->
--   (ItemEntry -> Item -> Event Unit) ->
--   Props ->
--   ItemEntry ->
--   Event Unit
-- newOrEdit fNew fEdit props itemEntry =
--   case props.item of
--     Just item ->
--       fEdit item.entry item

form :: Props -> JSX
form props = make comp
  { render
  , initialState: IU.initialState props
  } props
  where
    update :: Self Props State -> Action -> StateUpdate Props State
    update self =
      case _ of
        TextSelected entry ->
          let
            nextState = self.state { itemName = Just entry.val
                                   , changeName = false
                                   }
          in
           Update nextState

        ChangeName ->
          Update self.state { changeName = true }


        SaveItem ->
          SideEffects doSaveItem
            where
              doSaveItem :: Self Props State -> Effect Unit
              doSaveItem self' = do
                let
                  -- TODO see if i can make common funcions to extract
                  -- e.g. self.state.itemName into an itemnote
                  -- and reuse everywhere
                  nextName = ItemName $ maybe "" identity self'.state.itemName
                  nextNote = ItemNote $ maybe "" identity self'.state.note
                  nextEntry =
                    ActivityItem $ { name: nextName, note: nextNote }
                maybe (self'.props.onEntryComplete nextEntry)
                      (flip self'.props.onEntryUpdate $ nextEntry)
                      self'.props.item

    send = runUpdate update

    render self =
      let
        renderItem {item} = text { children : [ string "string" ] }

        descriptionAutoComplete =
          AC.autoComplete { onItemSelected: itemSelected
                          , key: "descriptionAutoComplete"
                          , initialEntries: allActivitySuggestions
                          , addCreateEntry: true
                          , handler: inputChangeHandler allActivitySuggestions
                          }

        autoCompWrapped =
          view { key: "DescriptionWrapper"
               , style: css { height: "100%"
                            , width: "100%"
                            }
               , children: [ descriptionAutoComplete ]
               }

        allActivitySuggestions = activitySuggestions self.props.items

        activityTypeText name =
          view { children:
                 [ text { key: "activity label"
                        , children: [ string $ name ]
                        }
                 , button { title: "change"
                          , key: "labelChangeButton"
                          , onPress: capture_ (send self ChangeName)
                          }
                 ]

               }

        itemSelected entry =
          send self $ TextSelected entry

        setNote tv =
          self.setState _ { note = tv }

        doSave =
          send self SaveItem

        activityNameElem :: JSX
        activityNameElem =
          case self.state.itemName of
            Just name | not self.state.changeName ->
              activityTypeText name
            _ ->
              autoCompWrapped
      in
       view { style: css { flexDirection: "column"
                         , padding: 50
                         , width: "100%"
                         , height: "100%"
                         }
            , key: self.props.key
            , children:
              [ text { key: "instructions"
                     , children: [ string "Activity type:" ]
                     }
              , activityNameElem
              -- , maybe autoCompWrapped activityTypeText self.state.itemName

              , CV.notesInput self.state.note setNote doSave

              , button { title: "save"
                       , key: "clickyButton"
                       , onPress: capture_ doSave
                       }
              ]}

inputChangeHandler :: List AC.Entry -> Maybe String -> List AC.Entry -> Int -> Effect AC.Response
inputChangeHandler activitys' mtext _entries nextId =
  pure $ AC.Response (maybe activitys' filterEntries mtext) nextId
  where
    filterEntries :: String -> List AC.Entry
    filterEntries str =
      List.filter (hasStr str) activitys'

    hasStr :: String -> AC.Entry -> Boolean
    hasStr str =
      let
        entryPattern = Str.Pattern (Str.toLower str)
      in
       Str.contains entryPattern <<< Str.toLower <<< _.val

providedActivitys :: Array String
providedActivitys =
  [ "walk"
  , "jog"
  , "resistance training"
  , "yoga"
  , "meditation"
  ]

 -- pull names from all existing item entries for suggestions
activitySuggestions :: Array Item -> List AC.Entry
activitySuggestions items =
  let
    suggestions =
      items <#>
      _.entry #
      MU.activityItemEntryDescriptions #
      Util.filterEmptyStrings #
      Array.nubEq

    allSuggestions = suggestions <> providedActivitys
  in
   AC.arrayToEntries allSuggestions
