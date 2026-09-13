-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import qualified Data.Map           as M
-----------------------------------------------------------------------------
import           Miso
import           Miso.FFI.QQ        (js)
import           Miso.JSON          (decode)
import           Miso.Lens
import qualified Miso.Html.Element  as H
import           Miso.Html.Event    (onClick, onInput, onSubmit)
import qualified Miso.Html.Property as P
import           Miso.String        ()
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | Which of the two Web Storage areas a widget operates on.
data Area = Local | Session
  deriving (Eq, Show)
-----------------------------------------------------------------------------
data Model = Model
  { _localItems   :: [(MisoString, MisoString)]
  , _sessionItems :: [(MisoString, MisoString)]
  , _localKey     :: MisoString
  , _localVal     :: MisoString
  , _sessionKey   :: MisoString
  , _sessionVal   :: MisoString
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
localItems, sessionItems :: Lens Model [(MisoString, MisoString)]
localItems   = lens _localItems   $ \m x -> m { _localItems = x }
sessionItems = lens _sessionItems $ \m x -> m { _sessionItems = x }

localKey, localVal, sessionKey, sessionVal :: Lens Model MisoString
localKey   = lens _localKey   $ \m x -> m { _localKey = x }
localVal   = lens _localVal   $ \m x -> m { _localVal = x }
sessionKey = lens _sessionKey $ \m x -> m { _sessionKey = x }
sessionVal = lens _sessionVal $ \m x -> m { _sessionVal = x }
-----------------------------------------------------------------------------
data Action
  = Refresh
  | Refreshed [(MisoString, MisoString)] [(MisoString, MisoString)]
  | SetKey Area MisoString
  | SetVal Area MisoString
  | Add Area
  | Remove Area MisoString
  | ClearAll Area
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model [] [] "" "" "" ""
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { mount = Just Refresh
  , subs = [ storageSub ]
  }
-----------------------------------------------------------------------------
-- | The @storage@ event fires when *another tab* changes storage:
-- open this page twice and watch the tables stay in sync.
storageSub :: Sub model Action
storageSub = windowSub "storage" emptyDecoder (const Refresh)
-----------------------------------------------------------------------------
-- | Snapshot an entire storage area via @JSON.stringify@, then decode it
-- with "Miso.JSON" — every stored value is a string.
localSnapshot :: IO MisoString
localSnapshot = [js| return JSON.stringify(window.localStorage) |]

sessionSnapshot :: IO MisoString
sessionSnapshot = [js| return JSON.stringify(window.sessionStorage) |]
-----------------------------------------------------------------------------
parseItems :: MisoString -> [(MisoString, MisoString)]
parseItems payload =
  maybe [] M.toList (decode payload :: Maybe (M.Map MisoString MisoString))
-----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  Refresh ->
    io $ Refreshed
      <$> (parseItems <$> localSnapshot)
      <*> (parseItems <$> sessionSnapshot)
  Refreshed ls ss -> do
    localItems .= ls
    sessionItems .= ss
  SetKey Local k   -> localKey .= k
  SetKey Session k -> sessionKey .= k
  SetVal Local v   -> localVal .= v
  SetVal Session v -> sessionVal .= v
  Add area -> do
    k <- use (keyLens area)
    v <- use (valLens area)
    keyLens area .= ""
    valLens area .= ""
    if k == ""
      then pure ()
      else io (setter area k v >> pure Refresh)
  Remove area k ->
    io (remover area k >> pure Refresh)
  ClearAll area ->
    io (clearer area >> pure Refresh)
  where
    keyLens Local = localKey
    keyLens Session = sessionKey
    valLens Local = localVal
    valLens Session = sessionVal
    setter Local = setLocalStorage
    setter Session = setSessionStorage
    remover Local = removeLocalStorage
    remover Session = removeSessionStorage
    clearer Local = clearLocalStorage
    clearer Session = clearSessionStorage
-----------------------------------------------------------------------------
viewModel :: Model -> View () () Model Action
viewModel m =
  H.div_
  [ P.class_ "app" ]
  [ H.header_
    [ P.class_ "hero" ]
    [ H.h1_ [] [ "🍜 💾 ", H.a_ [ P.href_ repoUrl ] [ "miso-storage" ] ]
    , H.p_ [ P.class_ "tagline" ]
      [ "The Web Storage API from Haskell: persistent "
      , H.code_ [] [ "localStorage" ]
      , " and per-tab "
      , H.code_ [] [ "sessionStorage" ]
      , ", kept in sync across tabs with a storage-event subscription."
      ]
    , H.a_ [ P.class_ "gh", P.href_ repoUrl ] [ "View source on GitHub" ]
    ]
  , H.main_
    [ P.class_ "grid" ]
    [ areaCard Local "localStorage"
        "Survives reloads and browser restarts. Open this page in a second tab — edits there appear here instantly."
        (m ^. localItems) (m ^. localKey) (m ^. localVal)
    , areaCard Session "sessionStorage"
        "Scoped to this tab; a reload keeps it, a new tab starts empty."
        (m ^. sessionItems) (m ^. sessionKey) (m ^. sessionVal)
    ]
  , H.footer_
    [ P.class_ "foot" ]
    [ H.p_ []
      [ "Built with "
      , H.a_ [ P.href_ "https://github.com/dmjio/miso" ] [ "miso" ]
      , ", a Haskell web framework — compiled to WebAssembly."
      ]
    ]
  ]
  where
    repoUrl = "https://github.com/haskell-miso/miso-storage"
-----------------------------------------------------------------------------
areaCard
  :: Area
  -> MisoString
  -> MisoString
  -> [(MisoString, MisoString)]
  -> MisoString
  -> MisoString
  -> View () () Model Action
areaCard area title hint items keyVal valVal =
  H.section_
  [ P.class_ "card" ]
  [ H.h2_ [] [ H.code_ [] [ text title ] ]
  , H.p_ [ P.class_ "hint" ] [ text hint ]
  , H.form_
    [ P.class_ "add", onSubmit (Add area) ]
    [ H.input_
      [ P.type_ "text"
      , P.placeholder_ "key"
      , P.value_ keyVal
      , onInput (SetKey area)
      ]
    , H.input_
      [ P.type_ "text"
      , P.placeholder_ "value"
      , P.value_ valVal
      , onInput (SetVal area)
      ]
    , H.button_ [ P.class_ "btn primary", P.type_ "submit" ] [ "Add" ]
    ]
  , if null items
      then H.p_ [ P.class_ "empty" ] [ "Nothing stored yet." ]
      else H.table_
        [ P.class_ "items" ]
        [ H.thead_ []
          [ H.tr_ []
            [ H.th_ [] [ "Key" ]
            , H.th_ [] [ "Value" ]
            , H.th_ [] []
            ]
          ]
        , H.tbody_ []
          [ H.tr_ []
            [ H.td_ [] [ H.code_ [] [ text k ] ]
            , H.td_ [] [ text v ]
            , H.td_ []
              [ H.button_
                [ P.class_ "btn danger", P.title_ ("delete " <> k)
                , onClick (Remove area k)
                ]
                [ "✕" ]
              ]
            ]
          | (k, v) <- items
          ]
        ]
  , H.div_
    [ P.class_ "card-foot" ]
    [ H.span_ [ P.class_ "count" ]
      [ text (ms (length items) <> if length items == 1 then " entry" else " entries") ]
    , H.button_ [ P.class_ "btn", onClick (ClearAll area) ] [ "Clear all" ]
    ]
  ]
-----------------------------------------------------------------------------
