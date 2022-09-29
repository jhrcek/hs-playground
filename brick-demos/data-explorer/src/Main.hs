{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Brick (hLimit, on, showFirstCursor, suffixLenses, zoom)
import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main (App (..), defaultMain, halt)
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget, get)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Core (Padding (..), hBox, padBottom, str, txt, vBox, withAttr)
import Brick.Widgets.List qualified as L
import Brick.Widgets.Table (renderTable, table)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Data.Vector qualified as V
import Database.SQLite.Simple (Connection, Only (..), Query (..), SQLData (..), fromOnly, query, query_, withConnection)
import Graphics.Vty (Event (..), Key (..), defAttr)
import Graphics.Vty.Attributes.Color (blue, white)
import Lens.Micro ((^?))
import Lens.Micro.Mtl (preuse, (%=), (.=), (<~))
import System.Environment (getArgs)
import Prelude hiding (lines)

data TableInfo = TableInfo
    { tableName :: Text
    , columnNames :: [Text]
    , rowCount :: Int
    }

data Model = Model
    { tableInfos :: L.List Name TableInfo
    , currentOffset :: Int
    , dbResults :: [[SQLData]]
    }

data Name = TableList deriving (Eq, Ord, Show)

suffixLenses ''Model

main :: IO ()
main = do
    sqliteFile <- parseCli
    withConnection sqliteFile $ \con -> do
        tableNames <- fmap fromOnly <$> query_ con "SELECT name FROM sqlite_master WHERE type ='table' AND name NOT LIKE 'sqlite_%' ORDER BY 1"
        tableInfos <- for tableNames $ \tableName -> do
            columnNames <- fmap fromOnly <$> query con "SELECT name FROM pragma_table_info(?)" (Only tableName)
            rowCount <- fromOnly . head <$> query_ con ("SELECT COALESCE(MAX(RowId),0) FROM " <> coerce tableName)
            pure TableInfo{tableName, columnNames, rowCount}
        let initModel =
                Model
                    { tableInfos = L.list TableList (V.fromList tableInfos) 1 {- no. of rows one list widget row takes up -}
                    , currentOffset = 0
                    , dbResults = []
                    }
        void $ defaultMain (app con) initModel

parseCli :: IO FilePath
parseCli = do
    args <- getArgs
    case args of
        [file] -> pure file
        _ -> error "Usage: brick-data-explorer.hs FILE"

app :: Connection -> App Model e Name
app con =
    App
        { appDraw = view
        , appHandleEvent = update con
        , appStartEvent = return ()
        , appAttrMap = const theMap
        , appChooseCursor = showFirstCursor
        }

view :: Model -> [Widget Name]
view Model{tableInfos, currentOffset, dbResults} =
    [ vBox
        [ padBottom Max $ hBox [viewTableMenu, viewData]
        , hBorder
        , viewHelp
        ]
    ]
  where
    viewTableMenu =
        borderWithLabel (txt "Tables")
            . hLimit maxTableNameWidth
            $ L.renderList viewTableName True tableInfos

    maxTableNameWidth =
        maximum $ fmap (\(TableInfo tableName _ _) -> Text.length tableName) tableInfos

    viewData = case tableInfos ^? L.listSelectedElementL of
        Nothing -> txt "No table selected"
        Just (TableInfo _ columnNames rowCount) ->
            let header = txt <$> columnNames
                dataCells = fmap viewCell <$> dbResults
             in vBox
                    [ renderTable . table $ header : dataCells
                    , str $
                        if null dbResults
                            then "No data"
                            else
                                "Showing rows "
                                    <> show (1 + currentOffset)
                                    <> " - "
                                    <> show (min rowCount (currentOffset + rowsPerPage))
                                    <> " of "
                                    <> show rowCount
                    ]

    viewTableName :: Bool -> TableInfo -> Widget Name
    viewTableName isSelected (TableInfo tn _ _) =
        if isSelected
            then withAttr L.listSelectedAttr $ txt tn
            else txt tn

    viewHelp = txt "esc:Quit  ↑:Prev Table  ↓:Next Table  ←:Prev Page  →:Next Page"

viewCell :: SQLData -> Widget a
viewCell = \case
    SQLInteger i -> str (show i)
    SQLFloat f -> str (show f)
    SQLText t -> txt t
    SQLBlob _ -> txt "BLOB"
    SQLNull -> txt "NULL"

theMap :: AttrMap
theMap =
    attrMap
        defAttr
        [ (L.listAttr, white `on` blue)
        , (L.listSelectedAttr, blue `on` white)
        ]

update :: Connection -> BrickEvent Name e -> EventM Name Model ()
update con = \case
    VtyEvent (EvKey KEsc []) ->
        halt
    VtyEvent (EvKey KLeft []) -> do
        currentOffsetL %= (max 0 . subtract rowsPerPage)
        fetchDbPage
    VtyEvent (EvKey KRight []) -> do
        rowCnt <- maybe 0 rowCount <$> preuse (tableInfosL . L.listSelectedElementL)
        currentOffsetL %= (\offset -> offset + if (offset + rowsPerPage) <= rowCnt then rowsPerPage else 0)
        fetchDbPage
    VtyEvent e -> do
        zoom tableInfosL $ L.handleListEvent e
        currentOffsetL .= 0
        fetchDbPage
    _ -> pure ()
  where
    fetchDbPage = do
        Model{tableInfos, currentOffset} <- get
        for_ (tableInfos ^? L.listSelectedElementL) $ \tableInfo ->
            dbResultsL <~ liftIO (fetchPage con tableInfo currentOffset)

fetchPage :: Connection -> TableInfo -> Int -> IO [[SQLData]]
fetchPage con (TableInfo tableName columnNames _) offset = do
    let colList = Text.intercalate "," (fmap (\c -> Text.snoc (Text.cons '\"' c) '\"') columnNames)
        pageQuery = Query $ "SELECT " <> colList <> " FROM " <> tableName <> " LIMIT ? OFFSET ?"
    query con pageQuery (rowsPerPage, offset)

rowsPerPage :: Int
rowsPerPage = 20