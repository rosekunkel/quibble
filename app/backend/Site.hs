{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

--------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Util.FileServe
import Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T
import Text.RawString.QQ
import qualified Data.ByteString as BS
import Data.Aeson (ToJSON(toJSON), object, (.=), encode)

--------------------------------------------------------------------------------
import Application

--------------------------------------------------------------------------------
data Clue = Clue
    { tournament :: T.Text
    , year :: Int
    , packet :: Int
    , content :: T.Text
    , answer :: T.Text
    } deriving Show

data RawClue = RawClue
    { rawClueName :: T.Text
    , rawClueYear :: Int
    , rawCluePacket :: Int
    , rawClueContent :: T.Text
    , rawClueAnswer :: T.Text
    , rawClueStartIndex :: Int
    , rawClueEndIndex :: Int
    }

data Question = Question
    { q_tournament :: T.Text
    , q_year :: Int
    , q_packet :: Int
    , q_content :: [T.Text]
    , q_answer :: T.Text
    } deriving Show

instance FromRow RawClue where
    fromRow = RawClue
              <$> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

instance ToJSON Clue where
    toJSON clue = object [ "tournament" .= tournament clue
                         , "year" .= year clue
                         , "packet" .= packet clue
                         , "content" .= content clue
                         , "answer" .= answer clue
                         ]

instance ToJSON Question where
    toJSON question = object [ "tournament" .= q_tournament question
                             , "year" .= q_year question
                             , "packet" .= q_packet question
                             , "content" .= q_content question
                             , "answer" .= q_answer question
                             ]
--------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(BS.ByteString, Handler App App ())]
routes = [ ("",          serveDirectory "/srv/bitcamp-2016/app/frontend/app")
         , ("/clues/random", method GET getRandomClue)
         , ("/questions/random", method GET getRandomQuestion)
         ]

--------------------------------------------------------------------------------
getRandomClue :: Handler App App ()
getRandomClue = do
  modifyResponse $ setContentType "application/json; charset=utf-8"
  randomClue <- query_
                [r|SELECT name, year, packets.number, content,
                          answer, start_index, end_index
                   FROM clues
                   INNER JOIN questions ON question_id = questions.id
                   INNER JOIN packets ON packet_id = packets.id
                   INNER JOIN tournaments ON tournament_id = tournaments.id
                   ORDER BY RANDOM()
                   LIMIT 1|]
  writeLBS $ encode $ rawClueToClue (Prelude.head randomClue)

data RawQuestion = RawQuestion
    { rawq_id :: Int
    , rawq_name :: T.Text
    , rawq_year :: Int
    , rawq_number :: Int
    , rawq_content :: T.Text
    , rawq_answer :: T.Text
    }

instance FromRow RawQuestion where
    fromRow = RawQuestion
              <$> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

data BareClue = BareClue { bc_start :: Int, bc_end :: Int }

instance FromRow BareClue where
    fromRow = BareClue <$> field <*> field

getRandomQuestion :: Handler App App ()
getRandomQuestion = do
  modifyResponse $ setContentType "application/json; charset=utf-8"
  randomQuestion <- query_
                    [r|SELECT questions.id, name, year, number, content, answer
                       FROM questions
                       INNER JOIN packets ON packet_id = packets.id
                       INNER JOIN tournaments ON tournament_id = tournaments.id
                       ORDER BY RANDOM()
                       LIMIT 1|]
  questionClues <- query [r|SELECT start_index, end_index
                            FROM clues
                            WHERE question_id = ?
                            ORDER BY start_index|] (Only (rawq_id $ head randomQuestion))
  let question = Question { q_tournament = rawq_name $ head randomQuestion
                          , q_year = rawq_year $ head randomQuestion
                          , q_packet = rawq_number $ head randomQuestion
                          , q_content = bareClueToClue (rawq_content $ head randomQuestion) <$> questionClues
                          , q_answer = rawq_answer $ head randomQuestion
                          } in writeLBS $ encode question
    where
      bareClueToClue question bc = T.take ((bc_end bc) - (bc_start bc)) $
                                   T.drop (bc_start bc) question

rawClueToClue :: RawClue -> Clue
rawClueToClue rawClue = Clue
  { tournament = rawClueName rawClue
  , year = rawClueYear rawClue
  , packet = rawCluePacket rawClue
  , content = T.take (end - start) $
              T.drop start (rawClueContent rawClue)
  , answer = rawClueAnswer rawClue
  }
    where
      start = rawClueStartIndex rawClue
      end = rawClueEndIndex rawClue

--------------------------------------------------------------------------------
app :: SnapletInit App App
app = makeSnaplet "bitcamp" "Bitcamp 2016 application." Nothing $ do
        pg <- nestSnaplet "pg" pg pgsInit
        addRoutes routes
        return $ App pg

