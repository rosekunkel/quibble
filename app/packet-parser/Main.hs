{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Text.Pandoc
import qualified Data.ByteString.Lazy as B
import Data.Default
import Text.Regex.Posix
import Text.RawString.QQ
import Database.PostgreSQL.Simple

isTossup :: Block -> Bool
isTossup ast =
    case ast of
      (Para ((Str s):_)) -> s =~ ("[0-9]+\\." :: String)
      _ -> False

isAnswer :: Block -> Bool
isAnswer ast =
    case ast of
      (Para ((Str s):_)) -> s =~ ("ANSWER:" :: String)
      _ -> False

isTossupOrAnswer :: Block -> Bool
isTossupOrAnswer ast = isTossup ast || isAnswer ast

splitPair :: (Block -> Block -> Bool) -> [Block] -> [[Block]]
splitPair _ [] = []
splitPair _ [x] = [[x]]
splitPair p (x:y:xs) =
    if p x y
    then [x] : (splitPair p (y:xs))
    else case (splitPair p (y:xs)) of
           [] -> undefined
           h:t -> (x:h):t

splitOnTossupBoundary :: [Block] -> [[Block]]
splitOnTossupBoundary = splitPair (\x y -> isTossup y)

isGroupOf2 :: [Block] -> Bool
isGroupOf2 [x, y] = True
isGroupOf2 _ = False

tupleize :: [a] -> (a, a)
tupleize [x, y] = (x, y)

stripLeader :: Block -> Block
stripLeader (Para (_:_:x)) = Para x

plainTextBlocks :: [Block] -> String
plainTextBlocks l = foldl (++) "" $ fmap plainTextBlock l

plainTextBlock :: Block -> String
plainTextBlock (Para l) = foldl (++) "" $ fmap plainTextInline l

plainTextInlines :: [Inline] -> String
plainTextInlines l = foldl (++) "" $ fmap plainTextInline l

plainTextInline :: Inline -> String
plainTextInline inline =
    case inline of
      Str s -> s
      Emph l -> plainTextInlines l
      Strong l -> plainTextInlines l
      Strikeout l -> plainTextInlines l
      Superscript l -> plainTextInlines l
      Subscript l -> plainTextInlines l
      SmallCaps l -> plainTextInlines l
      Quoted SingleQuote l -> "\8216" ++ plainTextInlines l ++ "\8217"
      Quoted DoubleQuote l -> "\8220" ++ plainTextInlines l ++ "\8221"
      Cite _ l -> plainTextInlines l
      Space -> " "
      SoftBreak -> ""
      LineBreak -> "\n"
      RawInline _ s -> s
      Span _ l -> plainTextInlines l

isLineBreak :: Inline -> Bool
isLineBreak SoftBreak = True
isLineBreak LineBreak = True
isLineBreak _ = False

stripLineBreaks :: Block -> Block
stripLineBreaks (Para l) = Para (filter (not . isLineBreak) l)

overlappingPairs :: [a] -> [(a, a)]
overlappingPairs [] = []
overlappingPairs [a] = []
overlappingPairs (x:y:xs) = (x, y):(overlappingPairs (y:xs))

parseClues :: String -> [(Int, Int)]
parseClues s =
    let indices = fmap ((+) 1) $
                  fmap fst $
                  getAllMatches $
                  ((s =~ ("[.!?] |[.!?]\8217 |[.!?]\8221 " :: String))
                  :: AllMatches [] (MatchOffset, MatchLength)) in
    overlappingPairs ([0] ++ indices ++ [length s])

insertQuestion :: Connection -> (String, String) -> IO ()
insertQuestion conn (q, a) = do
  [Only id] <- query conn [r|INSERT INTO questions (content, answer, packet_id)
                             VALUES (?, ?, 2)
                             RETURNING id|]
                             (q, a)
  _ <- executeMany conn [r|INSERT INTO clues (start_index, end_index, question_id)
                           VALUES (?, ?, ?)|]
                             (fmap (\(s, e) -> (s, e, (id :: Int))) (parseClues q))
  return ()

main :: IO ()
main = do
  doc <- B.readFile "packet.docx"
  conn <- connect $ ConnectInfo
          { connectHost = "localhost"
          , connectPort = 5432
          , connectUser = "bitcamp-2016"
          , connectPassword = "bitcamp-2016"
          , connectDatabase = "bitcamp-2016"
          }
  case readDocx def doc of
    Right (Pandoc _ ast, _) ->
        sequence_ $
        fmap (insertQuestion conn) $
        fmap (\(x, y) -> (plainTextBlock x, plainTextBlock y)) $
        fmap (\(x, y) -> (stripLineBreaks x, stripLineBreaks y)) $
        fmap (\(x, y) -> (stripLeader x, stripLeader y)) $
        fmap tupleize $
        filter isGroupOf2 $
        splitOnTossupBoundary $
        filter isTossupOrAnswer ast
        where
          printTossup (h:t) = (putStrLn . show) h >>
                              putStrLn "" >>
                              printTossup t
          printTossup [] = putStrLn ""
    Left error -> putStrLn $ show error
