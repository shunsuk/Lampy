import Prelude
import Network.CGI
import Text.XHtml
import Control.Monad.Trans
import Data.Maybe
import Data.List
import qualified System.IO.UTF8 as U
import Random
import Text.Printf


------------------------
-- Model
------------------------

-- ランダムなIDを生成
generateId :: IO String
generateId = do
  g <- getStdGen
  return $ concat $ map (\i -> show $ mod i 10) $ take 8 (randoms g :: [Int])

-- 存在するランダムなIDを返す
randomId :: IO String
randomId = do
  cf <- U.readFile ("./data/index")
  g <- getStdGen
  let o = mod (head $ randoms g) (length $ lines cf)
  return $ head $ drop o $ lines cf

-- 作成
create :: String -> IO String
create entry =
  if 0 < length entry
    then do id <- generateId
            writeFile ("./data/" ++ id) entry
            appendFile "./data/index" id
            return id
    else do id <- randomId
            return id

-- 更新
update :: String -> String -> IO String
update id entry = do
  writeFile ("./data/" ++ id) entry
  return id

--  エントリーを取得
readEntry :: String -> IO String
readEntry id = do
  cf <- U.readFile ("./data/" ++ id)
  return cf

-- エントリーのタイトル（1行目）を取得
readTitle :: String -> IO String
readTitle id = do
  cf <- U.readFile ("./data/" ++ id)
  return $ head $ lines cf

-- エントリーの本文（2行目以降）を取得
readBody :: String -> IO String
readBody id = do
  cf <- U.readFile ("./data/" ++ id)
  return $ unlines $ tail $ lines cf

-- エントリー数
entriesLength :: IO Int
entriesLength = do
  cf <- readFile "./data/index"
  return $ length $ lines cf

------------------------
-- View
------------------------

-- <head>
htmlHeader :: String -> Html
htmlHeader t = header
  << [meta ! [httpequiv "content-type", content "text/html; charset=utf-8"],
      thelink ! [rel "stylesheet", thetype "text/css", href "style.css"] << noHtml,
      thetitle << t]

-- ナビの表示部分
navi :: String -> Html
navi id = if length id == 0
             then thediv ! [theclass "navi"]
                << [anchor ! [href "?action=new"] << "Create",
                    anchor ! [href "?action=show"] << "Random"]
             else thediv ! [theclass "navi"]
                << [anchor ! [href ("?action=edit&id=" ++ id)] << "Edit",
                    anchor ! [href "?action=new"] << "Create",
                    anchor ! [href "?action=show"] << "Random"]

-- フッター
footer :: Html
footer = thediv ! [theclass "footer"] << p
  << [anchor ! [href "http://d.hatena.ne.jp/shunsuk/"] << "医者を志す妻を応援する夫の日記",
      br,
      anchor ! [href "http://github.com/shunsuk/Lampy/tree/master"] << "ソースコード"]

-- エントリーの表示部分
entry :: String -> String -> Html
entry t b = thediv ! [theclass "entry"]
  << [p << h1 << t,
      p << (intersperse br $ map stringToHtml $ lines b)]

-- ページ全体のレイアウト
layout :: String -> String -> Html -> Html
layout id t content =  htmlHeader t +++
  body << thediv ! [theclass "main"] << [navi id, content, footer]

-- showビュー
showPage :: String -> String -> String -> Html
showPage id t b = layout id t $ entry t b

-- フォームの表示部分
entryForm :: String -> String -> String -> Html
entryForm id act entry = form ! [action act, method "POST"]
  << [hidden "id" id,
      textarea ! [identifier "area", name "entry"] << entry,
      br,
      submit "" "Write"]

-- newビュー
newPage :: Html
newPage = layout "" "New Page" $ entryForm "" "?action=create" ""

-- editビュー
editPage :: String -> String -> Html
editPage id entry = layout id (head $ lines entry) $ entryForm id "?action=update" entry

------------------------
-- Controller
------------------------
 
cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  act <- getInput "action"
  case act of
       Just "show" -> do
         len <- lift entriesLength
         if 0 < len
            then do
               i <- getInput "id"
               case i of
                    Just id -> do
                      t <- lift $ readTitle id
                      b <- lift $ readBody id
                      output $ renderHtml $ showPage id t b
                    Nothing -> do
                      id <- lift $ randomId
                      redirect ("?action=show&id=" ++ id)
            else do
              redirect "?action=new"
       Just "new"  -> output $ renderHtml newPage
       Just "create"  -> do
         e <- getInput "entry"
         case e of
              Just x -> do
                id <- lift $ create x
                redirect ("?action=show&id=" ++ id)
              Nothing -> do
                redirect "?action=show"
       Just "edit" -> do
         id <- getInput "id"
         case id of
              Just x -> do
                e <- lift $ readEntry x
                output $ renderHtml $ editPage x e
              Nothing -> do
                redirect "?action=new"
       Just "update"  -> do
         e <- getInput "entry"
         id <- getInput "id"
         case e of
              Just x -> do
                case id of
                     Just i -> do
                       lift $ update i x
                       redirect ("?action=show&id=" ++ i)
                     Nothing -> do
                       output $ renderHtml (body << "no")
              Nothing -> do
                output $ renderHtml (body << "no")
       Nothing     -> redirect "?action=show"
       _ -> redirect "?action=show"

------------------------
-- Main
------------------------
 
main :: IO ()
main = runCGI $ handleErrors cgiMain
