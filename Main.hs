import System.IO (hPutStr, stderr, hFlush, stdout)
import System.Environment (getArgs, getEnv)
import Database.HDBC
import Database.HDBC.Sqlite3

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["add"] = with_db add
parse ["dump"] = with_db dump
parse [] = with_db pick
parse _ = urge_to_add

pick :: Connection -> IO ()
pick conn = do
  result <- quickQuery' conn "SELECT * FROM htodo WHERE waiting IS NULL LIMIT 1" []
  case result of
      [] -> urge_to_add
      [r] -> focus conn r
      _ -> undefined


focus :: IConnection t => t -> [SqlValue] -> IO ()
focus conn [i,name,SqlNull] = do
  putStrLn $ fromSql name
  putStrLn "  d: done"
  putStrLn "  n: never doing"
  putStrLn "  w: waiting for another task"
  putStr "> "
  hFlush stdout
  command <- getLine
  case command of
    "d" -> remove i conn
    "n" -> remove i conn
    "w" -> add_wait i conn
    _   -> error "command not understood"
focus _ _ = error "should not happen"

remove :: IConnection conn => SqlValue -> conn -> IO ()
remove i conn = do
  _ <- run conn "UPDATE htodo SET waiting = NULL WHERE waiting = ?" [i]
  _ <- run conn "DELETE FROM htodo WHERE id = ?" [i]
  commit conn
  return ()

add_wait :: IConnection conn => SqlValue -> conn -> IO ()
add_wait i conn = do
  dump conn
  putStrLn "after which task?"
  putStr "> "
  hFlush stdout
  number <- getLine
  count <- quickQuery' conn "SELECT * FROM htodo WHERE id = ?" [toSql number]
  if ((count == []) || (toSql number == i)) then
     error "invalid task number specified"
     else do
       _ <- run conn "UPDATE htodo SET waiting = ? WHERE id = ?" [toSql number, i]
       commit conn

dump :: IConnection conn => conn -> IO ()
dump conn = do
  result <- quickQuery' conn "SELECT * FROM htodo ORDER BY id" []
  pp result
    where
      pp [] = return ()
      pp ([i,n,_]:tl) = do
        putStrLn $ fromSql i ++ ": " ++ fromSql n
        pp tl
      pp _ = error "should not happen"

add :: Connection -> IO ()
add conn = do
  putStr "a task to: "
  hFlush stdout
  task <- getLine
  _ <- run conn "INSERT INTO htodo (name, waiting) VALUES (?, ?)" [toSql task, SqlNull]
  commit conn
  return ()

db :: IO String
db = do
  home <- getEnv "HOME"
  return $ home ++ "/.htodo.db"

with_db :: (Connection -> IO ()) -> IO ()
with_db f = do
  conn <- db >>= connectSqlite3
  _ <- run conn schema []
  f conn
  disconnect conn

schema :: String
schema = "CREATE TABLE IF NOT EXISTS htodo (id INTEGER PRIMARY KEY, name TEXT, waiting INTEGER)"

urge_to_add :: IO ()
urge_to_add = do
  hPutStr stderr "usage\n"
  hPutStr stderr "htodo add: add a task\n"
  hPutStr stderr "htodo:     pick a task\n"
