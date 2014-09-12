-- A minimal personal journal system.
-- Copyright (C) 2014 Pedro Tacla Yamada
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

import Control.Exception (finally)
import Control.Monad (liftM)
import Data.Time (getCurrentTime, formatTime)
import System.Directory (getCurrentDirectory, getTemporaryDirectory, removeFile)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO -- (SeekMode(..), Handle, hClose, hFlush, hGetContents,
                 -- hPutStrLn, hSeek, openTempFile, stdout)
import System.IO.Error (catchIOError)
import System.Locale (defaultTimeLocale)
import System.Process (system)


main :: IO ()
main = withSystemTempFile "battleLogs.md" $ \fp _ -> do
    -- Capture output from the standard editor
    openInEditor fp

    -- Append entry to battlelogs
    fh <- openFile fp ReadMode
    header <- getHeader
    c <- hGetContents fh
    targetPth <- battleLogsPth

    targetFh <- openFile targetPth AppendMode
    hPutStrLn targetFh (header ++ c)

    -- Clean-up
    hClose fh
    hClose targetFh

battleLogsPth :: IO FilePath
battleLogsPth =  liftM logsPthOf logsDir
    where logsDir = catchIOError (getEnv "HOME") (const getCurrentDirectory)
          logsPthOf = (</> ".battlelogs.md")


openInEditor :: FilePath -> IO ()
openInEditor fp = do
    editorCmd <- catchIOError (getEnv "EDITOR") (const promptForEditor)
    _ <- system $ editorCmd ++ " " ++ fp
    return ()
  where promptForEditor = prompt "What editor should I use? "

prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

getHeader :: IO String
getHeader = liftM formatTime' getCurrentTime
  where formatTime' = formatTime defaultTimeLocale "%x %X\n"

-- From "Real World Haskell"
withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile templ fn = do
    tempdir <- catchIOError getTemporaryDirectory (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir templ
    finally (fn tempfile temph) (hClose temph >> removeFile tempfile)
