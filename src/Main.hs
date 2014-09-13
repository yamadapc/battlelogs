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
import Control.Monad (liftM, void)
import Data.Time (getCurrentTime, formatTime)
import System.Directory (doesFileExist, getCurrentDirectory,
                        getTemporaryDirectory, removeFile)
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>))
import System.IO (Handle, hClose, hFlush, openTempFile, stdout)
import System.IO.Error (catchIOError)
import System.Locale (defaultTimeLocale)
import System.Process (system)
import System.Console.Docopt (Arguments, command, getArg, isPresent,
                             optionsWithUsage, shortOption)

main :: IO ()
main = do
    args <- getOptions

    if args `isPresent` command "commit"
       then execCommit args
    else if args `isPresent` command "init"
       then execInit
    else if args `isPresent` command "show"
       then execShow
    else printUsage
  where getOptions = getArgs >>= optionsWithUsage usage
        printUsage = putStr usage

usage :: String
usage = unlines [ "Usage:"
                , "      blg init"
                , "      blg commit [-m=<message>]"
                , "      blg show"
                , "      blg [-h]"
                , "Options:"
                , "      -h  Show this help message"
                ]

execInit :: IO ()
execInit = do
    cwd <- getCurrentDirectory
    appendFile (battleLogsPthOf cwd) ""

execCommit :: Arguments -> IO ()
execCommit args = getMessage >>= appendToLog
  where getMessage = if args `isPresent` shortOption 'm'
                         then liftM (++ "\n") (getArg args (shortOption 'm'))
                         else getEditorMessage

execShow :: IO ()
execShow = do
    pagerCmd <- getEnv "PAGER"
    targetPth <- battleLogsPth
    void $ system $ pagerCmd ++ " " ++ targetPth

appendToLog :: String -> IO ()
appendToLog str = do
    header <- getHeader
    targetPth <- battleLogsPth
    appendFile targetPth (header ++ str)

battleLogsPth :: IO FilePath
battleLogsPth =  do
    cwd <- getCurrentDirectory
    let initedPth = battleLogsPthOf cwd
    isInited <- doesFileExist initedPth
    if isInited
        then return initedPth
        else catchIOError (liftM battleLogsPthOf (getEnv "HOME"))
                          (const $ return initedPth)

battleLogsPthOf :: FilePath -> FilePath
battleLogsPthOf = (</> ".battlelogs.md")

getHeader :: IO String
getHeader = liftM formatTime' getCurrentTime
  where formatTime' = formatTime defaultTimeLocale "# %x %X\n"

getEditorMessage :: IO String
getEditorMessage = withSystemTempFile "battlelogs.md" $ \fp _ -> do
    openInEditor fp
    readFile fp

prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

openInEditor :: FilePath -> IO ()
openInEditor fp = do
    editorCmd <- catchIOError (getEnv "EDITOR") (const promptForEditor)
    _ <- system $ editorCmd ++ " " ++ fp
    return ()
  where promptForEditor = prompt "What editor should I use? "

-- From "Real World Haskell"
withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile templ fn = do
    tempdir <- catchIOError getTemporaryDirectory (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir templ
    finally (fn tempfile temph) (hClose temph >> removeFile tempfile)
