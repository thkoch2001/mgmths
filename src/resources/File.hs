-- Copyright (C) 2019 GOOGLE LLC
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

{-# LANGUAGE OverloadedStrings #-}

module File (
  FileResource(..)
            ) where

import Control.Concurrent (threadDelay) -- only used initially for sleep, will go away?
--import qualified Data.Text as T
import Engine (Resource(..), ResourceWatcher(..))
import Filesystem (isFile, writeTextFile)
import Filesystem.Path (FilePath)

data FileResource = FileResource {
  path :: Filesystem.Path.FilePath
}


instance Resource FileResource where
  setupWatcher fr = return ResourceWatcher {
    watch = watchFile fr,
    tearDown = return (),
    checkApply = checkApplyFile fr
                                           }
watchFile :: FileResource -> IO()
watchFile fr = do
  ok <- isFile (path fr)
  if ok
    then do
      threadDelay (500 * 1000)
      watchFile fr
    else return ()

checkApplyFile :: FileResource -> IO ()
checkApplyFile fr =
  if False -- dryRun
    then (return ())
    else do
      ok <- isFile (path fr)
      if ok then return()
        else writeTextFile (path fr) ""

