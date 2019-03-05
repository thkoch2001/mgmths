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

module Engine (
    Resource(..)
  , ResourceWatcher(..)
  , runResource
              ) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar (newTVar, readTVar, readTVarIO, TVar, writeTVar)
import Control.Monad (forever, unless)

data ResourceWatcher = ResourceWatcher {
  watch :: IO(),
  tearDown :: IO(),
  checkApply :: IO ()
}

class Resource r where
  setupWatcher :: r -> IO ResourceWatcher
--  uniqueId :: r -> String


runResource :: Resource r => r -> IO()
runResource resource = do
  needsChecking <- atomically $ newTVar True
  watcher <- setupWatcher resource
  thread1 <- async $ runResourceWatcher needsChecking (watch watcher)
  _ <- async $ runCheckApply needsChecking (checkApply watcher)
  _ <- wait thread1
  return ()

runResourceWatcher :: TVar Bool -> IO() -> IO()
runResourceWatcher needsChecking w = forever $ do
  w
  currentNeedsChecking <- readTVarIO needsChecking
  unless currentNeedsChecking $ atomically $ writeTVar needsChecking True

runCheckApply :: TVar Bool -> IO() -> IO()
runCheckApply needsChecking ca = forever $ do
  _ <- atomically $ do
    currentNeedsChecking <- readTVar needsChecking
    if currentNeedsChecking
      then writeTVar needsChecking False
      else retry
  ca
