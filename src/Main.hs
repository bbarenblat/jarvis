-- Copyright © 2015 Benjamin Barenblat
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License.  You may obtain a copy
-- of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
-- License for the specific language governing permissions and limitations under
-- the License.

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.SampleVar
import Control.Monad (forever, void)
import Data.Default.Class (def)
import Graphics.Vty (Vty, (<->), (<|>))
import qualified Graphics.Vty as Vty

main :: IO ()
main = do
  terminal <- Vty.mkVty def
  initialize terminal
  jarvis terminal
  Vty.shutdown terminal

initialize :: Vty -> IO ()
initialize _ = return ()

jarvis :: Vty -> IO ()
jarvis terminal = do
  log <- newSampleVar Vty.emptyImage
  prompt <- newSampleVar Vty.emptyImage
  ready <- newEmptySampleVar
  void $ forkIO $ redrawer ready log prompt terminal
  void $ forkIO $ prompter prompt ready
  void $ forkIO $ logger log ready
  void $ Vty.nextEvent terminal

prompter :: SampleVar Vty.Image -> SampleVar () -> IO ()
prompter output redraw = prompt' 0
  where prompt' n = do
          writeSampleVar output $ Vty.string def ("jarvis " ++ show n ++ " >")
          writeSampleVar redraw ()
          threadDelay 1000000
          prompt' (n + 1)

logger :: SampleVar Vty.Image -> SampleVar () -> IO ()
logger output redraw = log 0 []
  where log n lines = do
          let lines' = lines ++ ["<" ++ show n ++ ">"]
          writeSampleVar output $ Vty.vertCat $ map (Vty.string def) lines'
          writeSampleVar redraw ()
          threadDelay 200000
          log (n + 1) lines'

redrawer :: SampleVar () -> SampleVar Vty.Image -> SampleVar Vty.Image -> Vty -> IO ()
redrawer ready log prompt terminal = forever $ do
  void $ readSampleVar ready
  (_maxX, maxY) <- Vty.displayBounds $ Vty.outputIface terminal
  images <- sequence $ map readSampleVar [log, prompt]
  let stack = resizeHeight' maxY $ Vty.vertCat images
  Vty.update terminal $ Vty.picForImage stack
  Vty.refresh terminal

resizeHeight' :: Int -> Vty.Image -> Vty.Image
resizeHeight' h i = case h `compare` Vty.imageHeight i of
    LT -> Vty.cropTop h i
    EQ -> i
    GT -> Vty.translateY (h - Vty.imageHeight i) i
