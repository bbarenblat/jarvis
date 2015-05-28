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

module Flag ( Flag
            , new
            , wave
            , wait
            ) where

import Control.Concurrent.MSampleVar

-- | A flag that a thread can wave at another thread.
newtype Flag = Flag (MSampleVar ())

-- | Creates a new flag.
new :: IO Flag
new = fmap Flag newEmptySV

-- | Waves the flag, allowing a thread waiting on the flag to wake up.
wave :: Flag -> IO ()
wave (Flag v) = writeSV v ()

-- | Waits until the flag is waved.
wait :: Flag -> IO ()
wait (Flag v) = readSV v
