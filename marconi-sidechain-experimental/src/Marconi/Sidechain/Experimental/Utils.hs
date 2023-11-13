module Marconi.Sidechain.Experimental.Utils where

import Control.Concurrent.STM (STM, TMVar, putTMVar, tryTakeTMVar)

{- | Non-blocking write of a new value to a 'TMVar'
 Puts if empty. Replaces if populated.

 Needs a later version of stm than we're using currently.
 TODO: Remove this function once we no longer need backwards compatibility.
-}
#if !MIN_VERSION_stm(2.5.1)
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar t new = tryTakeTMVar t >> putTMVar t new
#endif```
