module Bot.NetHack.WebDiagnostics
  ( runWebDiagnostics )
  where

import Control.Concurrent.STM
import Bot.NetHack.WorldState

runWebDiagnostics :: STM WorldState -> IO ()
runWebDiagnostics world_state_tvar = do
  f
