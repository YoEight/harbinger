--------------------------------------------------------------------------------
-- |
-- Module    :  Main
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import Prelude hiding (getArgs)

--------------------------------------------------------------------------------
import Harbinger.Command

--------------------------------------------------------------------------------
main :: IO ()
main = getArgs >>= print
