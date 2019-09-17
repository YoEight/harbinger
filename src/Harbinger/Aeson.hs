--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Aeson
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Aeson where

--------------------------------------------------------------------------------
import Data.Aeson.Types (Options, defaultOptions, fieldLabelModifier)

--------------------------------------------------------------------------------
import Harbinger.Common (stripFieldWith)

--------------------------------------------------------------------------------
-- | Overrides the way `aeson` creates a JSON label property based on
--   a record field name.
defaultAesonOptions :: String -> Options
defaultAesonOptions prefix =
    defaultOptions { fieldLabelModifier = stripFieldWith prefix }


