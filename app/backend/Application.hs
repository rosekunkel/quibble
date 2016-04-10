{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Application where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Snap
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

data App = App
    { _pg :: Snaplet Postgres }

makeLenses ''App

instance HasPostgres (Handler b App) where
    getPostgresState = with pg get
    setLocalPostgresState s = local (set (pg . snapletValue) s)
