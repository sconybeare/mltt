{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module MLTT.Types.Variable where

import           Data.Data     (Data)
import           GHC.Generics  (Generic)

import           Data.Hashable

data Variable = StringVar String
              | GenSym String Integer
              | Dummy
              deriving (Show, Eq, Ord, Generic, Data)

instance Hashable Variable
