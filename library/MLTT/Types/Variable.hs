{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module MLTT.Types.Variable where

import           Data.Data                    (Data)
import           GHC.Generics                 (Generic)

import           Data.Text                    (Text, unpack)

import           Data.Hashable
import           Text.PrettyPrint.ANSI.Leijen

data Variable = NamedVar !Text
              | GenSym !Text !Integer
              | Dummy
              deriving (Show, Eq, Ord, Generic, Data)

instance Hashable Variable

instance Pretty Variable where
  pretty (NamedVar str) = pretty $ unpack str
  pretty (GenSym str _) = pretty $ unpack str
  pretty Dummy          = "dummy"
