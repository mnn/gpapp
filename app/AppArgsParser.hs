{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module AppArgsParser(
  GpArgs(..),
  parseArgs
) where

import System.Console.CmdArgs

data GpArgs = GpArgs
  { json :: Bool
  , number :: Int
  } deriving (Show, Data, Typeable)

gpArgs :: GpArgs
gpArgs = GpArgs
  { json = def &= help "Generate JSON instead of human readable format"
  , number = 10 &= help "Number of posts to fetch"
  } &=
  verbosity

parseArgs :: IO GpArgs
parseArgs = cmdArgs gpArgs
