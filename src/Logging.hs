{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
module Logging where

import Polysemy

data Log m r where
  Log :: Show a => a -> Log m ()

makeSem ''Log

logStdout :: Log m r -> IO r
logStdout (Log x) = putStrLn $ show x

runLogStdout :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
runLogStdout =
  interpret $ embed . logStdout
