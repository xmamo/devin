{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Devin.Parsec (
  module Text.Parsec,
  getOffset,
  toOffset,
  toOffsetT
) where

import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Pos


instance (Num a, Stream s m t) => Stream (a, s) m t where
  uncons :: (a, s) -> m (Maybe (t, (a, s)))
  uncons (offset, stream) = uncons stream >>= \case
    Just (token, rest) -> pure (Just (token, (offset + 1, rest)))
    Nothing -> pure Nothing


getOffset :: Monad m => ParsecT (a, s) u m a
getOffset = do
  State{stateInput = (offset, _)} <- getParserState
  pure offset


toOffset :: (Num a, Stream s Identity Char) => SourcePos -> s -> a
toOffset sourcePos stream = runIdentity (toOffsetT sourcePos stream)


toOffsetT :: (Num a, Stream s m Char) => SourcePos -> s -> m a
toOffsetT sourcePos stream = go 0 (initialPos "") stream
  where
    go result sourcePos' _ | sourcePos' >= sourcePos = pure result

    go result sourcePos' stream = uncons stream >>= \case
      Just (c, rest) -> go (result + 1) (updatePosChar sourcePos' c) rest
      Nothing -> pure result
