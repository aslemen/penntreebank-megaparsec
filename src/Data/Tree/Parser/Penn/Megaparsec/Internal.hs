{-|
    Module          : Data.Tree.Parser.Penn.Megaparsec.Internal
    Description     : The internal module for tree parsers
    Copyright       : (c) 2020 Nori Hayashi
    License         : BSD3
    Maintainer      : Nori Hayashi <net@hayashi-lin.net>
    Stability       : experimental
    Portability     : portable
    Language        : Haskell2010
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Tree.Parser.Penn.Megaparsec.Internal (
    ParsableAsTerm(..),
) where

import Text.Megaparsec

{-|
    A type class for node label types @term@ 
    data of which can be obtained by parsing a stream of type @str@
    that is safely carved by the tree parser
    'Data.Tree.Parser.Penn.Megaparsec.Char.pTree'
    containing no spaces and parenthese.
-}
class (Stream str) => ParsableAsTerm str term where
    {-|
        A parser for non-terminal node labels.
        Empty inputs are expected.
    -}
    pNonTerm :: (Ord err) => ParsecT err str m term

    {-|
        A parser for terminal node labels.
        Empty inputs are not expected.
    -}
    pTerm :: (Ord err) => ParsecT err str m term

instance (Stream str, Tokens str ~ term) 
    => ParsableAsTerm str term where
    pNonTerm = takeRest
    pTerm = takeRest

