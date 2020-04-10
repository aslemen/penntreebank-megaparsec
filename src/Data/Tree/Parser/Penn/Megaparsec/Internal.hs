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
    ParsableAsTerm(..)
) where

import Data.Monoid
import Text.Megaparsec

{-|
    A type class for node label types 'term' 
    data of which can be obtained by parsing a stream of type 'str'.
-}
class (Stream str, Monoid term) => ParsableAsTerm str term where
    {-|
        A parser that extracts exactly one token 
        of the node label type 'term' from an input stream of type 'str'.
    -}
    pNonTerm :: (Ord err) => ParsecT err str m term
    pTerm :: (Ord err) => ParsecT err str m term

instance (Stream str, Monoid term, Tokens str ~ term) 
    => ParsableAsTerm str term where
    pNonTerm = takeRest
    pTerm = takeRest