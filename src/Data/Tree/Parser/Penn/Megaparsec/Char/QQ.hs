{-|
    Module          : Data.Tree.Parser.Penn.Megaparsec
    Description     : Quasiquoters that quotes strings representation of treebank trees.
    Copyright       : (c) 2020 Nori Hayashi
    License         : BSD3
    Maintainer      : Nori Hayashi <net@hayashi-lin.net>
    Stability       : experimental
    Portability     : portable
    Language        : Haskell2010
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Tree.Parser.Penn.Megaparsec.Char.QQ (
    pennTreeQQ
    , penn
    , pennTreeUnsafeQQ
    , pennUnsafe
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Megaparsec
import qualified Text.Megaparsec.Char as MC

import Data.Tree
import Data.Tree.Parser.Penn.Megaparsec.Char

instance (Lift a) => Lift (Tree a) where
    lift (Node root children) = do
        qRoot <- lift root
        qChildren <- mapM lift children
        return $ (ConE 'Node) 
            `AppE` qRoot
            `AppE` (ListE qChildren)

pennTreeQQ :: 
    forall a. (ParsableAsTerm String a, Lift a) 
    => QuasiQuoter
pennTreeQQ = QuasiQuoter
    {
        quoteDec = error "Exp quoter only"
        , quoteExp = expQ
        , quotePat = error "Dec quoter only"
        , quoteType = error "Dec quoter only"
    }
    where
        parser :: PennTreeParser String a
        parser = do
            MC.space
            tree <- pTree
            MC.space
            eof
            return tree
        expQ :: String -> Q Exp
        expQ str = do
            let pres = parse 
                        parser
                        "Haskell QuasiQuote"
                        str 
            case pres of 
                Left errbundle -> do
                    reportError $ errorBundlePretty errbundle
                    return undefined
                Right t -> lift t

penn = pennTreeQQ

pennTreeUnsafeQQ :: 
    forall a. (UnsafelyParsableAsTerm String a, Lift a) 
    => QuasiQuoter
pennTreeUnsafeQQ = QuasiQuoter
    {
        quoteDec = error "Exp quoter only"
        , quoteExp = expQ
        , quotePat = error "Dec quoter only"
        , quoteType = error "Dec quoter only"
    }
    where
        parser :: PennTreeParser String a
        parser = do
            MC.space
            tree <- pUnsafeTree
            MC.space
            eof
            return tree
        expQ :: String -> Q Exp
        expQ str = do
            let pres = parse 
                        parser
                        "Haskell QuasiQuote"
                        str 
            case pres of 
                Left errbundle -> do
                    reportError $ errorBundlePretty errbundle
                    return undefined
                Right t -> lift t

pennUnsafe = pennTreeUnsafeQQ