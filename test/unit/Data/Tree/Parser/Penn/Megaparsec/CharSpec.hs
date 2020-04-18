{-# LANGUAGE OverloadedStrings #-}

module Data.Tree.Parser.Penn.Megaparsec.CharSpec (
    spec
) where

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
import Control.Monad.Identity

import Data.Void (Void)
import Data.Text (Text)
import Data.Tree 
import Text.Megaparsec

import Data.Tree.Parser.Penn.Megaparsec.Char as TC

parser :: (Monad m) => TC.PennTreeParserT Text m Text
parser = TC.pTree

parserUnsafe :: (Monad m) => TC.PennTreeParserT Text m Text
parserUnsafe = TC.pUnsafeTree

spec :: Spec
spec = do
    describe "pTree" $ do
        it "parses" $ do 
            parseTest parser ""
            parseTest parser "()"
            parseTest parser "( )"
            parseTest parser "A"
            parseTest parser "A B V"
            parseTest parser "A (B V)"
            parseTest parser "(A (B V))"
            parseTest parser "( A (B V))"
            parseTest parser "(A (B C (D E)))"
        it "fails against broken trees" $ do
            parseTest parser "(A (B C (D E)"
    describe "pUnsafeTree" $ do
        it "parses" $ do 
            parseTest parserUnsafe ""
            parseTest parserUnsafe "()"
            parseTest parserUnsafe "( )"
            parseTest parserUnsafe "A"
            parseTest parserUnsafe "A B V"
            parseTest parserUnsafe "A (B V)"
            parseTest parserUnsafe "(A (B V))"
            parseTest parserUnsafe "(A (B C (D E)))"
        it "fails against broken trees" $ do
            parseTest parserUnsafe "(A (B C (D E)"