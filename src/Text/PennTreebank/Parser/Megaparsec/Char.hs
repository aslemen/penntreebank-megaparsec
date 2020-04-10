{-|
    Module          : Data.Tree.Parser.Penn.Megaparsec
    Description     : Parser combinators that parse Penn Trebank files.
    Copyright       : (c) 2020 Nori Hayashi
    License         : BSD3
    Maintainer      : Nori Hayashi <net@hayashi-lin.net>
    Stability       : experimental
    Portability     : portable
    Language        : Haskell2010
-}

{-# LANGUAGE TypeFamilies #-}

module Text.PennTreebank.Parser.Megaparsec.Char (
    -- * Parser
    pDoc,

    -- * Parser Runners
    -- | Re-imported from "Text.Megaparsec".
    parse,
    parseMaybe,
    parseTest,
    runParser,
    runParser',
    runParserT,
    runParserT'
) where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer as Lex

import Data.Maybe (fromMaybe, catMaybes)
import Data.Tree
import Data.Void (Void)
import qualified Text.Megaparsec.Char as MC
import qualified Data.Tree.Parser.Penn.Megaparsec.Char as TPC

import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Identity (Identity)

data ParserOption = ParserOption {
    ignoreLabelError :: Bool 
}

-- | A parser (monad transformer) that consumes spaces.
spaceConsumer :: (MonadParsec err str m, Token str ~ Char) => m ()
spaceConsumer
    = Lex.space
        MC.space1   -- Space
        empty       -- LineComment
        empty       -- BlockComment

-- | A parser (monad transformer) wrapper to make lexemes separated by spaces.
lexer :: (MonadParsec err str m, Token str ~ Char) 
    => m term -- ^ A lexeme parser to be wrapped
    -> m term -- ^ The parser wrapped by spaces 
lexer = Lex.lexeme spaceConsumer

{-|
    A parser (parser monad transformer) 
        for treebank files in the Penn Treebank format,
        where @err@ is the type of custom errors,
        @str@ is the type of the stream,
        @m@ is the type of the undelying monad and
        @term@ is the type of node labels.

    This parser will do a secondary parse for node labels,
        results of which are of type @term@.
    Failures are registered to the main tree parsing process.
    The secondary node label parser is designated by
        specifying @term@ as an instance of 'Data.Tree.Parser.Penn.Megaparsec.Internal.ParsableAsTerm'.
    
    This parser accepts various types of text stream,
        including 'String', 'Data.Text.Text' and 'Data.Text.Lazy.Text'.
    You might need to manually annotate the type of this parser
        to specify what type of stream you target at in the following way:
    
    > (pDoc :: ParsecT Void Text Identity [Tree Text])
-}
pDoc :: (
        Ord err,
        TPC.ParsableAsTerm str term,
        Monad m,
        Token str ~ Char,
        Tokens str ~ str
    ) => ParsecT err str m [Tree term]
pDoc = do
    MC.space 
    doc <- many $ lexer TPC.pTree
    eof
    return doc
