{-|
    Module          : Data.Tree.Parser.Penn.Megaparsec
    Description     : Parser combinators that parse 'Data.Text' 
                        or 'String' streams to generate Penn Treebank trees.
    Copyright       : (c) 2020 Nori Hayashi
    License         : BSD3
    Maintainer      : Nori Hayashi <net@hayashi-lin.net>
    Stability       : experimental
    Portability     : portable
    Language        : Haskell2010
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.Parser.Penn.Megaparsec.Char (
    -- * Parsers
    pTree,
    ParsableAsTerm(..),

    -- * Parser Type Synonyms
    PennTreeParserT,
    PennTreeParser,

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

import Data.Char as DCh
import Data.Tree
import Data.Void (Void)
import Data.Proxy (Proxy(..))

import Text.Megaparsec
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as Lex

import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Identity (Identity)

import Data.Tree.Parser.Penn.Megaparsec.Internal
    
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

-- | A parser (monad transformer) wrapper that parenthesize the given parser.
pParens :: (
    MonadParsec err str m, 
    Token str ~ Char,
    Tokens str ~ str
    ) => m term -- ^ A parser to be wrapped
    -> m term   -- ^ The parser wrapped by parentheses
pParens = 
    between
        (lexer $ single '(')
        (lexer $ single ')')

-- | A parser for raw node labels.
invokeLabelParserRaw :: (MonadParsec err str m, Token str ~ Char)  
    => m (Tokens str)
invokeLabelParserRaw 
    = takeWhile1P
        (Just "Literal String")
        (\x -> x /= '{' && x /= '(' && x /= ')' && not (DCh.isSpace x))

{-|
    A vertial parser (monad transformer) compositor that 
    make raw node labels get a secondary parse
    by another parser.
-}
invokeLabelParser :: (
        Ord err,
        ParsableAsTerm str term,
        Monad m,
        Token str ~ Char,
        Tokens str ~ str
    ) => ParsecT err str m term -- ^ A secondary parser for node labels
    -> State str err            -- ^ The state given by the main parsing thread
    -> ParsecT err str m term   -- ^ The resulting parser that is going to be embedded in the main thread 
invokeLabelParser labelParser substate = do
    (_, res) <- lift $ runParserT' (labelParser <* eof) substate
    case res of
        Left b@(ParseErrorBundle errors _) -> do
            forM_ errors registerParseError
            return undefined
        Right label -> return label
{-|
    A parser (parser monad transformer) for trees in the Penn Treebank format,
        where @err@ is the type of custom errors,
        @str@ is the type of the stream,
        @m@ is the type of the undelying monad and
        @term@ is the type of node labels.

    This parser will do a secondary parse for node labels (of type @term@).
    The secondary node label parser is designated by
        specifying the type @term@ as an instance of 'ParsableAsTerm'.
    
    This parser accepts various types of text stream,
        including 'String', 'Data.Text.Text' and 'Data.Text.Lazy.Text'.
    You might need to manually annotate the type of this parser
        to specify what type of stream you target at in the following way:
    
    > (pTree :: ParsecT Void Text Identity (Tree Text))
-}

pTree ::
    (
        Ord err,
        ParsableAsTerm str term,
        Monad m,
        Token str ~ Char,
        Tokens str ~ str
    ) => ParsecT err str m (Tree term)
pTree
    = pParens pTreeInside <|> pTerminalNode <?> "Parsed Tree"
    where 
        pTreeInside :: forall str. forall err. forall m. forall term.  (
                Ord err,
                ParsableAsTerm str term,
                Monad m,
                Token str ~ Char,
                Tokens str ~ str
            ) => ParsecT err str m (Tree term)
        pTreeInside = do
            state <- getParserState
            maybeLabelRaw <- lexer $ optional invokeLabelParserRaw
            let substate = state {
                stateInput 
                    = case maybeLabelRaw of 
                        Just labelRaw -> labelRaw
                        Nothing -> tokensToChunk pxy []
                ,
                stateOffset = 0
            }
            labelParsed <- invokeLabelParser pNonTerm substate
            children <- many $ lexer pTree -- Recursion
            return $ Node labelParsed children
            where
                pxy :: Proxy str
                pxy = Proxy
            
        pTerminalNode :: (
                Ord err,
                ParsableAsTerm str term,
                Monad m,
                Token str ~ Char,
                Tokens str ~ str
            ) => ParsecT err str m (Tree term)
        pTerminalNode = do
            state <- getParserState
            labelRaw <- lexer $ invokeLabelParserRaw
            labelParsed <- do
                let substate = state {
                    stateInput = labelRaw,
                    stateOffset = 0
                }
                invokeLabelParser pTerm substate
            return $ Node labelParsed []

type PennTreeParserT str m term = ParsecT Void str m (Tree term)
type PennTreeParser str term = PennTreeParserT str Identity term
