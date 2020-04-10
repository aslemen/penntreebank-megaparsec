# penntreebank-megaparsec :: Megaparsec parsers for trees in the Penn Treebank format

This Haskell package provides parsers for syntactic trees annotated 
    in the Penn Treebank format, powered 
    by [Megaparsec](https://hackage.haskell.org/package/megaparsec).

It supports vertical composition of custom label parsers with tree parsers,
    which means you can customize your label parsers 
    and use these parsers to perform parsing of labels
    at the same time as that of trees.
For the following example of categorial grammar trees,

```
(A/B (B He)
     (B\<A/B> thinks))
```

the result of tree parsing is:

```haskell
Node "A/B" [
    Node "B" [
        Node "He" []
    ], 
    Node "B\<A/B>" [
        Node "thinks" []
    ]
] 
```

which you can make followed by a secondary parsing of labels (= categories):

```haskell
Node (CatRight (Atom B) (Atom A)) [
    Node (Atom B) [
        Node (Lex "He") []
    ], 
    Node (CatLeft (Atom B) (CatRight (Atom B) (Atom A))) [
        Node (Lex "thinks") []
    ]
] 
```

## Usage 
```haskell
import Data.Tree.Parser.Penn.Megaparsec.Char as TC
import Data.Text (Text)
import MegaParsec as MegaP

-- define the node type
data PennNode = NonTerm Text | Term Text

-- define the node parsers
instance ParsableAsTerm Text PennNode where
    pNonTerm = NonTerm <$> MegaP.takeRest
    pTerm = Term <$> MegaP.takeRest

-- specify and disambiguate the type of the input stream 
parser :: (Monad m) => TC.PennTreeParserT Text m PennNode
parser = TC.pTree

main :: IO ()
main = MegaP.parseTest parser "(A (B C (D E)))"

```