module MLTT.Parser where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Data.HashSet                 as HS

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Parser.Token.Highlight

import qualified Text.Trifecta.Delta          as Trifecta
import qualified Text.Trifecta.Parser         as Trifecta
import qualified Text.Trifecta.Result         as Trifecta

import           Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import           MLTT.Types

type Parser = Trifecta.Parser

variableStyle :: IdentifierStyle Parser
variableStyle = IdentifierStyle
                { _styleName              = "variable"
                , _styleStart             = letter
                , _styleLetter            = alphaNum
                , _styleReserved          = HS.fromList ["Π", "λ"]
                , _styleHighlight         = Identifier
                , _styleReservedHighlight = ReservedIdentifier }

variableP :: Parser Variable
variableP = StringVar <$> ident variableStyle

referenceP :: Parser Expr
referenceP = Var <$> variableP

universeP :: Parser Expr
universeP = Universe . fromInteger <$> natural

piP :: Parser Expr
piP = do symbol "Π"
         v <- variableP
         colon
         t <- exprP
         dot
         e <- exprP
         return $ Pi $ Abs v t e

lambdaP :: Parser Expr
lambdaP = do symbol "λ"
             v <- variableP
             colon
             t <- exprP
             dot
             e <- exprP
             return $ Lambda $ Abs v t e


appP :: Parser Expr
appP = exprP >> someSpace >> exprP

exprP :: Parser Expr
exprP = choice [ universeP
               , piP
               , lambdaP
               , parens exprP
               , referenceP
               , appP ]

testParseExpr :: (MonadIO m) => String -> m ()
testParseExpr = Trifecta.parseTest exprP

data ParseException = ParseException Doc
                      deriving (Show)

instance Exception ParseException

parseExpr' :: (MonadThrow m) => Trifecta.Delta -> String -> m Expr
parseExpr' delta str = case Trifecta.parseString exprP delta str
                       of Trifecta.Success s -> return s
                          Trifecta.Failure d -> throwM $ ParseException d

parseExpr :: (MonadThrow m) => String -> m Expr
parseExpr = parseExpr' mempty
