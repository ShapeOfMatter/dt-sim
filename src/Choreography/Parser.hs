module Choreography.Parser
where

import Data.Functor.Identity (Identity(..))
import Data.List (intercalate, nub)
import Data.Set (fromList)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.String (Parser)

import Choreography.AbstractSyntaxTree
import Choreography.Functors (Sourced)
import Choreography.Party
import Utils ((<$$>))

positioned :: Parser a -> Parser (Sourced a)
positioned p = do source <- getPosition
                  (source,) <$> p

opNames :: [String]
opNames = [bindKeyword, atKeyword, secretKeyword, flipKeyword, outputKeyword, choiceKeyword]
          <> sendKeywords
          <> oblivKeywords
          <> xorNames
          <> andNames
          <> notNames
          <> macroKeywords
          <> callKeywords

tokenizer :: TokenParser st
tokenizer = makeTokenParser LanguageDef {
  commentStart = "{-",
  commentEnd = "-}",
  commentLine = "--",
  nestedComments = False,
  identStart = letter <|> char '_',
  identLetter = lower <|> digit <|> char '_',
  opStart = upper <|> oneOf (nub $ head <$> opNames),
  opLetter = upper <|> digit  <|> oneOf (nub $ concat opNames),
  reservedNames = trueNames <> falseNames,
  reservedOpNames = opNames,
  caseSensitive = True
}

listSeparator :: Parser ()
listSeparator = whiteSpace tokenizer >> char ',' >> whiteSpace tokenizer

-- Define parser for Party
partyParser :: Parser (Sourced Party)
partyParser = do (source, _) <- positioned $ char '@'
                 party <- Party <$> identifier tokenizer
                 return (source, party)

partiesParser :: Parser (Sourced PartySet)
partiesParser = do let makePartySet = Parties . fromList . (Party <$>)
                   positioned $ makePartySet <$> identifier tokenizer `sepBy1` listSeparator

variableParser :: Parser (Sourced Variable)
variableParser = positioned $ Variable <$> identifier tokenizer

funcNameParser :: Parser (Sourced FuncName)
funcNameParser = positioned $ FuncName <$> identifier tokenizer

pargsParser :: Parser [(Sourced Party, [Sourced Variable])]
pargsParser = parens tokenizer $ pasParser `sepBy` listSeparator
  where pasParser = do fp <- positioned $ Party <$> identifier tokenizer
                       args <- parens tokenizer $ variableParser `sepBy` listSeparator
                       return (fp, args)

-- Define parser for Algebra
algebraParser :: Parser (Sourced (Algebra Sourced))
algebraParser = buildExpressionParser ops terms <?> "Algebra"
  where terms = parens tokenizer algebraParser <|> litParser <|> varParser
        litParser = do (source, b) <- (const (Bit True) <$$> chooseOf reserved trueNames)
                                       <|> (const (Bit False) <$$> chooseOf reserved falseNames)
                       return (source, Literal (source, b))
        varParser = do (loc, var) <- variableParser
                       return (loc, Var (loc, var))
        ops :: OperatorTable String () Identity (Sourced (Algebra Sourced))
        ops = [ [Prefix $ do (isource, _) <- chooseOf reservedOp notNames
                             return \alg -> (isource, Not alg)]
               ,[Infix (do (isource, _) <- chooseOf reservedOp xorNames
                           return $ biOpParser Xor isource) AssocLeft]
               ,[Infix (do (isource, _) <- chooseOf reservedOp andNames
                           return $ biOpParser And isource) AssocLeft] ]
        biOpParser :: (Sourced (Algebra Sourced) -> Sourced (Algebra Sourced) -> Algebra Sourced)
                      -> SourcePos
                      -> Sourced (Algebra Sourced) -> Sourced (Algebra Sourced)
                      -> Sourced (Algebra Sourced)
        biOpParser constructor source alg1 alg2 = (source, constructor alg1 alg2)
        chooseOf cls subcls = choice [positioned . try $ cls tokenizer sc | sc <- subcls] <?> ("one of " ++ intercalate ", " subcls)

-- Define parser for Oblivious terms
obvTransferParser :: Parser (Sourced (ObvBody Sourced))
obvTransferParser = do let branchParser = (ObvLeaf <$$> variableParser)
                                          <|> (ObvBranch <$$> obvTransferParser)
                       (source, [choice0, choice1]) <- positioned
                                                        $ brackets tokenizer
                                                        $ branchParser `sepBy` listSeparator
                       reservedOp tokenizer choiceKeyword
                       selectionVar <- variableParser
                       return (source, ObvBody choice0 choice1 selectionVar)

-- Define parser for Expression
expressionParser :: Parser (Sourced (Statement Sourced))
expressionParser =  sendParser <|> outputParser <|> declarationParser <|> callParser <|> bindingParser
  where
    bindingParser :: Parser (Sourced (Statement Sourced))
    bindingParser = do (source, boundVar) <- positioned $ Variable <$> identifier tokenizer
                       _ <- reservedOp tokenizer bindKeyword
                       (stmt :: Statement Sourced) <- choice [
                          -- Secret Parser:
                          do reservedOp tokenizer secretKeyword
                             Secret (source, boundVar) <$> partyParser,
                          -- Flip Parser:
                          do reservedOp tokenizer flipKeyword
                             Flip (source, boundVar) <$> partyParser,
                          -- Oblivious Transfer Parser:
                          do reservedOp tokenizer (head oblivKeywords)
                             lbody <- obvTransferParser
                             reservedOp tokenizer (oblivKeywords !! 1)
                             lpTo <- partiesParser
                             return (Oblivious (source, boundVar) lpTo lbody),
                          -- Compute Parser:
                          do Compute (source, boundVar) <$> algebraParser
                        ]
                       return (source, stmt)
    sendParser = do (source, _) <- positioned $ reservedOp tokenizer (head sendKeywords)
                    var <- variableParser
                    reservedOp tokenizer (sendKeywords !! 1)
                    lpTo <- partiesParser
                    return (source, Send lpTo var)
    outputParser = do (source, _) <- positioned $ reservedOp tokenizer outputKeyword
                      var <- variableParser
                      return (source, Output var)
    declarationParser = do (source, _) <- positioned $ reservedOp tokenizer $ head macroKeywords
                           ffName <- funcNameParser
                           pargs <- pargsParser
                           reservedOp tokenizer (macroKeywords !! 1)
                           body <- manyTill (whiteSpace tokenizer >> expressionParser)  -- I wish i could just recurse :( ?
                                            (whiteSpace tokenizer >> reservedOp tokenizer (macroKeywords !! 2))
                           return (source, Declaration ffName pargs body)
    callParser = do (source, _) <- positioned $ reservedOp tokenizer $ head callKeywords
                    ffname <- funcNameParser
                    pargs <- pargsParser
                    reservedOp tokenizer (callKeywords !! 1)
                    bindings <- parens tokenizer $ (do
                        v1 <- variableParser
                        reservedOp tokenizer bindKeyword
                        v2 <- variableParser
                        return (v1, v2)
                      ) `sepBy` listSeparator
                    return (source, Call ffname pargs bindings)


-- Define parser for Program
programParser :: Parser (Program Sourced)
programParser = do whiteSpace tokenizer
                   lns :: Program Sourced <- many (do e <- expressionParser
                                                      _ <- whiteSpace tokenizer
                                                      return e)
                   eof
                   return lns
