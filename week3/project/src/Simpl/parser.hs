module Simpl.Parser where

import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
