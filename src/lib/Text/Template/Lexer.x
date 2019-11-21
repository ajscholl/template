{
module Text.Template.Lexer (lexer, Token(..), showTokens) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

-- we may not define anything but imports here. alex adds
-- additional imports after this block
}

%wrapper "basic-bytestring"

----------------------------------------------
-- Define some macros for commonly used things
----------------------------------------------

-- Whitespace characters
-- We convert \r\n and \n\r to \n, but otherwise leave newlines unchanged,
-- so we better match on all "normal" newlines. We do not match on Unicode
-- newlines right now
$nl    = [\n\r\f]
$nows  = [^\n\r\f\v ]
-- any character which is a valid space character
$space  = [\v\ ]

$digit = 0-9

-- alphabetic characters
$large = [A-Z]
$small = [a-z \_]

$idchar  = [$small $large $digit \']

-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

-- variable identifiers
@id = [$small $large] $idchar*

@spaces = $space $space*

@endIf = $space* "end" $space* "if" $space*
@endFor = $space* "end" $space* "for" $space*

@other = $nows

------------------------
-- Now define our tokens
------------------------

tokens :-
    -- we need all special names first, so the lexer matches them as tokens instead of names/symbols
    <0> "("                               { const TLParen }
    <0> ")"                               { const TRParen }
    <0> ":"                               { const TColon }
    <0> ","                               { const TComma }
    <0> "="                               { const TEqual }
    <0> "."                               { const TDot }
    <0> "{{"                              { const TLBrace2 }
    <0> "}}"                              { const TRBrace2 }
    <0> "["                               { const TLBracket }
    <0> "]"                               { const TRBracket }
    <0> "_"                               { const TWild }
    <0> "{{" $space* "for"                { const TFor }
    <0> "in"                              { const TIn }
    <0> "{{" $space* "if"                 { const TIf }
    <0> "{{" $space* "else" $space* "}}"  { const TElse }
    <0> "{{" $space* "else" @spaces "if"  { const TElseIf }
    <0> "{{" @endIf "}}"                  { const TEndIf }
    <0> "{{" @endFor "}}"                 { const TEndFor }
    <0> $nl                               { const TNewline }
    <0> @spaces                           { TSpaces }
    <0> @id                               { TId }
    <0> @other                            { TChar }

{

----------------------------------------------------------------------------
-- A final code block to conclude the parser definition and expose it to the
-- outside world
----------------------------------------------------------------------------

-- | Run the lexer on the given input string.
lexer :: ByteString -> Either String [Token]
lexer str = go (AlexInput '\n' str 0)
    where
        go input = case alexScan input 0 of
            AlexEOF                  -> pure []
            AlexError input'         -> Left $ "parse error at offset " <> show (alexBytePos input')
            AlexSkip input' _        -> go input'
            AlexToken input' _ token -> do
                xs <- go input'
                let len = alexBytePos input' - alexBytePos input
                pure $ token (BL.take len (alexStr input)) : xs

---------------
-- * Data types
---------------

-- | Token type passed on to the parser.
data Token = TId !ByteString     -- ^ An identifier for a variable
           | TChar !ByteString   -- ^ An unrecognized character
           | TSpaces !ByteString -- ^ Space characters
           | TLParen             -- ^ '('
           | TRParen             -- ^ ')'
           | TColon              -- ^ ':'
           | TComma              -- ^ ','
           | TEqual              -- ^ '='
           | TDot                -- ^ '.'
           | TLBrace2            -- ^ '{{'
           | TRBrace2            -- ^ '}}'
           | TLBracket           -- ^ '['
           | TRBracket           -- ^ ']'
           | TWild               -- ^ '_'
           | TFor                -- ^ "{{ for"
           | TIn                 -- ^ "in"
           | TIf                 -- ^ "{{ if"
           | TElse               -- ^ "{{ else }}"
           | TElseIf             -- ^ "{{ else if"
           | TEndIf              -- ^ "{{ end if }}"
           | TEndFor             -- ^ "{{ end for }}"
           | TNewline            -- ^ "\n"
           deriving Show

showTokens :: [Token] -> String
showTokens = show . mconcat . map tokenContent

tokenContent :: Token -> ByteString
tokenContent t = case t of
    TId s               -> s
    TChar s             -> s
    TSpaces s           -> s
    TLParen             -> fromString "("
    TRParen             -> fromString ")"
    TColon              -> fromString ":"
    TComma              -> fromString ","
    TEqual              -> fromString "="
    TDot                -> fromString "."
    TLBrace2            -> fromString "{{"
    TRBrace2            -> fromString "}}"
    TLBracket           -> fromString "["
    TRBracket           -> fromString "]"
    TWild               -> fromString "_"
    TFor                -> fromString "{{ for"
    TIn                 -> fromString "in"
    TIf                 -> fromString "{{ if"
    TElse               -> fromString "{{ else }}"
    TElseIf             -> fromString "{{ else if"
    TEndIf              -> fromString "{{ end if }}"
    TEndFor             -> fromString "{{ end for }}"
    TNewline            -> fromString "\n"

}
