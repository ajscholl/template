{
module Text.Template.Lexer
    ( lexer
    , Token(..)
    , showTokens
    , SrcPos(..)
    , SrcSpan(..)
    , showSrcPos
    , showSrcSpan
    ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

-- we may not define anything but imports here. alex adds
-- additional imports after this block
}

%wrapper "posn-bytestring"

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
    <0> "("                               { simpleToken TLParen }
    <0> ")"                               { simpleToken TRParen }
    <0> ":"                               { simpleToken TColon }
    <0> ","                               { simpleToken TComma }
    <0> "="                               { simpleToken TEqual }
    <0> "."                               { simpleToken TDot }
    <0> "{{"                              { simpleToken TLBrace2 }
    <0> "}}"                              { simpleToken TRBrace2 }
    <0> "["                               { simpleToken TLBracket }
    <0> "]"                               { simpleToken TRBracket }
    <0> "_"                               { simpleToken TWild }
    <0> "{{" $space* "for"                { simpleToken TFor }
    <0> "in"                              { simpleToken TIn }
    <0> "{{" $space* "if"                 { simpleToken TIf }
    <0> "{{" $space* "else" $space* "}}"  { simpleToken TElse }
    <0> "{{" $space* "else" @spaces "if"  { simpleToken TElseIf }
    <0> "{{" @endIf "}}"                  { simpleToken TEndIf }
    <0> "{{" @endFor "}}"                 { simpleToken TEndFor }
    <0> $nl                               { simpleToken TNewline }
    <0> @spaces                           { TSpaces }
    <0> @id                               { TId }
    <0> @other                            { TChar }

{

----------------------------------------------------------------------------
-- A final code block to conclude the parser definition and expose it to the
-- outside world
----------------------------------------------------------------------------

-- | Run the lexer on the given input string.
lexer :: FilePath -> ByteString -> Either String [Token SrcSpan]
lexer fp bs = go (alexStartPos, '\n', bs, 0)
    where
        go :: AlexInput -> Either String [Token SrcSpan]
        go input@(pos, _, str, n) = case alexScan input 0 of
            AlexEOF                   -> pure []
            AlexError (pos', _, _, _) -> Left $ "lexical error at " <> showSrcPos (mkSrcPos pos') ""
            AlexSkip input' _         -> go input'
            AlexToken input'@(pos', _, _, n') _ token -> do
                xs <- go input'
                let len = n' - n
                pure $ token (SrcSpan fp (mkSrcPos pos) (mkSrcPos pos')) (BL.take len str) : xs

---------------
-- * Data types
---------------

data SrcPos = SrcPos { posLine :: !Int, posColumn :: !Int } deriving Show
data SrcSpan = SrcSpan { spanFile :: !FilePath, spanStart :: !SrcPos, spanEnd :: !SrcPos } deriving Show

mkSrcPos :: AlexPosn -> SrcPos
mkSrcPos (AlexPn _ l c) = SrcPos l c

showSrcPos :: SrcPos -> ShowS
showSrcPos (SrcPos l c) s = shows l $ ':' : shows c s

showSrcSpan :: SrcSpan -> String
showSrcSpan (SrcSpan fp start end) = fp ++ (':' : showSrcPos start (' ' : '-' : ' ' : showSrcPos end ""))

simpleToken :: (a -> b) -> a -> c -> b
simpleToken t p _ = t p

-- | Token type passed on to the parser.
data Token l = TId { tokenLocation :: !l, tokenText :: !ByteString }     -- ^ An identifier for a variable
             | TChar { tokenLocation :: !l, tokenText :: !ByteString }   -- ^ An unrecognized character
             | TSpaces { tokenLocation :: !l, tokenText :: !ByteString } -- ^ Space characters
             | TLParen { tokenLocation :: !l }                           -- ^ '('
             | TRParen { tokenLocation :: !l }                           -- ^ ')'
             | TColon { tokenLocation :: !l }                            -- ^ ':'
             | TComma { tokenLocation :: !l }                            -- ^ ','
             | TEqual { tokenLocation :: !l }                            -- ^ '='
             | TDot { tokenLocation :: !l }                              -- ^ '.'
             | TLBrace2 { tokenLocation :: !l }                          -- ^ '{{'
             | TRBrace2 { tokenLocation :: !l }                          -- ^ '}}'
             | TLBracket { tokenLocation :: !l }                         -- ^ '['
             | TRBracket { tokenLocation :: !l }                         -- ^ ']'
             | TWild { tokenLocation :: !l }                             -- ^ '_'
             | TFor { tokenLocation :: !l }                              -- ^ "{{ for"
             | TIn { tokenLocation :: !l }                               -- ^ "in"
             | TIf { tokenLocation :: !l }                               -- ^ "{{ if"
             | TElse { tokenLocation :: !l }                             -- ^ "{{ else }}"
             | TElseIf { tokenLocation :: !l }                           -- ^ "{{ else if"
             | TEndIf { tokenLocation :: !l }                            -- ^ "{{ end if }}"
             | TEndFor { tokenLocation :: !l }                           -- ^ "{{ end for }}"
             | TNewline { tokenLocation :: !l }                          -- ^ "\n"
             deriving Show

showTokens :: [Token t] -> String
showTokens = show . mconcat . map tokenContent

tokenContent :: Token t -> ByteString
tokenContent t = case t of
    TId _ s     -> s
    TChar _ s   -> s
    TSpaces _ s -> s
    TLParen _   -> fromString "("
    TRParen _   -> fromString ")"
    TColon _    -> fromString ":"
    TComma _    -> fromString ","
    TEqual _    -> fromString "="
    TDot _      -> fromString "."
    TLBrace2 _  -> fromString "{{"
    TRBrace2 _  -> fromString "}}"
    TLBracket _ -> fromString "["
    TRBracket _ -> fromString "]"
    TWild _     -> fromString "_"
    TFor _      -> fromString "{{ for"
    TIn _       -> fromString "in"
    TIf _       -> fromString "{{ if"
    TElse _     -> fromString "{{ else }}"
    TElseIf _   -> fromString "{{ else if"
    TEndIf _    -> fromString "{{ end if }}"
    TEndFor _   -> fromString "{{ end for }}"
    TNewline _  -> fromString "\n"

}
