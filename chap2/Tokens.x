{
module Lexer (main) where
}

%wrapper "basic"

$whitespace = [\ \t\b]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]
$letter     = [a-zA-Z]                                       -- alphabetic characters

@identifier = $alpha($alpha|_|$digit)*


state:-

    -- Ignore
    <0> $white+                 ;

    -- Comments
    -- use a counter to count how many levels deep a comment is for every level +1 for exiting every level -1
    -- if counter == 0 then you know the comment is over...

    -- Literals
    -- <0> @string { \s -> String s}

    -- Identifiers
    <0> @identifier { \s -> Identifier s}

    -- Reserved words
    <0> "while"    { \s -> While }
    <0> "for"      { \s -> For   }
    <0> "to"       { \s -> To    }
    <0> "break"    { \s -> Break }
    <0> "let"      { \s -> Let }
    <0> "in"       { \s -> In }
    <0> "end"      { \s -> End }
    <0> "function" { \s -> Function }
    <0> "var"      { \s -> Var }
    <0> "type"     { \s -> Type }
    <0> "array"    { \s -> Array }
    <0> "if"       { \s -> If }
    <0> "then"     { \s -> Then )
    <0> "else"     { \s -> Else }
    <0> "do"       { \s -> Do }
    <0> "of"       { \s -> Of }
    <0> "nil"      { \s -> Nil }

    -- Punctuation symbols
    \, { \s -> Comma }
    \: { \s -> Colon }
    \; { \s -> SemiColon }
    \( { \s -> LParen }
    \) { \s -> RParen }
    \{ { \s -> LBrace }
    \} { \s -> RBrace }
    \. { \s -> Point }
    \+ { \s -> Plus }
    \- { \s -> Minus }
    \* { \s -> Star }
    \/ { \s -> Slash }
    \= { \s -> Eq }
    \<> { \s -> Diamond }
    \< { \s -> LT }
    \<= { \s -> LTQ }
    \> { \s -> GT }
    \>= { \s -> GTQ }
    \& { \s -> And }
    \| { \s -> VLine }
    \:= { \s -> Assign }

{

  data Token = Identifier String
             | String String
             | Int Int
             | While
             | For
             | To
             | Break
             | Let
             | In
             | End
             | Function
             | Var
             | Type
             | Array
             | If
             | Then
             | Else
             | Do
             | Of
             | Nil
             | Comma
             | Colon
             | SemiColon
             | LParen
             | RParen
             | LBrace
             | RBrace
             | Point
             | Plus
             | Minus
             | Star
             | Slash
             | Eq
             | Diamond
             | LT
             | LTQ
             | GT
             | GTQ
             | AND
             | VLine
             | Assign

  main = do
    s <- getContents
    print (alexScanTokens s)

}
