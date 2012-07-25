# A .dircolors preprocessor

The format for `.dircolors` is, by default, a bit rubbish. It's simplistic, sure, but having one pattern per line turns out to be very repetitive and annoying. And the formatting has to be specified as ANSI codes. All in all, not pleasant. This program is a compiler (if you could call it that `:)`) for a nicer, more expressive, format, outputting something that the `dircolors` program can actually understand.

## Features

- Shell-glob-style `[abc]` and `{a,b,c}` forms
- Multiple file types per line
- Human-readable specification of formatting
- Style definitions for more DRY-ness

## Installation

Just `cabal install` it. The only dependency that doesn't come with GHC is [ReadArgs], which is on Hackage of course so that won't pose a problem either.

[ReadArgs]: http://hackage.haskell.org/package/ReadArgs

## Usage

The command-line arguments are up to two filenames: the input and output files.  (If only one is given, it's the input file.) Non-specified ones default to standard in/out. So the most likely invocation is:

    mkdircolors dircolors.in ~/.dircolors

## Syntax of the input file

Tokens are separated by whitespace. Comments start with `#` and extend to the end of the line. Attribute names (colours, `underline`, etc) and section names (`term` and `style`) are case insensitive, but (for now) style names aren't.

    rule ::= glob+ ":" attributes "."
          |  "term" ident+ "."
          |  "style" ident ":" attributes "."

    glob ::= part+

    part ::= branch | single

    branch ::= charclass | brace

    charclass ::=  "[" (single - ("]" | "-"))+ "]"

    brace ::= "{" braceinner "}"

    braceinner
      ::= braceglob "," braceinner
       |  braceglob

    braceglob ::= glob - ("," | "}")

    single ::= lit | esc

    lit ::= any - ("{" | "[" | "#" | ":" | " " | "\")

    esc ::= "\" any

    attributes ::= attribute-list | "reset" | ident

    attribute-list
      ::= attribute "," attribute-list
       |  attribute

    attribute ::= style | color

    style ::= "bold" | "underline" | "blink" | "reverse" | "conceal"

    color ::= ("fg" | "bg") "=" colorname

    colorname ::= "black" | "red" | "green" | "yellow" | "blue"
               |  "magenta" | "cyan" | "white"
