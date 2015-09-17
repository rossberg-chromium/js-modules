(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

{
open Parser

type pos = {file : string; line : int; column : int}
type region = {left : pos; right : pos}

let convert_pos pos =
  {
    Source.file = pos.Lexing.pos_fname;
    Source.line = pos.Lexing.pos_lnum;
    Source.column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  }

let region lexbuf =
  let left = convert_pos (Lexing.lexeme_start_p lexbuf) in
  let right = convert_pos (Lexing.lexeme_end_p lexbuf) in 
  {Source.left = left; Source.right = right}

let error lexbuf m = raise (Source.Error (region lexbuf, m))
let error_nest start lexbuf m =
  lexbuf.Lexing.lex_start_p <- start;
  error lexbuf m

let convert_escape = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | '\\' -> '\\'
  | '\'' -> '\''
  | '\"' -> '\"'
  | _ -> assert false

let convert_text s =
  let buf = String.create (String.length s) in
  let i = ref 1 in
  let j = ref 0 in
  while !i < String.length s - 1 do
    buf.[!j] <-
      if s.[!i] <> '\\'
      then s.[!i]
      else (incr i; convert_escape s.[!i]);
    incr i; incr j
  done;
  String.sub buf 0 !j
}

let space = [' ''\t']
let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let symbol = ['+''-''*''/''\\''^''~''=''<''>''!''?''@''#''$''%''&''|'':''`']
let tick = '\''
let escape = ['n''t''\\''\'''\"']
let character = [^'"''\\''\n'] | '\\'escape

let num = digit+
let name = letter (letter | digit | '_')*
let text = '"'character*'"'

rule token = parse
| "export" { EXPORT }
| "from" { FROM }
| "function" { FUNCTION }
| "import" { IMPORT }
| "let" { LET }
| "module" { MODULE }
| "return" { RETURN }
| "=" { EQUAL }
| "->" { ARROW }
| "." { DOT }
| "*" { STAR }
| "(" { LPAR }
| ")" { RPAR }
| "{" { LBRACE }
| "}" { RBRACE }
| "," { COMMA }
| ";" { SEMI }
| name as name { NAME name }
| text as text { STRING (convert_text text) }
| '"'character*('\n'|eof) { error lexbuf "unclosed text literal" }
| '"'character*'\\'_
  { error_nest (Lexing.lexeme_end_p lexbuf) lexbuf "illegal escape character" }
| "////"_*eof { EOF }  (* TODO: this is a temporary hack *)
| "//"[^'\n']*'\n' { Lexing.new_line lexbuf; token lexbuf }
| "//"[^'\n']*eof { Lexing.new_line lexbuf; EOF }
| "/*" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; token lexbuf }
| space { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| eof { EOF }
| _ { error lexbuf "illegal character" }

and comment start = parse
| "*/" { () }
| "/*" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; comment start lexbuf }
| '\n' { Lexing.new_line lexbuf; comment start lexbuf }
| eof { error_nest start lexbuf "unclosed comment" }
| _ { comment start lexbuf }
