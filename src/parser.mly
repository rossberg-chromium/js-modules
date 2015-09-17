/*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 */

%{
open Source
open Syntax

let position_to_pos position =
  {
    file = position.Lexing.pos_fname;
    line = position.Lexing.pos_lnum;
    column = position.Lexing.pos_cnum - position.Lexing.pos_bol
  }

let positions_to_region position1 position2 =
  {
    left = position_to_pos position1;
    right = position_to_pos position2
  }

let at () =
  positions_to_region (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())
let ati i =
  positions_to_region (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)

let parse_error s = raise (Source.Error (Source.nowhere_region, s))
%}

%token LET MODULE
%token IMPORT EXPORT
%token FROM
%token FUNCTION RETURN

%token EQUAL ARROW
%token LPAR RPAR
%token LBRACE RBRACE
%token DOT STAR
%token COMMA SEMI

%token EOF

%token<string> NAME
%token<string> STRING

%start prog
%type<Syntax.prog> prog

%%

name :
| NAME
  { $1@@at() }
;
name_list :
| 
  { [] }
| name_list1
  { $1 }
;
name_list1 :
| name
  { $1::[] }
| name COMMA name_list1
  { $1::$3 }
;

expr :
| name
  { VarE($1)@@at() }
| path DOT name
  { DotE($1, $3)@@at() }
| STRING
  { StrE($1)@@at() }
| expr LPAR expr_list RPAR
  { AppE($1, $3)@@at() }
| FUNCTION LPAR name_list RPAR LBRACE RETURN expr RBRACE
  { FunE($3, $7)@@at() }
;
expr_list :
| 
  { [] }
| expr_list1
  { $1 }
;
expr_list1 :
| expr
  { $1::[] }
| expr COMMA expr_list1
  { $1::$3 }
;

path :
| name
  { VarM($1)@@at() }
| path DOT name
  { DotM($1, $3)@@at() }
;

modl :
| path
  { $1 }
| LBRACE decl RBRACE
  { LitM($2)@@at() }
| FROM STRING
  { FromM(!Syntax.recursive $2)@@at() }
;

decl1 :
| expr
  { RunD($1)@@at() }
| LET name EQUAL expr
  { LetD($2, $4)@@at() }
| MODULE name EQUAL modl
  { ModD($2, $4)@@at() }
| EXPORT name
  { ExpD($2)@@at() }
| EXPORT name COMMA name_list1
  { List.fold_left (fun d x -> SeqD(d, ExpD(x)@@at())@@at())
      (ExpD($2)@@at()) $4 }
| EXPORT LET name EQUAL expr
  { SeqD(ExpD($3)@@at(), LetD($3, $5)@@at())@@at() }
| EXPORT MODULE name EQUAL modl
  { SeqD(ExpD($3)@@at(), ModD($3, $5)@@at())@@at() }
| IMPORT path DOT name
  { ImpD($2, $4)@@at() }
| IMPORT path DOT STAR
  { IncD($2)@@at() }
;
decl :
|
  { EmpD@@at() }
| decl1 SEMI decl
  { SeqD($1, $3)@@at() }
| decl1 decl
  { SeqD($1, $2)@@at() }
;

prog :
| decl EOF
  { LitM($1)@@at() }
;

%%
  