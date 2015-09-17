(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

open Source

type name = (string, unit) phrase

and expr = (expr', Types.typ) phrase
and expr' =
| VarE of name
| DotE of modl * name
| FunE of name list * expr
| AppE of expr * expr list
| StrE of string

and modl = (modl', Types.typ) phrase
and modl' =
| VarM of name
| DotM of modl * name
| LitM of decl
| FromM of prog

and decl = (decl', Types.row) phrase
and decl' =
| EmpD
| SeqD of decl * decl
| RunD of expr
| LetD of name * expr
| ModD of name * modl
| ExpD of name
| ImpD of modl * name
| IncD of modl

and prog = modl


(* Quick hack for recursively invoking loading/parsing *)
let recursive : (string -> prog) ref = ref (fun s -> assert false)
