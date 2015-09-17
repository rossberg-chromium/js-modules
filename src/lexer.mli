(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

val convert_pos : Lexing.position -> Source.pos

val token : Lexing.lexbuf -> Parser.token  (* raises Source.Error *)
