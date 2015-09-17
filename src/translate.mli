(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

val harmony_flag : bool ref  (* produce harmony output *)

val translate : Syntax.prog -> string
