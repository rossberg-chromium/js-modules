(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

val resolve: Syntax.prog -> Types.typ  (* raises Source.Error *)
