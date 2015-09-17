(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

type lab = string

and typ = typ' ref
and typ' =
| VarT of typ option
| ValT
| ModT of row

and row = row' ref
and row' =
| VarR of row option
| EmpR
| ExtR of row * lab * typ


val varT : unit -> typ
val valT : typ
val modT : row -> typ

val varR : unit -> row
val empR : row
val extR : row -> lab -> typ -> row
val catR : row -> row -> row

val asModT : typ -> row


module Dom : Set.S with type elt = lab
module Env :
sig  (* Hack for OCaml 3.11, remove if you're running 3.12 *)
  include Map.S with type key = lab
  val singleton : key -> 'a -> 'a t
  val bindings : 'a t -> (key * 'a) list
  val exists : (key -> 'a -> bool) -> 'a t -> bool
end

val normalizeT : typ -> typ
val normalizeR : row -> row option * typ Env.t

val definiteT : typ -> bool
val definiteR : row -> bool

val domT : typ -> Dom.t
val domR : row -> Dom.t
val filterR : Dom.t -> row -> row
val lookupR : lab -> row -> typ


exception Unify of [`Typ of typ * typ | `Row of row * row]

val unifyT : typ -> typ -> unit  (* raises Unify *)
val unifyR : row -> row -> unit  (* raises Unify *)


val string_of_typ : typ -> string
val string_of_row : row -> string
