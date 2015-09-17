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


(* Constructors *)

let varT () = ref (VarT None)
let valT = ref ValT
let modT r = ref (ModT r)

let varR () = ref (VarR None)
let empR = ref EmpR
let extR r l t = ref (ExtR (r, l, t))

let rec catR r1 r2 =
  match !r2 with
  | VarR None -> assert false
  | VarR (Some r2') -> catR r1 r2'
  | EmpR -> r1
  | ExtR (r2', l, t) -> extR (catR r1 r2') l t


(* Sets and Maps *)

module Lab = struct type t = lab let compare = compare end
module Dom = Set.Make(Lab)
module Env' = Map.Make(Lab)
module Env =  (* Hack for OCaml 3.11, remove if you're running 3.12 *)
struct
  include Env'
  let singleton x y = add x y empty
  let bindings m = fold (fun x y xs -> (x, y)::xs) m []
  let exists f m = fold (fun x y b -> b || f x y) m false
end


(* Observers *)

let rec normalizeT t =
  match !t with
  | VarT (Some t') -> normalizeT t'
  | _ -> t

let rec normalizeR r =
  match !r with
  | VarR (Some r') -> normalizeR r'
  | VarR None -> Some r, Env.empty
  | EmpR -> None, Env.empty
  | ExtR (r', l, t) ->
    let vo, env = normalizeR r' in
    vo, Env.add l t env


let asModT t =
  match !(normalizeT t) with
  | ModT r -> r
  | _ -> assert false


let rec definiteT t =
  match !t with
  | VarT None -> false
  | VarT (Some t') -> definiteT t'
  | ValT -> true
  | ModT r -> definiteR r

and definiteR r =
  match !r with
  | VarR None -> false
  | VarR (Some r') -> definiteR r'
  | EmpR -> true
  | ExtR (r', l, t) -> definiteR r'


let rec domT t =
  match !t with
  | VarT None -> assert false
  | VarT (Some t') -> domT t'
  | ValT -> assert false
  | ModT r -> domR r

and domR r =
  match !r with
  | VarR None -> assert false
  | VarR (Some r') -> domR r'
  | EmpR -> Dom.empty
  | ExtR (r', l, t) -> Dom.add l (domR r')

let rec filterR dom r =
  match !r with
  | VarR None -> assert false
  | VarR (Some r') -> filterR dom r'
  | EmpR -> r
  | ExtR (r', l, t) ->
    let r'' = filterR dom r' in
    if Dom.mem l dom then extR r'' l t else r''

and lookupR l r =
  match !r with
  | VarR None -> assert false
  | VarR (Some r') -> lookupR l r'
  | EmpR -> assert false
  | ExtR (r', l', t) -> if l = l' then t else lookupR l r'


(* Unification *)

exception Unify of [`Typ of typ * typ | `Row of row * row]

let rec unifyT t1 t2 =
  if t1 == t2 then () else
  match !t1, !t2 with
  | VarT (Some t1'), _ -> unifyT t1' t2
  | _, VarT (Some t2') -> unifyT t1 t2'
  | VarT None, _ -> t1 := VarT (Some t2)
  | _, VarT None -> t2 := VarT (Some t1)
  | ValT, ValT -> ()
  | ModT r1, ModT r2 -> unifyR r1 r2; t1 := VarT (Some t2)
  | _ -> raise (Unify (`Typ (t1, t2)))

and unifyR r1 r2 =
  if r1 == r2 then () else
  match !r1, !r2 with
  | VarR (Some r1'), _ -> unifyR r1' r2
  | _, VarR (Some r2') -> unifyR r1 r2'
  | VarR None, _ -> r1 := VarR (Some r2)
  | _, VarR None -> r2 := VarR (Some r1)
  | EmpR, EmpR -> ()
  | ExtR (r1', l1, t1), ExtR (r2', l2, t2) when l1 = l2 ->
    unifyT t1 t2; unifyR r1' r2'
  | ExtR (r1', l1, t1), ExtR (r2', l2, t2) ->
    let r = varR () in
    unifyR r1' (extR r l2 t2); unifyR r2' (extR r l1 t1)
  | _ -> raise (Unify (`Row (r1, r2)))


(* String conversion *)

let string_depth = 4

let rec string_of_typ' n t =
  match !t with
  | VarT None -> "_"
  | VarT (Some t') -> string_of_typ' n t'
  | ValT -> "V"
  | ModT r -> "{" ^ string_of_row' (n + 1) r ^ "}"

and string_of_row' n r =
  if n > string_depth then "..." else
  let vo, env = normalizeR r in
  let v = if vo = None then [] else ["_"] in
  let ls =
    List.map (fun (l,t) -> l ^ ": " ^ string_of_typ' n t) (Env.bindings env) in
  let s = String.concat ", " (v @ ls) in
  if s = "" then "." else s

let string_of_typ = string_of_typ' 0
let string_of_row = string_of_row' 0
