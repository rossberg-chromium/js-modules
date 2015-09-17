(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

open Source
open Syntax
open Types


let harmony_flag = ref false

let env0 = Env.singleton "print" "print"


(* Name mangling for internal variables *)

let mangle p =
  let x = String.copy p in
  for i = 0 to String.length x - 1 do
    if x.[i] = '.' then x.[i] <- '_'
  done;
  x


(* Generating module creation *)

let rec createM path m =
  match m.it with
  | LitM d ->
    let s, menv = createD path d in
    let var = mangle path in
    "const " ^ var ^ " = {};\n" ^ s, (normalizeT (sem m), path) :: menv
  | FromM p ->
    createM path p
  | _ ->
    "", []

and createD path d =
  match d.it with
  | ModD (x, m) ->
    createM (path ^ "." ^ x.it) m
  | SeqD (d1, d2) ->
    let s1, menv1 = createD path d1 in
    let s2, menv2 = createD path d2 in
    s1 ^ s2, menv1 @ menv2
  | _ ->
    "", []


(* Generating module linking *)

let rec linkM menv path m =
  match m.it with
  | VarM _ | DotM _ ->
    ""
  | LitM d ->
    let exports = domT (sem m) in
    linkD menv exports path d ^
    "Object.freeze(" ^ mangle path ^ ");\n"
  | FromM p ->
    linkM menv path p

and linkD menv exports path d =
  match d.it with
  | SeqD (d1, d2) ->
    linkD menv exports path d1 ^ linkD menv exports path d2
  | LetD (x, e) when Dom.mem x.it exports ->
    let path' = path ^ "." ^ x.it in
    "Object.defineProperty(" ^ mangle path ^ ", \"" ^ x.it ^ "\", " ^
      "{get: function() { return " ^ mangle path' ^ " }});\n"
  | ModD (x, m) when Dom.mem x.it exports ->
    let path' = List.assq (normalizeT (sem m)) menv in
    "Object.defineProperty(" ^ mangle path ^ ", \"" ^ x.it ^ "\", " ^
      "{value: " ^ mangle path' ^ "});\n" ^
    linkM menv (path ^ "." ^ x.it) m
  | ModD (x, m) ->
    let path' = List.assq (normalizeT (sem m)) menv in
    linkM menv path' m
  | ImpD (m, x) when Dom.mem x.it exports ->
    let path' = List.assq (normalizeT (sem m)) menv in
    "Object.defineProperty(" ^ mangle path ^ ", \"" ^ x.it ^ "\", " ^
      "{get: function() { return " ^ mangle path' ^ "." ^ x.it ^ " }});\n"
  | IncD m ->
    let path' = List.assq (normalizeT (sem m)) menv in
    let _, r = normalizeR (asModT (sem m)) in
    Env.fold
      (fun l _ s ->
        if Dom.mem l exports then
          s ^
          "Object.defineProperty(" ^ mangle path ^ ", \"" ^ l ^ "\", " ^
            "{get: function() { return " ^ mangle path' ^ "." ^ l ^ " }});\n"
        else s
      ) r ""
  | _ -> ""


(* Generating module execution *)

let rec envD menv path d =
  match d.it with
  | LetD (x, e) ->
    Env.singleton x.it (mangle (path ^ "." ^ x.it))
  | ModD (x, m) ->
    let path' = List.assq (normalizeT (sem m)) menv in
    Env.singleton x.it (mangle path')
  | ImpD (m, x) ->
    let path' = List.assq (normalizeT (sem m)) menv in
    Env.singleton x.it (mangle path' ^ "." ^ x.it)
  | IncD m ->
    let path' = List.assq (normalizeT (sem m)) menv in
    let _, r = normalizeR (asModT (sem m)) in
    Env.mapi (fun l _ -> mangle path' ^ "." ^ l) r
  | SeqD (d1, d2) ->
    Env.fold Env.add (envD menv path d1) (envD menv path d2)
  | _ ->
    Env.empty


let translate_list translateX xs =
  String.concat ", " (List.map translateX xs)

let rec translateE env e =
  match e.it with
  | VarE x -> mangle (Env.find x.it env)
  | DotE (m, x) -> translateP env m ^ "." ^ x.it
  | FunE (xs, e) ->
    let env' = List.fold_left (fun env x -> Env.add x.it x.it env) env xs in
    "function(" ^ translate_list it xs ^ ") " ^
      "{ return " ^ translateE env' e ^ " }"
  | AppE (e, es) ->
    translateE env e ^ "(" ^ translate_list (translateE env) es ^ ")"
  | StrE s -> "\"" ^ String.escaped s ^ "\""

and translateP env m =
  match m.it with
  | VarM x -> mangle (Env.find x.it env)
  | DotM (m', x) -> translateP env m' ^ "." ^ x.it
  | _ -> assert false

and translateM menv env path m =
  match m.it with
  | LitM d ->
    translateD menv (Env.fold Env.add (envD menv path d) env) path d
  | FromM p ->
    translateM menv env0 path p
  | _ -> ""

and translateD menv env path d =
  match d.it with
  | RunD e ->
    translateE env e ^ ";\n"
  | LetD (x, e) ->
    let path' = path ^ "." ^ x.it in
    (if !harmony_flag then "let " else "var ") ^
    mangle path' ^ " = " ^ translateE env e ^ ";\n"
  | ModD (x, m) ->
    translateM menv env (path ^ "." ^ x.it) m
  | SeqD (d1, d2) ->
    translateD menv env path d1 ^ translateD menv env path d2
  | _ ->
    ""


(* Complete translation *)

let translate p =
  let global = "_" in
  let s1, menv = createM global p in
  let s2 = linkM menv (List.assq (normalizeT (sem p)) menv) p in
  let s3 = translateM menv env0 global p in
  (if !harmony_flag then "\"use strict\";\n" else "") ^
  "{\n" ^
  "// Create:\n" ^ s1 ^
  "// Link:\n" ^ s2 ^
  "// Run:\n" ^ s3 ^
  "}\n"
