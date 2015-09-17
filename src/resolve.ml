(*
 * Module resolution for EcmaScript 6
 * (c) 2012 Andreas Rossberg
 *)

open Source
open Syntax
open Types


let env0 = Env.singleton "print" valT


let rec exportsD d =
  match d.it with
  | SeqD (d1, d2) -> Dom.union (exportsD d1) (exportsD d2)
  | ExpD x -> Dom.singleton x.it
  | _ -> Dom.empty


(* Types that may end up unbound, quick & dirty via global state :-} *)
let critical_ts = ref []

let rec resolveE env e =
  match e.it with
  | VarE x ->
    if Env.mem x.it env then
      unifyT (Env.find x.it env) valT
    else
      raise (Error (x.at, "unbound variable `" ^ x.it ^ "'"))
  | DotE (m, x) ->
    let r = varR () in
    resolveM env m (modT (extR r x.it valT))
  | FunE (xs, e) ->
    resolveE (List.fold_left (fun env x -> Env.add x.it valT env) env xs) e
  | AppE (e, es) ->
    resolveE env e; List.iter (resolveE env) es
  | StrE s -> ()

and resolveM env m t =
  ignore (m <~ t);
  match m.it with
  | VarM x ->
    if Env.mem x.it env then
      let r = varR () in
      unifyT (Env.find x.it env) (modT r);
      unifyT t (Env.find x.it env);
      critical_ts := t :: !critical_ts
    else
      raise (Error (x.at, "unbound variable `" ^ x.it ^ "'"))
  | DotM (m', x) ->
    let r = varR () in
    resolveM env m' (modT (extR r x.it t));
    critical_ts := t :: !critical_ts
  | LitM d ->
    let r = forwardD env d in
    unifyT t (modT (filterR (exportsD d) r));
    let _, env' = normalizeR r in
    resolveD (Env.fold Env.add env' env) d
  | FromM p ->
    let save_ts = !critical_ts in
    critical_ts := [];
    let t' = resolve p in
    critical_ts := save_ts;
    unifyT t t'

and resolveD env d =
  match d.it with
  | EmpD -> ()
  | SeqD (d1, d2) -> resolveD env d1; resolveD env d2
  | LetD (_, e) | RunD e -> resolveE env e
  | ModD (x, m) -> resolveM env m (Env.find x.it env)
  | ExpD x -> ()
  | ImpD (m, x) ->
    let r = varR () in
    let t = Env.find x.it env in
    resolveM env m (modT (extR r x.it t))
  | IncD m -> ()

and resolve p =
  try
    let t = modT (varR ()) in
    resolveM env0 p t;
    if List.for_all definiteT !critical_ts then
      normalizeT t
    else
      raise (Error (p.at, "cyclic module definition"));
  with Unify desc ->
    let s1, s2 =
      match desc with
      | `Typ (t1, t2) -> string_of_typ t1, string_of_typ t2
      | `Row (r1, r2) -> string_of_row r1, string_of_row r2
    in raise (Error (p.at, "unification error: " ^ s1 ^ " =!= " ^ s2))

and forwardD env d =
  match d.it with
  | EmpD | ExpD _ | RunD _ -> d <~ empR
  | SeqD (d1, d2) ->
    let r1 = forwardD env d1 in
    let r2 = forwardD env d2 in
    let clash = Dom.inter (domR r1) (domR r2) in
    if not (Dom.is_empty clash) then
      raise (Error
        (d.at, "multiple declarations for `" ^ Dom.choose clash ^ "'"));
    d <~ catR r1 r2
  | LetD (x, _) -> d <~ extR empR x.it valT
  | ModD (x, _) -> d <~ extR empR x.it (varT ())
  | ImpD (_, x) -> d <~ extR empR x.it (varT ())
  | IncD m ->
    let r = varR () in
    resolveM env m (modT r);
    let vo, r' = normalizeR r in
    if vo = None then
      d <~ r
    else
      raise (Error (d.at, "invalid forward import"))
