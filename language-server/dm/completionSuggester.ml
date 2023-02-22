module CompactedDecl = Context.Compacted.Declaration
open Printer
open EConstr

module TypeCompare = struct
  type t = types
  let compare = compare
end

module Atomics = Set.Make(TypeCompare)

let mk_hyp sigma d (env,l) =
  let d' = CompactedDecl.to_named_context d in
  let env' = List.fold_right Environ.push_named d' env in
  let ids, typ = match d with
  | CompactedDecl.LocalAssum (ids, typ) -> ids, typ
  | CompactedDecl.LocalDef (ids,c,typ) -> ids, typ
  in
  let ids' = List.map (fun id -> Names.Id.to_string id.Context.binder_name) ids in
  let typ' = pr_ltype_env env sigma typ in
  let hyps = ids' |> List.map (fun id -> (id, Pp.string_of_ppcmds typ', "")) in
  (env', hyps @ l)

let get_goal_type st loc =
  let goal, sigma = 
    match DocumentManager.get_proof st loc with
    | Some Proof.{ goals; sigma; _ } -> List.nth goals 0, sigma
    | None -> raise (Invalid_argument "goal") 
  in
  let evi = Evd.find_undefined sigma goal in
  let env = Evd.evar_filtered_env (Global.env ()) evi in
  (Evd.evar_concl evi, sigma, env)

let kind_of_type_opt sigma t = try Some (kind_of_type sigma t) with exn -> None 

let debug_print_kind_of_type sigma env k: unit = 
  let rec aux i k = 
    Printf.eprintf "%s" (String.init i (fun _ -> ' '));
    match k with
    | Some SortType t -> 
      Printf.eprintf "SortType\n"; 
    | Some CastType (tt, t) ->
      Printf.eprintf "CastType\n"; 
    | Some ProdType (n, t1, t2) ->
      Printf.eprintf "ProdType %s\n" (Names.Name.print n.binder_name |> Pp.string_of_ppcmds); 
      aux (i+1) (kind_of_type_opt sigma t1);
      aux (i+1) (kind_of_type_opt sigma t2);
    | Some LetInType _ ->
      Printf.eprintf "LetInType\n"; 
    | Some AtomicType (t, ta) -> 
      Printf.eprintf "AtomicType %s\n" (Pp.string_of_ppcmds (pr_econstr_env env sigma t)); 
      Array.iter (fun t -> kind_of_type_opt sigma t |> aux (i+1)) ta;
    | None -> () (* Lol :) *)
    in
  aux 0 k

(* Currently atomic type also returns "_UNBOUND_REL_N, we should probably skip those. "*)

let atomic_types sigma env t: Atomics.t = 
  let rec aux t : types list = 
    match (kind_of_type_opt sigma t) with
    | Some SortType t -> [] (* Might be possible to get atomics from also *)
    | Some CastType (tt, t) -> [] (* Dont know if we need this *)
    | Some ProdType (n, t1, t2) -> aux t1 @ aux t2
    | Some LetInType _ -> [] 
    | Some AtomicType (t, ta) ->
      t :: (Array.map aux ta |> Array.to_list |> List.flatten);
    | None -> [] (* Lol :) *)
    in
  aux t |>
  Atomics.of_list

let debug_print_atomics env sigma atomics = 
  Atomics.fold (fun t l -> (Pp.string_of_ppcmds (pr_econstr_env env sigma t ) |> Printf.sprintf "%s") :: l) atomics [] |>
  String.concat "," |>
  Printf.eprintf "Atomics: [%s]\n"

let compare_atomics (goal : Atomics.t) (a1, _ : Atomics.t * _) (a2, _ : Atomics.t * _) : int = 
  match (Atomics.inter a1 goal, Atomics.inter a2 goal) with
  | r1, r2 when Atomics.cardinal r1 = Atomics.cardinal r2 -> 
    (* If the size is equal, priotize the one with fewest types *)
    compare (Atomics.cardinal a1) (Atomics.cardinal a2)
  | r1, r2 -> 
    (* Return the set with largest overlap, so we sort in increasing order swap the arguments *)
    compare (Atomics.cardinal r2) (Atomics.cardinal r1)

let rank_choices (goal : Evd.econstr) sigma env lemmas : CompletionItems.completion_item list =
  let lemmaAtomics = List.map (fun (l : CompletionItems.completion_item) -> 
    (atomic_types sigma env (of_constr l.typ), l)
  ) lemmas in
  let goalAtomics = atomic_types sigma env goal in
  List.stable_sort (compare_atomics goalAtomics) lemmaAtomics |> 
  List.map snd

let get_hyps st loc =
  let mk_hyps sigma goal =
    let EvarInfo evi = Evd.find sigma goal in
    let env = Evd.evar_filtered_env (Global.env ()) evi in
    let min_env = Environ.reset_context env in
    let (_env, hyps) =
      Context.Compacted.fold (mk_hyp sigma)
        (Termops.compact_named_context (Environ.named_context env)) ~init:(min_env,[]) in
    hyps in

  DocumentManager.get_proof st loc
    |> Option.map (fun Proof.{ goals; sigma; _ } -> Option.cata (mk_hyps sigma) [] (List.nth_opt goals 0)) 
 
let take n l =
  let rec sub_list n accu l =
    match l with 
    | [] -> accu 
    | hd :: tl ->
      if n = 0 then accu 
      else sub_list (n - 1) (hd :: accu) tl
  in
  List.rev (sub_list n [] l)

let get_completion_items ~id params st loc =
  let open Yojson.Basic.Util in
  let hypotheses = get_hyps st loc in
  let lemmasOption = DocumentManager.get_lemmas st loc in
  let goal, sigma, env = get_goal_type st loc in
  let lemmas = lemmasOption |> Option.map 
    (fun l -> 
      rank_choices goal sigma env l |> 
      take 10000 |> 
      List.map CompletionItems.pp_completion_item
    ) in
  [lemmas; hypotheses] 
  |> List.map (Option.default [])
  |> List.flatten