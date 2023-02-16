module CompactedDecl = Context.Compacted.Declaration
open Printer

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

let map_first_goal_option st loc f default = 
  DocumentManager.get_proof st loc
    |> Option.map (fun Proof.{ goals; sigma; _ } -> Option.cata (f sigma) default (List.nth_opt goals 0)) 

let get_hyps st loc =
  let mk_hyps sigma goal =
    let evi = Evd.find sigma goal in
    let env = Evd.evar_filtered_env (Global.env ()) evi in
    let min_env = Environ.reset_context env in
    let (_env, hyps) =
      Context.Compacted.fold (mk_hyp sigma)
        (Termops.compact_named_context (Environ.named_context env)) ~init:(min_env,[]) in
    hyps in
  map_first_goal_option st loc mk_hyps []

let get_goal_name sigma goal =
  let evi = Evd.find sigma goal in
  let env = Evd.evar_filtered_env (Global.env ()) evi in
  let ccl = pr_letype_env ~goal_concl_style:true env sigma (Evd.evar_concl evi) in
  Pp.string_of_ppcmds ccl

let get_completion_items ~id params st loc =
  let open Yojson.Basic.Util in
  let hypotheses = get_hyps st loc in
  let lemmasOption = DocumentManager.get_lemmas st loc in
  let lemmas = lemmasOption |> Option.map (List.map CompletionItem.pp_completion_item) in
  let goalOption = map_first_goal_option st loc get_goal_name "" in
  let completionItems = [hypotheses; lemmas] 
    |> List.map (Option.default [])
    |> List.flatten 
  in
  let findPossibleOverlap goal = 
    match List.filter (fun (_, typ, _) -> typ = goal) completionItems with
    | [] -> completionItems
    | xs -> xs
  in
  Option.map findPossibleOverlap goalOption |> Option.default completionItems
  