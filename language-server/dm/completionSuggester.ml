module CompactedDecl = Context.Compacted.Declaration
open Printer

let get_type_of_hyp env id =
  match EConstr.lookup_named id env with
  | Context.Named.Declaration.LocalAssum (_, ty) -> ty
  | _ -> CErrors.user_err (let open Pp in
                            str (Names.Id.to_string id) ++
                            str " is not a plain hypothesis")

let mk_hyp sigma d (env,l) =
  let d' = CompactedDecl.to_named_context d in
  let env' = List.fold_right Environ.push_named d' env in
  let (ids : Names.variable Context.binder_annot list), typ = match d with
  | CompactedDecl.LocalAssum (ids, typ) -> ids, typ
  | CompactedDecl.LocalDef (ids,c,typ) -> ids, typ
  in
  let ids' = List.map (fun id -> Names.Id.to_string id.Context.binder_name) ids in
  let typ' = pr_ltype_env env sigma typ in
  let hyps = ids' |> List.map (fun id -> (id, Pp.string_of_ppcmds typ', "")) in
  (env', hyps @ l)

let mk_hyp_2 sigma d (env,l) =
  let d' = CompactedDecl.to_named_context d in
  let env' = List.fold_right Environ.push_named d' env in
  let (ids : Names.variable Context.binder_annot list), typ = match d with
  | CompactedDecl.LocalAssum (ids, typ) -> ids, typ
  | CompactedDecl.LocalDef (ids,c,typ) -> ids, typ
  in
  let ids' = ids |> List.map Context.binder_name in
  let type_string = typ |> pr_ltype_env env sigma |> Pp.string_of_ppcmds in
  let hyps = ids' |> List.map (fun id -> (get_type_of_hyp env id, (Names.Id.to_string id, type_string, ""))) in
  (env', hyps @ l)

let mk_hyp_id sigma d (env,l) = 
  let d' = CompactedDecl.to_named_context d in
  let env' = List.fold_right Environ.push_named d' env in
  let ids, typ = match d with
  | CompactedDecl.LocalAssum (ids, typ) -> ids, typ
  | CompactedDecl.LocalDef (ids,c,typ) -> ids, typ
  in
  let actualIds = List.map Context.binder_name ids in
  (env', actualIds @ l)

let map_first_goal_option st loc f default = 
  DocumentManager.get_proof st loc
    |> Option.map (fun Proof.{ goals; sigma; _ } -> Option.cata (f sigma) default (List.nth_opt goals 0)) 
let get_hyp_abstract f st loc = 
  let mk_hyps sigma goal =
    let evi = Evd.find sigma goal in
    let env = Evd.evar_filtered_env (Global.env ()) evi in
    let min_env = Environ.reset_context env in
    let (_env, hyps) =
      Context.Compacted.fold (f sigma)
        (Termops.compact_named_context (Environ.named_context env)) ~init:(min_env,[]) in
    hyps in
  map_first_goal_option st loc mk_hyps []
let get_hyps_ids = get_hyp_abstract mk_hyp_id


let get_first_goal st loc = 
  DocumentManager.get_proof st loc
    |> Option.map (fun Proof.{ goals; sigma; _ } -> (List.nth_opt goals 0))
    |> Option.flatten
    |> Option.get

let get_sigma st loc = 
  DocumentManager.get_proof st loc
    |> Option.map (fun Proof.{ goals; sigma; _ } -> sigma)
    |> Option.get


let get_hyps = get_hyp_abstract mk_hyp

let get_goal_name sigma goal =
  let evi = Evd.find sigma goal in
  let env = Evd.evar_filtered_env (Global.env ()) evi in
  let ccl = pr_letype_env ~goal_concl_style:true env sigma (Evd.evar_concl evi) in
  Pp.string_of_ppcmds ccl

let get_goal_type (sigma : Evd.evar_map) (goal  : Evar.t) : Evd.econstr  =
  let evi = Evd.find sigma goal in
  Evd.evar_concl evi

let get_type (item : CompletionItem.completion_item) =
  match item.ref with
  | VarRef r -> Some (get_type_of_hyp item.env r)
  | _ -> None

let get_completion_items ~id params st loc =
  let sigma = get_sigma st loc in
  let hypotheses = get_hyp_abstract mk_hyp_2 st loc in
  (*let lemmasOption = DocumentManager.get_lemmas st loc in
  let lemmas = lemmasOption |> Option.map (fun ls -> ls 
    |> List.map (fun item -> (get_type item, CompletionItem.pp_completion_item item)) 
    |> List.filter_map (function 
      | (None, b) -> None
      | (Some a, b) -> Some (a,b)
      )) in*)
  let lemmas = None in
  let goalType = get_first_goal st loc |> get_goal_type sigma in
  let completionItems = [hypotheses; lemmas] 
    |> List.map (Option.default [])
    |> List.flatten 
  in
  let finalItems = 
    match List.filter (fun (typ, _) -> EConstr.eq_constr sigma typ goalType) completionItems with
    | [] -> completionItems
    | xs -> xs
  in
  List.map snd finalItems

let get_completion_items2 ~id params st loc =
  let hypotheses = get_hyp_abstract mk_hyp_2 st loc in
  let hypotheses_strings = hypotheses |> Option.map (List.map snd) in
  let lemmasOption = DocumentManager.get_lemmas st loc in
  let lemmas = lemmasOption |> Option.map (List.map CompletionItem.pp_completion_item) in
  let goalOption = map_first_goal_option st loc get_goal_name "" in
  let completionItems = [hypotheses_strings; lemmas] 
    |> List.map (Option.default [])
    |> List.flatten 
  in
  let findPossibleOverlap goal = 
    match List.filter (fun (_, typ, _) -> typ = goal) completionItems with
    | [] -> completionItems
    | xs -> xs
  in
  Option.map findPossibleOverlap goalOption |> Option.default completionItems
  