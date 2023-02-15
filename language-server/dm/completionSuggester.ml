module CompactedDecl = Context.Compacted.Declaration
open Printer

let mk_hyp sigma d (env,l) =
  let d' = CompactedDecl.to_named_context d in
  let env' = List.fold_right Environ.push_named d' env in
  let ids, typ = match d with
  | CompactedDecl.LocalAssum (ids, typ) -> ids, typ
  | CompactedDecl.LocalDef (ids,c,typ) -> ids, typ
  in
  let ids' = List.map (fun id -> `String (Names.Id.to_string id.Context.binder_name)) ids in
  let typ' = pr_ltype_env env sigma typ in
  (*let hyps2 = ids' |> List.map (fun id -> `Assoc [
    "label", id;
    "typeString", `String (Pp.string_of_ppcmds typ')
  ]) in*)
  let open CompletionItem in
  let hyps = ids' |> List.map (fun id -> mk_completion_item sigma null null env null) in
  (env', hyps @ l)

let mk_hyps sigma goal =
  let evi = Evd.find sigma goal in
  let env = Evd.evar_filtered_env (Global.env ()) evi in
  let min_env = Environ.reset_context env in
  let (_env, hyps) =
    Context.Compacted.fold (mk_hyp sigma)
      (Termops.compact_named_context (Environ.named_context env)) ~init:(min_env,[]) in
  hyps
 
let getCompletionItems ~id params st loc =
  let open Yojson.Basic.Util in
  let hypotheses =
    DocumentManager.get_proof st loc
    |> Option.map (fun Proof.{ goals; sigma; _ } -> Option.cata (mk_hyps sigma) [] (List.nth_opt goals 0)) in
  let lemmas = DocumentManager.get_lemmas st loc in
  [hypotheses; lemmas] 
  |> List.map (Option.default [])
  |> List.flatten