(**************************************************************************)
(*                                                                        *)
(*                                 VSCoq                                  *)
(*                                                                        *)
(*                   Copyright INRIA and contributors                     *)
(*       (see version control and README file for authors & dates)        *)
(*                                                                        *)
(**************************************************************************)
(*                                                                        *)
(*   This file is distributed under the terms of the MIT License.         *)
(*   See LICENSE file.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Types

(** The scheduler is the component in charge of planning the execution of
    sentences. It also defines the task delegation strategy, and computes
    dependencies between tasks. Scheduling can be done incrementally. *)

type state
(** State for incremental schedule construction *)

val initial_state : state

type error_recovery_strategy =
  | RSkip
  | RAdmitted

type executable_sentence = {
  id : sentence_id;
  ast : ast;
  synterp : Vernacstate.Synterp.t;
  error_recovery : error_recovery_strategy;
}

type task =
  | Skip of sentence_id
  | Exec of executable_sentence
  | OpaqueProof of { terminator: executable_sentence;
                     opener_id: sentence_id;
                     proof_using: Vernacexpr.section_subset_expr;
                     tasks : executable_sentence list; (* non empty list *)
                   }
  | Query of executable_sentence

type schedule
(** Holds the dependencies among sentences and a schedule to evaluate all
    sentences *)

val initial_schedule : schedule

val schedule_sentence : sentence_id * (ast * Vernacextend.vernac_classification * Vernacstate.Synterp.t) option -> state -> schedule -> state * schedule
(** Identifies the structure of the document and dependencies between sentences
    in order to easily compute the tasks to interpret the a sentence.
    Input sentence is None on parsing error. *)

val task_for_sentence : schedule -> sentence_id -> sentence_id option * task
(** Finds the task to be performed and the state on which the task should run *)

val dependents : schedule -> sentence_id -> sentence_id_set
(** Computes what should be invalidated *)

(*
val string_of_schedule : schedule -> string
*)
