(*************************************************************
 *                                                           *
 *       Cryptographic protocol verifier                     *
 *                                                           *
 *       Bruno Blanchet and David Cadé                       *
 *                                                           *
 *       Copyright (C) ENS, CNRS, INRIA, 2005-2017           *
 *                                                           *
 *************************************************************)

(*

    Copyright ENS, CNRS, INRIA 
    contributors: Bruno Blanchet, Bruno.Blanchet@inria.fr
                  David Cadé

This software is a computer program whose purpose is to verify 
cryptographic protocols in the computational model.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use, 
modify and/ or redistribute the software under the terms of the CeCILL-B
license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info". 

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability. 

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or 
data to be ensured and,  more generally, to use and operate it in the 
same conditions as regards security. 

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-B license and that you accept its terms.

*)
open Types

(* 1. Compute and use simp_facts: facts that are separated into
   equalities that can be used as substitutions, other terms,
   else-find facts *)

(* Filter out terms if/let/find/res/event, so that the remaining terms
   can be used as facts *)
val filter_ifletfindres : term list -> term list

(* match_term is an intermediate function used for apply_reds. It is exported
   because we need to compute a variant of apply_reds in dependency analyses. *)
val match_term : simp_facts -> binder list -> (unit -> 'a) -> term -> term -> unit -> 'a

(* set to true by the functions below when they reduce the term *)
val reduced : bool ref

(* [apply_eq_statements_and_collisions_subterms_once reduce_rec try_no_var t] 
   simplifies the term [t] using the equalities coming from the
   equational theories, the equality statements, and the collisions  
   given in the input file.
   [reduce_rec f t] must simplify the term [t] knowing the fact [f] 
   in addition to the already known facts. It sets the flag [reduced]
   when [t] has really been modified.
   [try_no_var t] must simplify the term [t] by replacing variables
   with their values according to currently known facts. *)
val apply_eq_statements_and_collisions_subterms_once : (term -> term -> term) -> simp_facts -> term -> term

(* [apply_eq_statements_subterms_once try_no_var t] simplifies
   the term [t] using the equalities coming from the
   equational theories and the equality statements given in the input file.
   [try_no_var] is as above. *)
val apply_eq_statements_subterms_once : simp_facts -> term -> term

(* [apply_reds simp_facts t] applies all equalities coming from the
   equational theories, equality statements, and collisions given in
   the input file to all subterms of the term [t], taking into account
   the equalities in [simp_facts] to enable their application.
   Application is repeated until a fixpoint is reached. *)
val apply_reds : simp_facts -> term -> term

(* Display the facts. Mainly used for debugging *)
val display_elsefind : elsefind_fact -> unit
val display_facts : simp_facts -> unit

(* A dependency analysis is a function of type 
   [dep_anal = simp_facts -> term -> term -> term option] 
   such that [dep_anal facts t1 t2] is [Some t'] when [t1 = t2] 
   can be simplified into the term [t']
   up to negligible probability, by eliminating collisions
   between [t1] and [t2] using the results of some dependency analysis.
   Otherwise, [dep_anal facts t1 t2] returns [None].

   [no_dependency_anal] is a particular dependency analysis that
   does nothing, i.e. always returns [None].
   Other dependency analyses are defined in simplify.ml.
 *)
val no_dependency_anal : dep_anal

(* [simplif_add dep_anal facts t] updates the facts by taking into
   account that the term [t] is true. It can use [dep_anal] to eliminate
   collisions. 
   Raises Contradiction when the added fact [t] cannot be true when
   [facts] hold.
*)
val simplif_add : dep_anal -> simp_facts -> term -> simp_facts
(* [simplif_add_find_cond] is the same as [simplif_add] except
   that it allows (and ignores) terms that are not variables or function
   applications *)
val simplif_add_find_cond : dep_anal -> simp_facts -> term -> simp_facts
(* [simplif_add_list dep_anal facts tl] updates the facts by taking into
   account that the terms in [tl] are true. It can use [dep_anal] to eliminate
   collisions.
   Raises Contradiction when the added facts [tl] cannot be true when
   [facts] hold.
 *)
val simplif_add_list : dep_anal -> simp_facts -> term list -> simp_facts

(* 2. Compute the facts that hold and the variables that are defined
   at certain program points. *)

(* [get_initial_history pp] gets the known_history corresponding to the program
   point [pp] *)
val get_initial_history : program_point -> known_history option

(* [def_vars_from_defined current_node def_list] returns the variables that
   are known to be defined when the condition of a find with defined condition 
   [def_list] holds. [current_node] is the node of the find, at which [def_list]
   is tested (may be returned by [get_node]).
   Raises Contradiction when a variable that must be defined when [def_list]
   is defined has no definition in the game. *)
val def_vars_from_defined : known_history option -> binderref list -> binderref list

(* [facts_from_defined current_node def_list] returns the facts that
   are known to hold when the condition of a find with defined condition 
   [def_list] holds. [current_node] is the node of the find, at which [def_list]
   is tested (may be returned by [get_node]).
   Raises Contradiction when a variable that must be defined when [def_list]
   is defined has no definition in the game. *)
val facts_from_defined : known_history option -> binderref list -> term list

(* [get_def_vars_at pp] returns the variables that are known
   to be defined at program point [pp].
   May raise Contradiction when the program point [pp] is
   unreachable. *)
val get_def_vars_at : program_point -> binderref list

(* [get_facts_at pp] returns the facts that are known to hold
   at program point [pp].
   May raise Contradiction when the program point [pp] is
   unreachable. *)
val get_facts_at : program_point -> term list

(* [get_def_vars_full_block pp] returns the variables that are known
   to be defined at the end of the input...output block containing 
   program point [pp].
   May raise Contradiction when the program point [pp] is
   unreachable. *)
val get_def_vars_full_block : program_point -> binderref list

(* [get_facts_full_block pp] returns the facts that are known to hold
   at the end of the input...output block containing program point [pp].
   May raise Contradiction when the program point [pp] is
   unreachable. *)
val get_facts_full_block : program_point -> term list

(* [get_facts_full_block_cases pp] returns the facts that are known to hold
   at the end of the input...output block containing program point [pp]. 
   It is a modified version of
   [get_facts_full_block] to distinguish cases depending on the
   definition point of variables (instead of taking intersections), to
   facilitate the proof of correspondences.
   May raise Contradiction when the program point [pp] is
   unreachable. *)
val get_facts_full_block_cases : program_point -> term list * term list list list
 
(* [get_elsefind_facts_at pp] returns the elsefind facts that are known to hold
   at program point [pp]. *)
   
val get_elsefind_facts_at : program_point -> elsefind_fact list

(* [reduced_def_list fact_info def_list] removes variables that are 
   certainly defined from a [def_list] in a find. [fact_info] corresponds
   to the facts at the considered find. *)
val reduced_def_list : fact_info -> binderref list -> binderref list

(* Functions useful to simplify def_list *)

(* [filter_def_list accu l] returns a def_list that contains
   all elements of [accu] and [l] except the elements whose definition
   is implied by the definition of some other element of [l].
   The typical call is [filter_def_list [] l], which returns 
   a def_list that contains all elements of [l] except 
   the elements whose definition is implied by the definition 
   of some other element.*)
val filter_def_list : binderref list -> binderref list -> binderref list

(* [remove_subterms accu l] returns a def_list that contains
   all elements of [accu] and [l] except elements that
   also occur as subterms in [l].
   The typical call is  [remove_subterms [] l], which returns
   [l] with elements that occur as subterms removed. *)
val remove_subterms : binderref list -> binderref list -> binderref list

(* [eq_deflists dl dl'] returns true when the two def_list [dl]
   and [dl'] are equal (by checking mutual inclusion) *)
val eq_deflists : binderref list -> binderref list -> bool

(* [update_def_list_term already_defined newly_defined bl def_list tc' p'] 
   returns an updated find branch [(bl, def_list', tc'', p'')].
   This function should be called after modifying a branch of find 
   (when the find is a term), to make sure that all needed variables are defined.
   It updates in particular [def_list], but may also add defined conditions
   inside [tc'] or [p'].
   [already_defined] is a list of variables already known to be defined
   above the find.
   [newly_defined] is the set of variables whose definition is guaranteed
   by the old defined condition [def_list]; it is used only for a sanity check.
   [bl, def_list, tc', p'] describe the modified branch of find:
   [bl] contains the indices of find
   [def_list] is the old def_list
   [tc'] is the modified condition of the find
   [p'] is the modified then branch of the find. *) 
val update_def_list_term : binderref list -> binderref list -> 
  (binder * repl_index) list -> binderref list -> term -> term ->
    term findbranch

(* [update_def_list_process already_defined newly_defined bl def_list t' p1'] 
   returns an updated find branch [(bl, def_list', t'', p1'')].
   This function should be called after modifying a branch of find 
   (when the find is a process), to make sure that all needed variables are defined.
   It updates in particular [def_list], but may also add defined conditions
   inside [t'] or [p1'].
   [already_defined] is a list of variables already known to be defined
   above the find.
   [newly_defined] is the set of variables whose definition is guaranteed
   by the old defined condition [def_list]; it is used only for a sanity check.
   [bl, def_list, t', p1'] describe the modified branch of find:
   [bl] contains the indices of find
   [def_list] is the old def_list
   [t'] is the modified condition of the find
   [p1'] is the modified then branch of the find. *) 
val update_def_list_process : binderref list -> binderref list -> 
  (binder * repl_index) list -> binderref list -> term -> process ->
    process findbranch

(* 3. Some rich functions that rely on collecting facts and reasoning 
   about them *)

(* [simplify_term dep_anal facts t] returns a simplified form of
   the term [t] using the dependency analysis [dep_anal] and the
   true facts [facts]. *)
val simplify_term : dep_anal -> simp_facts -> term -> term

(* [check_equal t t' simp_facts] returns true when [t] and [t'] are
   proved equal when the facts in [simp_facts] are true.
   It is called from transf_insert_replace.ml. The probability of collisions
   eliminated to reach that result is taken into account by module [Proba]. *)
val check_equal : term -> term -> simp_facts -> bool

(* [display_facts_at p occ] displays the facts that are known
   to hold at the program point [occ] of the process [p]. *)
val display_facts_at : inputprocess -> int -> unit
