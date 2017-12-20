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
open Simplify1

(* Expand all if, let, and find in expressions, so that they occur only in 
   processes. 

After expansion, if/let/find/res may occur in terms only in conditions of find.
*)

let whole_game = ref Terms.empty_game

let current_pass_transfos = ref []

(* Priorities for orienting equalities into rewrite rules *)
let current_max_priority = ref 0
let priority_list = ref []

let simplify_term t = 
  if (Terms.check_simple_term t) && (not (Terms.is_true t || Terms.is_false t)) then
    begin
      try
	let facts = Facts.get_facts_at (DTerm t) in
	let simp_facts = Facts.simplif_add_list Facts.no_dependency_anal ([],[],[]) facts in
	let t' = Facts.simplify_term Facts.no_dependency_anal simp_facts t in
	(* When the obtained term is a complex term, using array accesses, I may
	   need to update defined conditions above the current program point.
	   To avoid the burden of doing that, I keep the result only when it is 
	   true or false. This is the only useful case for obtaining a smaller game in
	   expand, and the full simplification will be done later. *)
	if Terms.is_true t' || Terms.is_false t' then
	  begin
	    Settings.changed := true;
	    current_pass_transfos := (SReplaceTerm(t,t')) :: (!current_pass_transfos);
	    t'
	  end
	else
          (* The change is not really useful, don't do it *)
	  t
      with Contradiction ->
	(* The current program point is unreachable, I can return any term.
	   Returning false seems to be the best to get a smaller game.
	   Notice that simplify_term is called only for boolean terms
	   (conditions of if/find) so false is of the correct type. *)
	Settings.changed := true;
	let t' = Terms.make_false() in
	current_pass_transfos := (SReplaceTerm(t,t')) :: (!current_pass_transfos);
	t'
    end
  else
    t

let rec filter_find tfind = function
    [] -> []
  | (bl, def_list, t, p)::r ->
      let r' = filter_find tfind r in
      let t' = simplify_term t in
      if Terms.is_false t' then 
	begin
	  current_pass_transfos := (SFindBranchRemoved(DTerm tfind,(bl, def_list, t, DTerm p))) :: (!current_pass_transfos);
	  r' 
	end
      else 
	(bl, def_list, t', p)::r'


let rec simplify_cterm t =
  let pp = DTerm t in
  match t.t_desc with
    Var(b,l) -> Terms.build_term2 t (Var(b, List.map simplify_cterm l))
  | ReplIndex i -> Terms.build_term2 t (ReplIndex i)
  | FunApp(f,l) -> Terms.build_term2 t (FunApp(f, List.map simplify_cterm l))
  | TestE(t1,t2,t3) -> 
      (* Some trivial simplifications *)
      let t1' = simplify_term t1 in
      if Terms.is_true t1' then 
	begin
	  current_pass_transfos := (STestTrue pp) :: (!current_pass_transfos);
	  simplify_cterm t2
	end
      else if Terms.is_false t1' then 
	begin
	  current_pass_transfos := (STestFalse pp) :: (!current_pass_transfos);
	  simplify_cterm t3
	end
      else
      Terms.build_term2 t (TestE(simplify_cterm t1', simplify_cterm t2, simplify_cterm t3))
  | FindE(l0,t3, find_info) -> 
      (* Remove useless branches if possible *)
      let l0 = filter_find t l0 in
      if l0 == [] then  
	begin
	  current_pass_transfos := (SFindRemoved(pp)) :: (!current_pass_transfos);
	  simplify_cterm t3
	end
      else
      let l0' = List.map (fun (bl,def_list,t1,t2) ->
	let t1' = simplify_cterm t1 in
	let t2' = simplify_cterm t2 in
	(bl, def_list, t1', t2')) l0
      in
      let t3' = simplify_cterm t3 in
      Terms.build_term2 t (FindE(l0', t3', find_info))
  | LetE(pat, t1, t2, topt) ->
      let pat' = simplify_pat pat in
      let t1' = simplify_cterm t1 in
      let t2' = simplify_cterm t2 in
      let topt' = match topt with
	None -> None
      | Some t3 -> Some (simplify_cterm t3)
      in
      Terms.build_term2 t (LetE(pat', t1', t2', topt'))
  | ResE(b,t1) ->
      Terms.build_term2 t (ResE(b, simplify_cterm t1))
  | EventAbortE(f) ->
      Terms.build_term2 t (EventAbortE(f))
  | EventE(t1,p) ->
      let t1' = simplify_cterm t1 in
      let p' = simplify_cterm p in
      Terms.build_term2 t (EventE(t1', p'))
  | GetE _ | InsertE _ ->
      Parsing_helper.internal_error "Get/Insert should not appear in Transf_expand.simplify_cterm"

and simplify_pat = function
    PatVar b -> PatVar b
  | PatTuple (f,l) -> PatTuple(f,List.map simplify_pat l)
  | PatEqual t -> PatEqual(simplify_cterm t)

let rec simplify_process p = 
  Terms.iproc_from_desc 
      (match p.i_desc with
	Nil -> Nil
      | Par(p1,p2) -> 
	  let p1' = simplify_process p1 in
	  let p2' = simplify_process p2 in
	  Par(p1', p2')
      | Repl(b,p) -> Repl(b, simplify_process p)
      | Input((c,tl),pat,p) ->
	  let tl' = List.map simplify_cterm tl in
	  let pat' = simplify_pat pat in
	  let p' = simplify_oprocess p in
	  Input((c, tl'), pat', p'))

and simplify_oprocess p =
  Terms.oproc_from_desc 
      (match p.p_desc with
	Yield -> Yield
      |	EventAbort f -> EventAbort f
      | Restr(b,p) -> Restr(b, simplify_oprocess p)
      | Test(t,p1,p2) -> 
	  let t' = simplify_cterm t in
	  let p1' = simplify_oprocess p1 in
	  let p2' = simplify_oprocess p2 in
	  Test(t', p1', p2')
      | Find(l0, p2, find_info) -> 
	  let l0' = List.map (fun (bl, def_list, t, p1) -> 
	    let t' = simplify_cterm t in
	    let p1' = simplify_oprocess p1 in
	    (bl, def_list, t', p1')) l0
	  in
	  let p2' = simplify_oprocess p2 in
	  Find(l0', p2', find_info)
      | Let(pat,t,p1,p2) ->
	  let pat' = simplify_pat pat in
	  let t' = simplify_cterm t in
	  let p1' = simplify_oprocess p1 in
	  let p2' = simplify_oprocess p2 in	  
	  Let(pat', t', p1', p2')
      | Output((c,tl),t2,p) ->
	  let tl' = List.map simplify_cterm tl in
	  let t2' = simplify_cterm t2 in
	  let p' = simplify_process p in
	  Output((c, tl'), t2', p')
      | EventP(t,p) ->
	  let t' = simplify_cterm t in
	  let p' = simplify_oprocess p in
	  EventP(t', p')
      | Get _ | Insert _ -> 
	  Parsing_helper.internal_error "Get/Insert should not appear in Transf_expand.simplify_oprocess")

let simplify_process g =
  current_pass_transfos := [];
  Proba.reset [] g;
  Terms.build_def_process None g.proc;
  let p' = simplify_process g.proc in
  let simplif_transfos = 
    if (!current_pass_transfos) != [] then
      [DSimplify(!current_pass_transfos)]
    else
      []
  in
  current_pass_transfos := [];
  let proba = Proba.final_add_proba [] in
  (* Cannot cleanup here because it may delete information
     in the initial game, needed to compute the probabilities.
     Terms.empty_def_process g.proc; *)
  ({ proc = p'; game_number = -1; current_queries = g.current_queries }, proba, simplif_transfos)

(* Check that a term/binderref contains no if, let, find, new *)
    
let check_no_ifletfind t =
  if not (Terms.check_simple_term t) then
    Parsing_helper.input_error "I cannot handle if, let, find, new inside the definition condition of a find. Sorry." t.t_loc

let check_no_ifletfind_br (_,l) =
  List.iter check_no_ifletfind l

(* Check if a term/pattern needs to be modified by expansion *)

let rec need_expand t =
  match t.t_desc with
    Var(_,l) | FunApp(_,l) -> List.exists need_expand l
  | ReplIndex _ -> false
  | _ -> true

let rec need_expand_pat = function
    PatVar _ -> false
  | PatTuple(_,l) -> List.exists need_expand_pat l
  | PatEqual t -> need_expand t

(* Initialization of probability counting *)  

let reset coll_elim g =
  whole_game := g;
  current_max_priority := 0;
  List.iter (fun b -> b.priority <- 0) (!priority_list);
  priority_list := [];
  Simplify1.reset coll_elim g

(* [dependency_collision cur_array simp_facts t1 t2] simplifies [t1 = t2]
using dependency analysis.
It returns
- [Some t'] when it simplified [t1 = t2] into [t'];
- [None] when it could not simplify [t1 = t2]. 
[cur_array] is the list of current replication indices at [t1 = t2].
[simp_facts] contains facts that are known to hold. *)

let dependency_collision cur_array simp_facts t1 t2 = 
  let t1' = try_no_var_rec simp_facts t1 in
  let t2' = try_no_var_rec simp_facts t2 in
  let true_facts = true_facts_from_simp_facts simp_facts in
  repl_index_list := [];
  Simplify1.try_two_directions (Simplify1.dependency_collision_rec3 cur_array true_facts) t1' t2'

(* Note on the elimination of collisions in find conditions:
   The find indices are replaced with fresh replication indices,
   so that we correctly take into account that
   the condition of find is executed for every value of the indices. *)

(* Simplify a term knowing some true facts:
   we only consider two cases: we want to get "true" or to get a tuple. *)

let prove_true cur_array simp_facts t =
  try
    let _ = Facts.simplif_add (dependency_collision cur_array) simp_facts (Terms.make_not t) in
    false
  with Contradiction ->
    true

let rec get_tuple cur_array simp_facts t =
  if Terms.is_tuple t then t else
  let t' = Terms.try_no_var simp_facts t in
  if Terms.is_tuple t' then t' else
  let t'' = Facts.simplify_term (dependency_collision cur_array) simp_facts t' in
  if Terms.is_tuple t'' then t'' else
  match t''.t_desc with
    Var _ when (not (Terms.synt_equal_terms t' t'')) ->
      get_tuple cur_array simp_facts t''
  | _ -> t
	
(*
  The reason why we just simplify to true/false or tuples is subtle. 
  The high-level idea is that simplification during expansion mainly
  aims at cutting branches; the terms can be reduced in the simplification
  pass after expansion. 
  This is important in tls13-core-PSKandPSKDHE-NoCorruption.cv: 
  otherwise, we get a regression because of a subsequent simplification
  that cannot be done: if I make simplification here, the simplification
  is done in one branch of find and not in another yielding two
  different expressions for the same variable, hence CryptoVerif
	   cannot use the value of the variable. If I wait until the next
  simplification pass, the other simplification via DepAnal2 is done
  first, and the problem is solved... *)

(* [count_branches test bind] counts the branches generated by
   the expansion of tuples in [let]. *)

let rec count_branches_rec = function
    (PatVar _,_)::l -> count_branches_rec l
  | _::l -> 1 + count_branches_rec l
  | [] -> 0
	
let count_branches test bind =
  let br_bind = 1 + count_branches_rec bind in
  if Terms.is_true test then br_bind else 1+br_bind
    
    
(* Simplification of terms with if/let/find/res.
   The simplifications are very similar to those performed
   on processes below. *)

let simplify_term_restr rec_simplif cur_array true_facts b p =
  let true_facts = update_elsefind_with_def [b] true_facts in
  let p' = rec_simplif cur_array true_facts p in
  Terms.build_term p' (ResE(b, p'))

let simplify_term_if rec_simplif pp cur_array true_facts t p1 p2 =
  try
    (* The facts that are true in the "else" branch *)
    let true_facts' = Facts.simplif_add (dependency_collision cur_array) true_facts (Terms.make_not t) in
    (* Simplify the "else" branch *)
    let p2' = rec_simplif cur_array true_facts' p2 in
    try
      (* The facts that are true in the "then" branch *)
      let true_facts' = Facts.simplif_add (dependency_collision cur_array) true_facts t in
      (* Simplify the "then" branch *)
      let p1' =  rec_simplif cur_array true_facts' p1 in
      Terms.build_term p1' (TestE(t, p1', p2'))
    with Contradiction ->
      Settings.changed := true;
      current_pass_transfos := (STestFalse(pp)) :: (!current_pass_transfos);
      p2'
  with Contradiction ->
    Settings.changed := true;
    current_pass_transfos := (STestTrue(pp)) :: (!current_pass_transfos);
    rec_simplif cur_array true_facts p1

let simplify_term_let rec_simplif pp cur_array true_facts pat t p1 p2 =
  let true_facts' = update_elsefind_with_def (Terms.vars_from_pat [] pat) true_facts in
  try
    let (transfos, test, bind) = Terms.simplify_let_tuple (get_tuple cur_array true_facts) pat t in
    let (test, bind) =
      (* If the expansion of tuples creates too many branches,
         do not do it. *)
      if count_branches test bind <= 3 then
	(test, bind)
      else
	(Terms.make_true(), [pat, t])
    in
    if transfos != [] then
      begin
	Settings.changed := true;
	current_pass_transfos := (SLetSimplifyPattern(pp, transfos)) :: (!current_pass_transfos);
      end;
    (* always_succeeds = true when the let never fails *)
    let always_succeeds =
      (Terms.is_true test) &&
      (match bind with
      | ((PatTuple _| PatEqual _), _)::_ -> false
      | _ -> true)
    in
    (* Simplify the process p2 if it will be used at least once *)
    let p2' =
      match p2 with
	None -> None
      | Some t3 ->
	  if always_succeeds then 
	    begin
	      Settings.changed := true;
	      current_pass_transfos := (SLetElseRemoved(pp)) :: (!current_pass_transfos);
	      None
	    end
	  else
	    begin
	      try
		let true_facts_else =
		  try
		    Facts.simplif_add (dependency_collision cur_array) true_facts (Terms.make_for_all_diff (Terms.gen_term_from_pat pat) t) 
		  with Terms.NonLinearPattern -> true_facts
		in
		Some (rec_simplif cur_array true_facts_else t3)
	      with Contradiction ->
	        (* The else branch of the let will never be executed
		   => use some constant to simplify *)
		match t3.t_desc with
		  FunApp(_,[]) -> Some t3 (* t3 is already a constant, leave it as it is *)
		| _ ->
		    Settings.changed := true;
		    current_pass_transfos := (SLetElseRemoved(pp)) :: (!current_pass_transfos);
		    Some (Terms.cst_for_type t3.t_type)
	    end
    in
    (* Simplify the process p1 *)
    let rec add_true_facts true_facts = function
	[] -> true_facts
      | (PatVar b, t)::l ->
	  add_true_facts
	    (Facts.simplif_add (dependency_collision cur_array) true_facts
	       (Terms.make_let_equal (Terms.term_from_binder b) t)) l
      | (pat, t)::l ->
	  add_true_facts
	      (Facts.simplif_add (dependency_collision cur_array) true_facts 
		 (Terms.make_equal (Terms.term_from_pat pat) t)) l
    in
    let true_facts_in = Facts.simplif_add (dependency_collision cur_array)
	(add_true_facts true_facts' bind) test
    in
    let p1' = rec_simplif cur_array true_facts_in p1 in
      (* Put the lets *)
      let plet = Terms.put_lets_term bind p1' p2' in
      (* Put the test *)
      if Terms.is_true test then
	plet
      else
	try
	  let _ = Facts.simplif_add (dependency_collision cur_array) true_facts (Terms.make_not test) in
	  Terms.build_term plet (TestE(test, plet, Terms.get_else p2'))
	with Contradiction ->
	  plet
  with
    Terms.Impossible | Contradiction ->
      Settings.changed := true;
      (*
      current_pass_transfos := (SLetSimplifyPattern(let_p, [pat, DImpossibleTuple])) :: (!current_pass_transfos); OR *)
      current_pass_transfos := (SLetRemoved(pp)) :: (!current_pass_transfos);
      rec_simplif cur_array true_facts (Terms.get_else p2)
    
exception OneBranchTerm of term findbranch

let simplify_term_find rec_simplif pp cur_array true_facts l0 t3 find_info =
  match l0 with
    [] ->
      rec_simplif cur_array true_facts t3
  | [([],def_list,t1,t2)] when
    Facts.reduced_def_list (Terms.get_facts pp) def_list = [] && 
    (match t1.t_desc with Var _ | FunApp _ -> true | _ -> false) -> 
      Settings.changed := true;
      current_pass_transfos := (SFindtoTest pp) :: (!current_pass_transfos);
      simplify_term_if rec_simplif pp cur_array true_facts t1 t2 t3
  | _ -> 
      let def_vars = Facts.get_def_vars_at pp in
      let t3' = 
	try
	  rec_simplif cur_array (add_elsefind (dependency_collision cur_array) def_vars true_facts l0) t3
	with Contradiction ->
	  (* The else branch of the find will never be executed
             => use some constant to simplify *)
	  match t3.t_desc with
	    FunApp(_,[]) -> t3 (* t3 is already a constant, leave it as it is *)
	  | _ ->
	      Settings.changed := true;
	      current_pass_transfos := (SFindElseRemoved(pp)) :: (!current_pass_transfos);
	      Terms.cst_for_type t3.t_type
      in
      let rec simplify_findl = function
	  [] -> []
	| (bl, def_list, t1, t2)::l ->
	    begin
	    let l' = simplify_findl l in
	    let vars = List.map fst bl in
	    let repl_indices = List.map snd bl in
	    let cur_array_cond = repl_indices @ cur_array in
	    let vars_terms = List.map Terms.term_from_binder vars in
	    try
	      let this_branch_node = Facts.get_initial_history pp in 
	      let def_list' = Facts.reduced_def_list (Terms.get_facts pp) def_list in
	      let def_vars_cond = Facts.def_vars_from_defined this_branch_node def_list' in
	      let true_facts = update_elsefind_with_def vars true_facts in
	      let facts_def_list = Facts.facts_from_defined this_branch_node def_list in
	      let true_facts_t1 = Facts.simplif_add_list (dependency_collision cur_array_cond) true_facts facts_def_list in
	      let facts_from_elsefind_facts =
		if !Settings.elsefind_facts_in_simplify then
		  let def_vars_cond' = Terms.union_binderref def_vars_cond def_vars in
		  Simplify1.get_facts_of_elsefind_facts (!whole_game) cur_array_cond true_facts_t1 def_vars_cond'
		else
		  []
	      in
	      let true_facts_t1 = Facts.simplif_add_list (dependency_collision cur_array_cond) true_facts_t1 facts_from_elsefind_facts in
	      (* Set priorities of variables defined by this find, 
	         to orient rewrite rules preferably in the direction
	         b[..] -> t where b \in bl *)
	      incr current_max_priority;
	      List.iter (fun b -> 
		b.priority <- (!current_max_priority); 
		priority_list := b :: (!priority_list)) vars;
	      let (t1', facts_cond) =
		match t1.t_desc with
		  Var _ | FunApp _ ->
		    let t1' = if prove_true cur_array_cond true_facts_t1 t1 then Terms.make_true() else t1 in
		    (t1', t1' :: facts_from_elsefind_facts @ facts_def_list)
		| _ -> 
		    (t1, facts_from_elsefind_facts @ facts_def_list)
	      in

	      (* [facts_cond] contains the facts that hold,
		 using repl_indices as indices. We substitute vars from them to obtain
		 the facts that hold in the then branch.*)
	      let facts_cond' = List.map (Terms.subst repl_indices vars_terms) facts_cond in
	      let tf' = Facts.simplif_add_list (dependency_collision cur_array) true_facts facts_cond' in

	      (* Check that the "defined" conditions can hold,
		 if not remove the branch *)
	      (* [def_vars_cond] contains the variables that are certainly defined 
		 using repl_indices as indices. We substitute vars from them to obtain
		 the variables certainly defined in the then branch. *)
	      let def_vars_accu = Terms.subst_def_list repl_indices vars_terms def_vars_cond in
	      (* [Terms.def_list_at_pp_fact] adds facts inferred from the knowledge 
		 that the variables in [def_vars_accu] are defined
	         at the current program point *)
	      let cur_array_term = List.map Terms.term_from_repl_index cur_array in
	      let new_facts = Terms.def_list_at_pp_facts [] (DTerm t2) cur_array_term def_vars_accu in
	      let tf' = Facts.simplif_add_list (dependency_collision cur_array) tf' new_facts in
	      (* [Terms.both_def_list_facts] adds facts inferred from the knowledge
		 that all variables in [def_vars] and [def_vars_accu] are
		 simultaneously defined. *)
	      let tf' = 
		if !Settings.detect_incompatible_defined_cond then
		  let new_facts = Terms.both_def_list_facts [] def_vars def_vars_accu in
		  Facts.simplif_add_list (dependency_collision cur_array) tf' new_facts 
		else tf'
	      in
	      let def_vars' = 
		(* Using def_vars_accu instead of def_list' is more precise *)
	        def_vars_accu @ def_vars
	      in
	      let tf' = convert_elsefind (dependency_collision cur_array) def_vars' tf' in
	      let t2' = rec_simplif cur_array tf' t2 in

	      (* Update the defined condition *)
	      let (bl', def_list3, t1', t2') as find_branch = 
		Facts.update_def_list_term def_vars def_vars_cond bl def_list' t1' t2' in
              if List.length def_list3 < List.length def_list then
                begin
                  Settings.changed := true;
                  current_pass_transfos := (SFindDeflist(pp, def_list, def_list3)) :: (!current_pass_transfos)
		end
              else if not (Facts.eq_deflists def_list def_list3)  then
		current_pass_transfos := (SFindDeflist(pp, def_list, def_list3)) :: (!current_pass_transfos);

              (* If the find is marked "unique", and we can prove that
	         the current branch succeeds, keep only that branch *)
              if (!Settings.unique_branch) &&
		(match t1'.t_desc with
		  Var _ | FunApp _ -> true
		| _ -> false)
	      then 
		try 
		  branch_succeeds find_branch (dependency_collision cur_array_cond) true_facts def_vars;
		  find_branch :: l'
		with SuccessBranch(subst, keep_bl) ->
		  (* If the find has a single branch, which always succeeds, and the
	             indices defined by the find are not used, we can remove
	             the find, keeping only its then branch *)
		  if ((find_info == Unique) || (List.length l0 = 1)) && 
		    (not (List.exists (fun b -> Terms.has_array_ref_q b || Terms.refers_to b t2') (List.map fst bl'))) then
		    begin
		      let def_list4 = filter_deflist_indices bl' def_list3 in
		      if (bl' != []) && (def_list4 != []) && (List.length l0 = 1) 
                          (* When def_list4 == [] or List.length l0 > 1, the change is recorded below *) then
			begin
			  Settings.changed := true;
			  current_pass_transfos := (SFindSingleBranch(pp,(bl', def_list3, t1', DTerm t2'))) :: (!current_pass_transfos);
			end;
		      raise (OneBranchTerm([], def_list4, Terms.make_true(), t2'))
		    end
		  else if not (find_info == Unique) then 
		    find_branch :: l' 
		  else
		    begin
		      (* Subst is a substitution for replication indices,
			 compute the substitution for the corresponding variables *)
		      let subst_repl_indices_source = List.map (fun (_,b,_) -> b) subst in
		      let subst_repl_indices_target = List.map (fun (_,_,b_im) -> b_im) subst in
		      let subst = List.map (fun (b, _, b_im) -> 
			(b, Terms.subst repl_indices vars_terms b_im)
			  ) subst 
		      in
		      if List.exists (fun (b, b_im) -> 
			List.exists (fun (b', b_im') -> Terms.refers_to b b_im') subst
			  ) subst
		      then raise (OneBranchTerm(find_branch));
		      if subst != [] then 
			begin
			  Settings.changed := true;
			  current_pass_transfos := (SFindIndexKnown(pp, (bl, def_list, t1, DTerm t2), subst)) :: (!current_pass_transfos)
			end;
		      let def_list_tmp = ref [] in
		      List.iter (fun br ->
			Terms.get_deflist_subterms def_list_tmp 
			  (Terms.subst subst_repl_indices_source subst_repl_indices_target (Terms.term_from_binderref br))) def_list3;
		      let def_list3 = !def_list_tmp in 
		      let t1' = Terms.update_args_at_creation ((List.map snd keep_bl) @ cur_array) 
			  (Terms.subst subst_repl_indices_source subst_repl_indices_target t1') in
		      let t2' = add_let_term (Terms.subst3 subst t2') subst in
		      raise (OneBranchTerm(keep_bl, def_list3, t1', t2'))
		    end
	      else
		find_branch :: l'

	    with Contradiction ->
	      Settings.changed := true;
	      current_pass_transfos := (SFindBranchRemoved(pp,(bl, def_list, t1, DTerm t2))) :: (!current_pass_transfos);
	      l'
	    end
      in
      try 
	let l0' = simplify_findl l0 in
	if l0' == [] then
	  begin
	    Settings.changed := true;
	    current_pass_transfos := (SFindRemoved(pp)) :: (!current_pass_transfos);
	    t3'
	  end
	else
	  let find_info = is_unique l0' find_info in
	  Terms.build_term t3' (FindE(l0', t3',find_info))
      with OneBranchTerm(find_branch) ->
	match find_branch with
	  ([],[],t1,t2) -> 
	    Settings.changed := true;
	    current_pass_transfos := (SFindSingleBranch(pp,([],[],t1,DTerm t2))) :: (!current_pass_transfos);
	    t2
	| (bl,def_list,t1,t2) ->
            (* The else branch of the find will never be executed
               => use some constant to simplify *)
	    let t3'' = 
	      match t3'.t_desc with
		FunApp(_,[]) -> t3'
	      |	_ -> Terms.cst_for_type t3'.t_type 
	    in
	    if List.length l0 > 1 then 
	      begin
		Settings.changed := true;
		current_pass_transfos := (SFindSingleBranch(pp,(bl,def_list,t1,DTerm t2))) :: (!current_pass_transfos)
	      end
	    else if not (Terms.equal_terms t3' t3'') then
	      begin
		Settings.changed := true;
		current_pass_transfos := (SFindElseRemoved(pp)) :: (!current_pass_transfos)
	      end;
	    Terms.build_term t3'' (FindE([find_branch], t3'',find_info))

(* Expand term to term. Useful for conditions of find when they cannot be expanded to processes.
   Guarantees the invariant that if/let/find/res terms occur only in
   - conditions of find
   - at [ ] positions in TestE(t,[then],[else]), ResE(b,[...]), LetE(b,t,[in],[else]), 
     FindE((bl,def_list,[cond],[then]) list,[else])

   context = fun term -> C[term]
*)

let rec pseudo_expand_term (cur_array: Types.repl_index list) true_facts t context =
  let pseudo_expand_term_rec cur_array true_facts t =
    pseudo_expand_term cur_array true_facts t context
  in
  match t.t_desc with
    Var(b,l) ->
      pseudo_expand_term_list cur_array true_facts l
	(fun cur_array true_facts li ->
	  context cur_array true_facts (Terms.build_term t (Var(b,li))))
  | ReplIndex _ -> context cur_array true_facts t
  | FunApp(f,l) ->
      pseudo_expand_term_list cur_array true_facts l
	(fun cur_array true_facts li ->
	  context cur_array true_facts (Terms.build_term t (FunApp(f,li))))
  | TestE(t1,t2,t3) ->
      pseudo_expand_term cur_array true_facts t1
	(fun cur_array true_facts t1i ->
	  (* Equivalent to TestE(t1i,  t2, t3) with simplification
	     and expansion of t2 and t3 *)
	  simplify_term_if pseudo_expand_term_rec (DTerm t) cur_array true_facts t1i t2 t3)
  | LetE(pat, t1, t2, topt) ->
      pseudo_expand_term cur_array true_facts t1 (fun cur_array true_facts t1i ->
	pseudo_expand_pat cur_array true_facts pat (fun cur_array true_facts pati ->
	  (* Equivalent to LetE(pati, t1i, t2, topt) with simplification
	     and expansion of t2 and topt *)
	  simplify_term_let pseudo_expand_term_rec (DTerm t) cur_array true_facts
	    pati t1i t2 topt))
  | FindE(l0, t3, find_info) ->
      let rec expand_cond_find_list cur_array true_facts l context =
	match l with
	  [] -> context cur_array true_facts []
	| ((bl, def_list, t1, t2)::restl) ->
	    List.iter check_no_ifletfind_br def_list;
                  (* I move something outside a condition of
                     "find" only when bl and def_list are empty.  
                     I could be more precise, I would need to
                     check not only that what I move out does not
                     refer to the indices of "find", but also that it
                     is does not refer to the variables in the
                     "defined" condition---otherwise, some variable
                     accesses may not be defined after the
                     transformation *)
            if bl != [] || def_list != [] then
	      expand_cond_find_list cur_array true_facts restl (fun cur_array true_facts li ->
		context cur_array true_facts ((bl, def_list, final_pseudo_expand cur_array true_facts t1, t2)::li))
	    else
	      pseudo_expand_term cur_array true_facts t1 (fun cur_array true_facts t1i ->
		expand_cond_find_list cur_array true_facts restl (fun cur_array true_facts li ->
		  context cur_array true_facts ((bl, def_list, t1i, t2)::li)))
      in
      expand_cond_find_list cur_array true_facts l0 (fun cur_array true_facts l1i ->
	(* Equivalent to FindE(l1i, t3, find_info) after expansion
	   of the results in l1i and of t3 *)
	simplify_term_find pseudo_expand_term_rec (DTerm t) cur_array true_facts
	   l1i t3 find_info)
  | ResE(b, t) ->
      (* Equivalent to ResE(b, t) after simplification and expansion
	 of t *)
      simplify_term_restr pseudo_expand_term_rec cur_array true_facts b t
  | EventAbortE _ | EventE _ ->
      Parsing_helper.internal_error "Events should not occur in conditions of find before expansion"
  | GetE _ | InsertE _ ->
      Parsing_helper.internal_error "Get/Insert should not appear in Transf_expand.pseudo_expand_term"

and pseudo_expand_term_list cur_array true_facts l context =
  match l with
    [] -> context cur_array true_facts []
  | (a::l) -> 
      pseudo_expand_term cur_array true_facts a (fun cur_array true_facts a' ->
	pseudo_expand_term_list cur_array true_facts l (fun cur_array true_facts l' ->
	  context cur_array true_facts (a'::l')))

and pseudo_expand_pat cur_array true_facts pat context =
  match pat with
    PatVar b -> context cur_array true_facts (PatVar b)
  | PatTuple (ft,l) ->
      pseudo_expand_pat_list cur_array true_facts l (fun cur_array true_facts li ->
	context cur_array true_facts (PatTuple (ft,li)))
  | PatEqual t ->
      pseudo_expand_term cur_array true_facts t (fun cur_array true_facts ti ->
	context cur_array true_facts (PatEqual ti))

and pseudo_expand_pat_list cur_array true_facts l context =
  match l with
    [] -> context cur_array true_facts []
  | (a::l) -> 
      pseudo_expand_pat cur_array true_facts a (fun cur_array true_facts a' ->
	pseudo_expand_pat_list cur_array true_facts l (fun cur_array true_facts l' ->
	  context cur_array true_facts (a'::l')))
	
and final_pseudo_expand cur_array true_facts t =
  pseudo_expand_term cur_array true_facts t (fun cur_array true_facts t -> t)


(* Simplification of processes *)

let simplify_restr rec_simplif cur_array true_facts b p =
  let true_facts = update_elsefind_with_def [b] true_facts in
  let p' = rec_simplif cur_array true_facts p in
  Terms.oproc_from_desc (Restr(b, p'))

let simplify_if rec_simplif pp cur_array true_facts t p1 p2 =
  try
    (* The facts that are true in the "else" branch *)
    let true_facts' = Facts.simplif_add (dependency_collision cur_array) true_facts (Terms.make_not t) in
    (* Simplify the "else" branch *)
    let p2' = rec_simplif cur_array true_facts' p2 in
    try
      (* The facts that are true in the "then" branch *)
      let true_facts' = Facts.simplif_add (dependency_collision cur_array) true_facts t in
      (* Simplify the "then" branch *)
      let p1' =  rec_simplif cur_array true_facts' p1 in
      (* Merge "if t then yield else yield" *)
      if (p1'.p_desc == Yield) && (p2'.p_desc == Yield) then 
	begin
	  Settings.changed := true;
	  current_pass_transfos := (STestMerge(pp)) :: (!current_pass_transfos);
	  Terms.oproc_from_desc Yield
	end
      else
	Terms.oproc_from_desc (Test(t, p1', p2'))
    with Contradiction ->
      Settings.changed := true;
      current_pass_transfos := (STestFalse(pp)) :: (!current_pass_transfos);
      p2'
  with Contradiction ->
    Settings.changed := true;
    current_pass_transfos := (STestTrue(pp)) :: (!current_pass_transfos);
    rec_simplif cur_array true_facts p1

let is_yield_proc p = (p.p_desc == Yield)

let is_yield_term_opt = function
    None -> true
  | Some _ -> false

let is_yield_term t = false
      
let simplify_let rec_simplif1 rec_simplif2 is_yield2 pp cur_array true_facts pat t p1 p2 =
  let true_facts' = update_elsefind_with_def (Terms.vars_from_pat [] pat) true_facts in
  try
    let (transfos, test, bind) = Terms.simplify_let_tuple (get_tuple cur_array true_facts) pat t in
    let (test, bind) =
      (* If the expansion of tuples creates too many branches,
         do not do it. *)
      if count_branches test bind <= 3 then
	(test, bind)
      else
	(Terms.make_true(), [pat, t])
    in
    if transfos != [] then
      begin
	Settings.changed := true;
	current_pass_transfos := (SLetSimplifyPattern(pp, transfos)) :: (!current_pass_transfos);
      end;
    (* always_succeeds = true when the let never fails *)
    let always_succeeds =
      (Terms.is_true test) &&
      (match bind with
      | ((PatTuple _| PatEqual _), _)::_ -> false
      | _ -> true)
    in
    (* Simplify the process p2 if it will be used at least once *)
    let p2' =
      if is_yield2 p2 then
	Terms.oproc_from_desc Yield
      else if always_succeeds then 
	begin
	  Settings.changed := true;
	  current_pass_transfos := (SLetElseRemoved(pp)) :: (!current_pass_transfos);
	  Terms.oproc_from_desc Yield
	end
      else
	begin
	  try
	    let true_facts_else =
	      try
		Facts.simplif_add (dependency_collision cur_array) true_facts (Terms.make_for_all_diff (Terms.gen_term_from_pat pat) t) 
	      with Terms.NonLinearPattern -> true_facts
	    in
	    rec_simplif2 cur_array true_facts_else p2
	  with Contradiction ->
	    Settings.changed := true;
	    current_pass_transfos := (SLetElseRemoved(pp)) :: (!current_pass_transfos);
	    Terms.oproc_from_desc Yield
	end
    in
    (* Simplify the process p1 *)
    let rec add_true_facts true_facts = function
	[] -> true_facts
      | (PatVar b, t)::l ->
	  add_true_facts
	    (Facts.simplif_add (dependency_collision cur_array) true_facts
	       (Terms.make_let_equal (Terms.term_from_binder b) t)) l
      | (pat, t)::l ->
	  add_true_facts
	      (Facts.simplif_add (dependency_collision cur_array) true_facts 
		 (Terms.make_equal (Terms.term_from_pat pat) t)) l
    in
    let true_facts_in = Facts.simplif_add (dependency_collision cur_array)
	(add_true_facts true_facts' bind) test
    in
    let p1' = rec_simplif1 cur_array true_facts_in p1 in
    (* Merge if both branches are Yield and the variables are not needed *)
    if (p1'.p_desc == Yield) && (p2'.p_desc == Yield) &&
      (List.for_all (fun (pat, _) -> not (needed_vars_in_pat pat)) bind) then
      begin
	Settings.changed := true;
	current_pass_transfos := (SLetRemoved(pp)) :: (!current_pass_transfos);
	Terms.oproc_from_desc Yield
      end
    else
      (* Put the lets *)
      let plet = Terms.put_lets bind p1' p2' in
      (* Put the test *)
      if Terms.is_true test || prove_true cur_array true_facts test then
	plet
      else
	Terms.oproc_from_desc (Test(test, plet, p2'))
  with
    Terms.Impossible | Contradiction ->
      Settings.changed := true;
      (*
      current_pass_transfos := (SLetSimplifyPattern(let_p, [pat, DImpossibleTuple])) :: (!current_pass_transfos); OR *)
      current_pass_transfos := (SLetRemoved(pp)) :: (!current_pass_transfos);
      rec_simplif2 cur_array true_facts p2  

exception OneBranchProcess of process findbranch

(* All conditions of find in l0 must have already been expanded *)

let simplify_find rec_simplif is_yield get_pp pp cur_array true_facts l0 p2 find_info =
  match l0 with
    [] ->
      rec_simplif cur_array true_facts p2
  | [([],def_list,t1,p1)] when
    (Facts.reduced_def_list (Terms.get_facts pp) def_list = []) && 
    (match t1.t_desc with Var _ | FunApp _ -> true | _ -> false) -> 
      Settings.changed := true;
      current_pass_transfos := (SFindtoTest pp) :: (!current_pass_transfos);
      simplify_if rec_simplif pp cur_array true_facts t1 p1 p2
  | _ -> 
      let def_vars = Facts.get_def_vars_at pp in
      let p2' = 
	if is_yield p2 then Terms.oproc_from_desc Yield else
	try
	  rec_simplif cur_array (add_elsefind (dependency_collision cur_array) def_vars true_facts l0) p2
	with Contradiction ->
	  Settings.changed := true;
	  current_pass_transfos := (SFindElseRemoved(pp)) :: (!current_pass_transfos);
	  Terms.oproc_from_desc Yield
      in
      let rec simplify_findl l1 = 
	match l1 with
	  [] -> []
	| ((bl, def_list, t, p1)::l) ->
	    begin
	    let l' = simplify_findl l in
	    let vars = List.map fst bl in
	    let repl_indices = List.map snd bl in
	    let cur_array_cond = repl_indices @ cur_array in
	    let vars_terms = List.map Terms.term_from_binder vars in
	    try
	      let this_branch_node = Facts.get_initial_history pp in 
	      let def_list' = Facts.reduced_def_list (Terms.get_facts pp) def_list in
	      let def_vars_cond = Facts.def_vars_from_defined this_branch_node def_list' in
	      let true_facts = update_elsefind_with_def vars true_facts in
	      let facts_def_list = Facts.facts_from_defined this_branch_node def_list in
	      let true_facts_t = Facts.simplif_add_list (dependency_collision cur_array_cond) true_facts facts_def_list in
	      let facts_from_elsefind_facts =
		if !Settings.elsefind_facts_in_simplify then
		  let def_vars_cond' = Terms.union_binderref def_vars_cond def_vars in
		  Simplify1.get_facts_of_elsefind_facts (!whole_game) cur_array_cond true_facts_t def_vars_cond'
		else
		  []
	      in
	      let true_facts_t = Facts.simplif_add_list (dependency_collision cur_array_cond) true_facts_t facts_from_elsefind_facts in
	      (* Set priorities of variables defined by this find, 
	         to orient rewrite rules preferably in the direction
	         b[..] -> t where b \in bl *)
	      incr current_max_priority;
	      List.iter (fun b -> 
		b.priority <- (!current_max_priority);
		priority_list := b :: (!priority_list)) vars;
	      let (t', facts_cond) =
		match t.t_desc with
		  Var _ | FunApp _ ->
		    let t' = if prove_true cur_array_cond true_facts_t t then Terms.make_true() else t in
		    (t', t' :: facts_from_elsefind_facts @ facts_def_list)
		| _ -> 
		    (t, facts_from_elsefind_facts @ facts_def_list)
	      in

	      (* [facts_cond] contains the facts that hold,
		 using repl_indices as indices. We substitute vars from them to obtain
		 the facts that hold in the then branch.
		 Same substitution for the dependency info. *)
	      let facts_cond' = List.map (Terms.subst repl_indices vars_terms) facts_cond in
	      let tf' = Facts.simplif_add_list (dependency_collision cur_array) true_facts facts_cond' in

	      (* Check that the "defined" conditions can hold,
		 if not remove the branch *)
	      (* [def_vars_cond] contains the variables that are certainly defined 
		 using repl_indices as indices. We substitute vars from them to obtain
		 the variables certainly defined in the then branch. *)
	      let def_vars_accu = Terms.subst_def_list repl_indices vars_terms def_vars_cond in
	      (* [Terms.def_list_at_pp_facts] adds facts inferred from the knowledge 
		 that the variables in [def_vars_accu] are defined
	         at the current program point *)
	      let cur_array_term = List.map Terms.term_from_repl_index cur_array in
	      let new_facts = Terms.def_list_at_pp_facts [] (get_pp p1) cur_array_term def_vars_accu in
	      let tf' = Facts.simplif_add_list (dependency_collision cur_array) tf' new_facts in
	      (* [Terms.both_def_list_facts] adds facts inferred from the knowledge
		 that all variables in [def_vars] and [def_vars_accu] are
		 simultaneously defined. *)
	      let tf' = 
		if !Settings.detect_incompatible_defined_cond then
		  let new_facts = Terms.both_def_list_facts [] def_vars def_vars_accu in
		  Facts.simplif_add_list (dependency_collision cur_array) tf' new_facts 
		else tf'
	      in
	      let def_vars' = 
		(* Using def_vars_accu instead of def_list' is more precise *)
		def_vars_accu @ def_vars
	      in
	      let tf' = convert_elsefind (dependency_collision cur_array) def_vars' tf' in
	      
	      let p1' = rec_simplif cur_array tf' p1 in

	      (* Update the defined condition *)
	      let (bl', def_list3, t', p1') as find_branch = 
		Facts.update_def_list_process def_vars def_vars_cond bl def_list' t' p1' in
              if List.length def_list3 < List.length def_list then
                begin
                  Settings.changed := true;
                  current_pass_transfos := (SFindDeflist(pp, def_list, def_list3)) :: (!current_pass_transfos)
		end
              else if not (Facts.eq_deflists def_list def_list3)  then
		current_pass_transfos := (SFindDeflist(pp, def_list, def_list3)) :: (!current_pass_transfos);

              (* If the find is marked "unique", and we can prove that
	         the current branch succeeds, keep only that branch *)
              if (!Settings.unique_branch) &&
		(match t'.t_desc with
		  Var _ | FunApp _ -> true
		| _ -> false)
	      then 
		try
		  branch_succeeds find_branch (dependency_collision cur_array_cond) true_facts def_vars;
		  find_branch :: l'
		with SuccessBranch(subst, keep_bl) ->
		  (* If the find has a single branch, which always succeeds, and the
	             indices defined by the find are not used, we can remove
	             the find, keeping only its then branch *)
		  if ((find_info == Unique) || (List.length l0 = 1)) && 
		    (not (List.exists (fun b -> Terms.has_array_ref_q b || Terms.refers_to_oprocess b p1') (List.map fst bl'))) then
		    begin
		      let def_list4 = filter_deflist_indices bl' def_list3 in
		      if (bl' != []) && (is_yield p2) && (def_list4 != []) && (List.length l0 = 1) 
                          (* When p2.p_desc != Yield or def_list4 == [] or List.length l0 > 1, the change is recorded below *) then
			begin
			  Settings.changed := true;
			  current_pass_transfos := (SFindSingleBranch(pp,(bl', def_list3, t', DProcess p1'))) :: (!current_pass_transfos);
			end;
		      raise (OneBranchProcess([], def_list4, Terms.make_true(), p1'))
		    end
		  else if not (find_info == Unique) then 
		    find_branch :: l' 
		  else
	            begin
		      (* Subst is a substitution for replication indices,
		         compute the substitution for the corresponding variables *)
		      let subst_repl_indices_source = List.map (fun (_,b,_) -> b) subst in
		      let subst_repl_indices_target = List.map (fun (_,_,b_im) -> b_im) subst in
		      let subst = List.map (fun (b, _, b_im) -> 
			(b, Terms.subst repl_indices vars_terms b_im)
			  ) subst 
		      in
		      if List.exists (fun (b, b_im) -> 
			(List.exists (fun (b', b_im') -> Terms.refers_to b b_im') subst)
			  ) subst
		      then raise (OneBranchProcess(find_branch));
		      if subst != [] then 
			begin
			  Settings.changed := true;
			  current_pass_transfos := (SFindIndexKnown(pp, (bl, def_list, t, DNone(*Not used*)), subst)) :: (!current_pass_transfos)
			end;
		      let def_list_tmp = ref [] in
		      List.iter (fun br ->
			Terms.get_deflist_subterms def_list_tmp 
			  (Terms.subst subst_repl_indices_source subst_repl_indices_target (Terms.term_from_binderref br))) def_list3;
		      let def_list3 = !def_list_tmp in 
		      let t' = Terms.update_args_at_creation ((List.map snd keep_bl) @ cur_array) 
			  (Terms.subst subst_repl_indices_source subst_repl_indices_target t') in
		      let p1' = add_let (Terms.subst_oprocess3 subst p1') subst in
		      raise (OneBranchProcess(keep_bl, def_list3, t', p1'))
		    end
	      else
		find_branch :: l'

	      (*let t_or_and = Terms.or_and_form t' in
	      simplify_find true_facts' l' bl def_list' t_or_and p1 *)
	    with Contradiction ->
	      Settings.changed := true;
	      current_pass_transfos := (SFindBranchRemoved(pp,(bl, def_list, t, DNone(*Not needed*)))) :: (!current_pass_transfos);
	      l'
	    end
      in
      try
	let l0' = simplify_findl l0 in
	if l0' == [] then
	  begin
	    Settings.changed := true;
	    current_pass_transfos := (SFindRemoved(pp)) :: (!current_pass_transfos);
	    p2'
	  end
	else
	  begin
	    if (p2'.p_desc == Yield) && (List.for_all (fun (bl,_,t,p1) ->
	      (p1.p_desc == Yield) && (not (List.exists Terms.has_array_ref_q (List.map fst bl)))
		) l0') then
	      begin
		Settings.changed := true;
		current_pass_transfos := (SFindRemoved(pp)) :: (!current_pass_transfos);
		Terms.oproc_from_desc Yield
	      end
	    else
	      let find_info = is_unique l0' find_info in
	      Terms.oproc_from_desc (Find(l0', p2', find_info))
	  end
      with OneBranchProcess(find_branch) ->
	match find_branch with
	  ([],[],t1,p1) -> 
	    Settings.changed := true;
	    current_pass_transfos := (SFindSingleBranch(pp,([],[],t1,DProcess p1))) :: (!current_pass_transfos);
	    p1
	| (bl,def_list,t1,p1) ->
	    (* the else branch of the find will never be executed *)
	    if (List.length l0 > 1) || (not (is_yield p2)) then 
	      begin
		Settings.changed := true;
		current_pass_transfos := (SFindSingleBranch(pp,(bl,def_list,t1,DProcess p1))) :: (!current_pass_transfos);
	      end;
	    Terms.oproc_from_desc (Find([find_branch], Terms.oproc_from_desc Yield, find_info))
  

	
let simplify_event rec_simplif cur_array true_facts t p = 
  match t.t_desc with
    FunApp(f,_) ->
      if not (Settings.event_occurs_in_queries f (!whole_game).current_queries) then
	rec_simplif cur_array true_facts p
      else
	Terms.oproc_from_desc (EventP(t, rec_simplif cur_array true_facts p))
  | _ ->
    Parsing_helper.internal_error "Events must be function applications"

let simplify_output rec_simplif cur_array true_facts (c,tl) t2 p =
  (* Remove all "Elsefind" facts since variables may be defined 
     between the output and the following input *)
  let (subst, facts, _) = true_facts in
  let true_facts' = (subst, facts, []) in
  Terms.oproc_from_desc 
    (Output((c, tl), t2, rec_simplif cur_array true_facts' p))


(* Expand term to process *)

let rec expand_term cur_array true_facts t context =
  let expand_term_rec cur_array true_facts t =
    expand_term cur_array true_facts t context
  in
  match t.t_desc with
    Var(b,l) ->
      expand_term_list cur_array true_facts  l
	(fun cur_array true_facts li ->
	  context cur_array true_facts (Terms.build_term t (Var(b,li))))
  | ReplIndex _ -> context cur_array true_facts t
  | FunApp(f,l) ->
      expand_term_list cur_array true_facts l
	(fun cur_array true_facts li ->
	  context cur_array true_facts (Terms.build_term t (FunApp(f,li))))
  | TestE(t1,t2,t3) ->
      expand_term cur_array true_facts t1
	(fun cur_array true_facts t1i ->
	  (* Equivalent to Test(t1i,  t2, t3) with simplification
	     and expansion of t2 and t3 *)
	  simplify_if expand_term_rec (DTerm t) cur_array true_facts t1i t2 t3)

  | LetE(pat, t1, t2, topt) ->
      let expand_term_opt_rec cur_array true_facts = function
	  None -> Terms.oproc_from_desc Yield
	| Some t3 -> expand_term cur_array true_facts t3 context
      in
      expand_term cur_array true_facts t1 (fun cur_array true_facts t1i ->
	expand_pat cur_array true_facts pat (fun cur_array true_facts pati ->
	  (* Equivalent to Let(pati, t1i, t2, topt) with simplification
	     and expansion of t2 and topt *)
	    simplify_let expand_term_rec expand_term_opt_rec
	    is_yield_term_opt (DTerm t) cur_array true_facts pati t1i t2 topt))

  | FindE(l0, t3, find_info) ->
      let rec expand_cond_find_list cur_array true_facts l context =
	match l with
	  [] -> context cur_array true_facts []
	| ((bl, def_list, t1, t2)::restl) ->
	    List.iter check_no_ifletfind_br def_list;
                  (* I move something outside a condition of
                     "find" only when bl and def_list are empty.  
                     I could be more precise, I would need to
                     check not only that what I move out does not
                     refer to the indices of "find", but also that it
                     is does not refer to the variables in the
                     "defined" condition---otherwise, some variable
                     accesses may not be defined after the
                     transformation *)
            if bl != [] || def_list != [] then
	      expand_cond_find_list cur_array true_facts restl (fun cur_array true_facts li ->
		context cur_array true_facts ((bl, def_list, final_pseudo_expand cur_array true_facts t1, t2)::li))
	    else
	      expand_term cur_array true_facts t1 (fun cur_array true_facts t1i ->
		 expand_cond_find_list cur_array true_facts restl (fun cur_array true_facts li -> context cur_array true_facts ((bl, def_list, t1i, t2)::li)))
      in
      expand_cond_find_list cur_array true_facts l0 (fun cur_array true_facts l1i ->
	(* Equivalent to Find(l1i, t3, find_info) after expansion
	   of the results in l1i and of t3 *)
	simplify_find expand_term_rec is_yield_term (fun t -> DTerm t) (DTerm t) cur_array true_facts
	   l1i t3 find_info)
  | ResE(b, t) ->
      (* Equivalent to Restr(b, t) after simplification and expansion
	 of t *)
      simplify_restr expand_term_rec cur_array true_facts b t
  | EventAbortE(f) ->
      (* The event is expanded to a process that stops just after the event.
	 Events in terms are used only in the RHS of equivalences, and 
	 one includes their probability of execution in the probability of
	 breaking the protocol. *)
      Terms.oproc_from_desc (EventAbort f)
  | EventE(t,p) ->
      simplify_event expand_term_rec cur_array true_facts t p
  | GetE _ | InsertE _ ->
      Parsing_helper.internal_error "Get/Insert should not appear in Transf_expand.expand_term"

and expand_term_list cur_array true_facts l context =
  match l with
    [] -> context cur_array true_facts []
  | (a::l) -> 
      expand_term cur_array true_facts a (fun cur_array true_facts a' ->
	expand_term_list cur_array true_facts l (fun cur_array true_facts l' ->
	  context cur_array true_facts (a'::l')))

and expand_pat cur_array true_facts pat context =
  match pat with
    PatVar b -> context cur_array true_facts (PatVar b)
  | PatTuple (ft,l) ->
      expand_pat_list cur_array true_facts l (fun cur_array true_facts li ->
	context cur_array true_facts (PatTuple (ft,li)))
  | PatEqual t ->
      expand_term cur_array true_facts t (fun cur_array true_facts ti ->
	context cur_array true_facts (PatEqual ti))

and expand_pat_list cur_array true_facts l context =
  match l with
    [] -> context cur_array true_facts []
  | (a::l) -> 
      expand_pat cur_array true_facts a (fun cur_array true_facts a' ->
	expand_pat_list cur_array true_facts l (fun cur_array true_facts l' ->
	  context cur_array true_facts (a'::l')))


(* Expand process to process *)

let rec expand_process cur_array true_facts p' = 
  match p'.i_desc with
    Nil -> Terms.iproc_from_desc Nil
  | Par(p1,p2) ->
      Terms.iproc_from_desc  (Par(expand_process cur_array true_facts p1,
				  expand_process cur_array true_facts p2))
  | Repl(b,p) ->
      Terms.iproc_from_desc (Repl(b, expand_process (b::cur_array) true_facts p))
  | Input((c,tl),pat,p) ->
      List.iter check_no_ifletfind tl;
      begin
	match true_facts with
	  (_,_,[]) -> ()
	| _ -> Parsing_helper.internal_error "There should be no elsefind facts at inputs"
      end;
      if need_expand_pat pat then
	begin
	  Settings.changed := true;
	  let b = Terms.create_binder "patv" (Terms.new_vname()) 
	      (Terms.get_type_for_pattern pat) cur_array
	  in
	  let bterm = Terms.term_from_binder b in
	  Terms.iproc_from_desc
	    (Input((c,tl),PatVar b, 
		   expand_pat cur_array true_facts pat (fun cur_array true_facts pati ->
		     (* Equivalent to Let(pati, bterm, p', Yield) *)
		     simplify_let expand_oprocess expand_oprocess
		       is_yield_proc (DInputProcess p') cur_array true_facts
		       pati bterm p (Terms.oproc_from_desc Yield))))
	end
      else
        Terms.iproc_from_desc (Input((c,tl),pat,
				     expand_oprocess cur_array true_facts p))

and expand_oprocess cur_array true_facts p =
  match p.p_desc with 
    Yield -> Terms.oproc_from_desc Yield
  | EventAbort f -> Terms.oproc_from_desc (EventAbort f)
  | Restr(b,p) ->
      (* Equivalent to Restr(b, p) *)
      simplify_restr expand_oprocess cur_array true_facts b p
  | Test(t,p1,p2) ->
      if need_expand t then
	begin
	  Settings.changed := true;
	  expand_term cur_array true_facts t (fun cur_array true_facts ti ->
	    simplify_if expand_oprocess (DProcess p) cur_array true_facts ti p1 p2)
	end
      else
	simplify_if expand_oprocess (DProcess p) cur_array true_facts t p1 p2
  | Find(l0, p2, find_info) ->
      let rec expand_find_list cur_array true_facts next_f = function
	  [] -> next_f cur_array true_facts []
	| ((bl, def_list, t, p1)::rest_l) ->
	    List.iter check_no_ifletfind_br def_list;
	    if need_expand t then
	      begin
		Settings.changed := true;
		if bl != [] || def_list != [] then
		  expand_find_list cur_array true_facts (fun cur_array true_facts rest_l' ->
		    next_f cur_array true_facts ((bl, def_list, final_pseudo_expand cur_array true_facts t, p1)::rest_l')) rest_l
		else
		  expand_term cur_array true_facts t (fun cur_array true_facts ti ->
		    expand_find_list cur_array true_facts (fun cur_array true_facts rest_l' ->
		      next_f cur_array true_facts ((bl, def_list, ti, p1)::rest_l')) rest_l)
	      end
	    else
	      expand_find_list cur_array true_facts (fun cur_array true_facts rest_l' ->
		next_f cur_array true_facts ((bl, def_list, t, p1)::rest_l')) rest_l
      in
      expand_find_list cur_array true_facts (fun cur_array true_facts l0' ->
	simplify_find expand_oprocess is_yield_proc (fun p -> DProcess p) (DProcess p)
	  cur_array true_facts l0' p2 find_info) l0
  | Output((c,tl),t2,p) ->
      if (List.exists need_expand tl) || (need_expand t2) then
	begin
	  Settings.changed := true;
	  expand_term_list cur_array true_facts tl (fun cur_array true_facts tli ->
	    expand_term cur_array true_facts t2 (fun cur_array true_facts t2i ->
	      simplify_output expand_process cur_array true_facts (c,tli) t2i p))
	end
      else
	simplify_output expand_process cur_array true_facts (c,tl) t2 p
  | Let(pat,t,p1,p2) ->
      if (need_expand_pat pat) || (need_expand t) then
	begin
	  Settings.changed := true;
	  expand_term cur_array true_facts t (fun cur_array true_facts ti ->
	    expand_pat cur_array true_facts pat (fun cur_array true_facts pati ->
	      simplify_let expand_oprocess expand_oprocess is_yield_proc (DProcess p)
		cur_array true_facts pati ti p1 p2))
	end
      else
	simplify_let expand_oprocess expand_oprocess is_yield_proc (DProcess p)
	  cur_array true_facts pat t p1 p2
  | EventP(t,p) ->
      if need_expand t then
	begin
	  Settings.changed := true;
	  expand_term cur_array true_facts t (fun cur_array true_facts ti ->
	    simplify_event expand_oprocess cur_array true_facts ti p)
	end
      else
	simplify_event expand_oprocess cur_array true_facts t p
  | Get _|Insert _ -> Parsing_helper.internal_error "Get/Insert should not appear here"


let expand_main g =
  reset [] g;
  current_pass_transfos := [];
  Terms.array_ref_process g.proc;
  Simplify1.improved_def_process None true g.proc;
  let p' = expand_process [] ([],[],[]) g.proc in
  let current_transfos = !current_pass_transfos in
  current_pass_transfos := [];
  Terms.cleanup_array_ref();
  let proba = final_add_proba() in
  Simplify1.empty_improved_def_process true g.proc;
  whole_game := Terms.empty_game;
  ({ proc = p'; game_number = -1; current_queries = g.current_queries },
   proba, [DSimplify(current_transfos)])
    
(* Main function for expansion of if and find
   Call auto_sa_rename after expand_process, so that facts associated with 
   nodes are emptied, and variables defined in
   conditions of find have a single definition. 
   Expansion is called only when there is no advice pending, so we can simply 
   ignore the list of done SArenamings.
*)

let expand_process g =
  print_string "SA renaming... "; flush stdout;
  let (g0, proba0, ins0) = Transf_auto_sa_rename.auto_sa_rename g in
  print_string "Simplifying... "; flush stdout;
  let tmp_changed = !Settings.changed in
  let (g1, proba1, ins1) = simplify_process g0 in
  g1.proc <- Terms.move_occ_process g1.proc;
  print_string "Expanding and simplifying... "; flush stdout;
  let (g2, proba2, ins2) = expand_main g1 in
  if !Settings.changed then 
    begin
      print_string "SA renaming... "; flush stdout;
      let (g3, proba3, ins3) = Transf_auto_sa_rename.auto_sa_rename g2 in
      (g3, proba3 @ proba2 @ proba1 @ proba0, ins3 @ (DExpandIfFind :: ins2) @ins1 @ ins0)
    end
  else
    begin
      Settings.changed := tmp_changed;
      (g0, proba0, ins0)
    end
    
