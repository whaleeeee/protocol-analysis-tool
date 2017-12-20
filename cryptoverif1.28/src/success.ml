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

(* check_usage can return the following results:
- raise Not_found, when secrecy cannot be proved, even by applying
  advised transformations
- add transformations to "advise", when secrecy cannot be proved
  in the current game, but may be provable by applying the transformations
  in "advise"
- leave "advise" empty when secrecy is proved in the current game.
*)

(* [advise] stores advised transformations to try to make the proof
   of secrecy succeed. *)

let advise = ref []

(* [whole_game] contains the game on which we want to prove security properties *)

let whole_game = ref Terms.empty_game

(* [proved_one_session_secrets] contains a list of triples [(b,l,res)]
   which mean that [check_secrecy b l] returned [res].
   The goal is avoid redoing the proof of one-session secrecy when we
   want to prove both secrecy and one-session secrecy for the same
   variable with the same public variables. *)

let proved_one_session_secrets = ref []

(* [detected_leaks] and [current_restr] store information 
   useful for explaining why the proof of secrecy fails.

   [detected_leaks] stores all the detected reasons why the
   proof of secrecy fails. 

   [current_restr] stores the current restriction that defines the secret:
   typically, we want to prove the secrecy of a variable [b]. 
   [b] may itself be defined by a restriction, in which case [current_restr]
   will be set to [Some b]. 
   [b] may also be defined by assignment from [b'], where [b'] is defined 
   by a restriction. In this case, [!current_restr = Some b'].

   [public_vars] stores the list of public variables. *)

type leak_t =
    CyclicDep of binder (* [CyclicDep b] means that there is a cyclic dependency on variable [b]:
			   [b] depends on itself *)
  | Leak of binder * int list (* [Leak(b, occs)] means that variable [b] is 
				 leaked at the occurrences [occs] in the game *)
  | PublicVar of binder (* [PublicVar b] means that variable [b] is public *)

type group_leak_t =
    LeaksOf of binder * leak_t list
  | NotOnlyRestr of binder
  | NotRestrOrAssign

let detected_leaks = ref ([] : group_leak_t list)

let current_restr = ref None

let public_vars = ref []

(* [add_leak_for_current_restr l1] adds the leak [l1],
   which may be [CyclicDep b'] or [Leak(b',occs)],
   where [b'] depends on the current restriction [current_restr = Some b]. 
   This leak is added inside the group [LeaksOf(b, ...)]
   in [detected_leaks]. If this group is not already present,
   it is created. *)

let add_leak_for_current_restr l1 =
  let rec add_leak_rec = function
      [] -> [l1]
    | (l2::rest) as l ->
	match l1, l2 with
	  CyclicDep b, CyclicDep b' when b == b' -> 
	    l
	| Leak(b,n), Leak(b',n') when b == b' -> 
	    Leak(b, Terms.unionq n n') :: rest
	| PublicVar b, PublicVar b' when b == b' ->
	    l
	| _ -> 
	    l2 :: (add_leak_rec rest)
  in
  let current_restr = 
    match !current_restr with
      None -> Parsing_helper.internal_error "current_restr should be set"
    | Some b -> b
  in
  let rec add_leak_rec2 = function
      [] -> [LeaksOf(current_restr, [l1])]
    | l2::rest ->
	match l2 with
	  LeaksOf(b,l) when b == current_restr ->
	    (LeaksOf(b, add_leak_rec l))::rest
	| _ -> 
	    l2::add_leak_rec2 rest
  in
  detected_leaks := add_leak_rec2 (!detected_leaks)

(* [add_leak l1] adds the leak [l1] to [detected_leaks]
   if it is not already present. [l1] may be
   [NotOnlyRestr b] or [NotRestrOrAssign]. *)

let add_leak l1 =
  let rec add_leak_rec = function
      [] -> [l1]
    | (l2::rest) as l ->
	match l1,l2 with
	  NotRestrOrAssign, NotRestrOrAssign ->
	    l
	| NotOnlyRestr b, NotOnlyRestr b' ->
	    if b == b' then l else l2::(add_leak_rec rest)
	| _ ->
	    l2::(add_leak_rec rest)
  in
  detected_leaks := add_leak_rec (!detected_leaks)


(* [display_leaks b0] displays the explanation of the failure
   of the proof of one-session secrecy of [b0], as recorded
   in [detected_leaks]. *)

let rec display_list_sc f = function
    [] -> ()
  | [a] -> f a
  | (a::l) -> f a; print_string "; ";
      display_list_sc f l

let display_leaks_of b l =
  let display_leak = function
      CyclicDep b' ->
	print_string ("there is a cyclic dependency on " ^ (Display.binder_to_string b'));
	if b' != b then
	  print_string (", which may depend on " ^ (Display.binder_to_string b))
    | Leak(b',occs) ->
	print_string "at ";
	Display.display_list print_int occs;
	print_string (", bad usage(s) of " ^ (Display.binder_to_string b'));
	if b' != b then
	  print_string (", which may depend on " ^ (Display.binder_to_string b))
    | PublicVar b' ->
	print_string ((Display.binder_to_string b') ^ " is a public variable");
	if b' != b then
	  print_string (", which may depend on " ^ (Display.binder_to_string b))
  in
  display_list_sc display_leak l;
  print_string ".\n"

let display_leaks b0 =
  print_string ("Proof of (one-session) secrecy of " ^ 
		(Display.binder_to_string b0) ^ " failed:\n");
  List.iter (function
      LeaksOf(b,l) -> 
	print_string "  ";
	if b != b0 then 
	  print_string ((Display.binder_to_string b0) ^ " is defined from " ^ 
			(Display.binder_to_string b) ^ "; ");
	display_leaks_of b l
    | NotOnlyRestr b' ->
	print_string ("  " ^ (Display.binder_to_string b0) ^ " is defined from " ^ 
		      (Display.binder_to_string b') ^
		      ", which is not defined only by restrictions.\n")
    | NotRestrOrAssign ->
	print_string ("  " ^ (Display.binder_to_string b0) ^ 
		      " is not defined only by restrictions or assignments.\n")
      ) (!detected_leaks)


(* We can prove secrecy of part of an array; this is useful for forward secrecy  *)

(* [add_facts_at (all_indices, simp_facts0, defined_refs0, pp_list) 
   cur_array new_facts pp fact_info] updates the quadruple 
   [(all_indices, simp_facts0, defined_refs0, pp_list)] where
   - [cur_array] contains the current replication indices
   - [pp] is the current program point
   - [fact_info] contains facts that hold at the current program point
   - [new_facts] contains other facts that should also be added.
   It renames the current_replication indices of [cur_array] to 
   fresh indices [lidx'].
   Inside the quadruple, 
   - [all_indices] contains all indices seen so far. (It is extended with the
   fresh indices [lidx'].)
   - [simp_facts0] contains facts that are known to hold. (It is extended with
   facts from [fact_info] and from [new_facts], after renaming
   of replication indices, as well as from facts inferred by [Terms.both_pp_add_fact]
   from the list of visited program points.)
   - [defined_refs] contains variables that are known to be defined. (It is extended
   with the variables known to be defined from [fact_info], after renaming
   of replication indices.)
   - [pp_list] contains the program points that are known to have been
   visited with their corresponding indices. (It is extended with [(lidx', pp)].)
   [add_facts_at] returns [lidx'] as well as the updated quadruple.
*)

let add_facts_at (all_indices, simp_facts0, defined_refs0, pp_list) cur_array new_facts pp =
  let ri_lidx' = List.map Terms.new_repl_index cur_array in
  let lidx' = List.map Terms.term_from_repl_index ri_lidx' in
  let new_def_list = Terms.subst_def_list cur_array lidx' (Facts.get_def_vars_at pp) in
  let defined_refs1 = new_def_list @ defined_refs0 in
  let facts1 = List.map (Terms.subst cur_array lidx') (new_facts @ (Facts.get_facts_at pp)) in
  let new_pp = (lidx', pp) in
  (* Add facts inferred from the compatibility between [new_pp] and [pp_list] *)
  let facts2 = List.fold_left (fun accu -> Terms.both_pp_add_fact accu new_pp) facts1 pp_list in
  (* Add facts inferred from the compatibility between [defined_refs0] and [new_def_list] *)
  let facts3 = Terms.both_def_list_facts facts2 defined_refs0 new_def_list in
  (* Add facts inferred from the compatibility between [new_pp] and [defined_refs1] *)
  let facts4 = Terms.def_list_pp facts3 (pp, lidx') defined_refs1 in
  (* Add facts inferred from the compatibility between [pp_list] and [new_def_list] *)
  let facts5 = List.fold_left (fun accu (lidx, pp0) -> 
    Terms.def_list_pp accu (pp0, lidx) new_def_list) facts4 pp_list 
  in
  let simp_facts1 = Terms.auto_cleanup (fun () -> 
    Facts.simplif_add_list Facts.no_dependency_anal simp_facts0 facts5) 
  in
  (lidx', (ri_lidx' @ all_indices, simp_facts1, defined_refs1, new_pp::pp_list))

(* [collect_bargs args_accu b t] collects in [args_accu] the arguments
   of the variables [b] inside the term [t]. 
   It raises [TooComplex] when it does not support the term [t]
   (let/find/new). *)

exception TooComplex

let rec collect_bargs args_accu b t =
  match t.t_desc with
    Var(b',l) -> 
      if b' == b then
	begin
	  if not (List.exists (Terms.equal_term_lists l) (!args_accu)) then
	    args_accu := l :: (!args_accu)
	end
      else
	List.iter (collect_bargs args_accu b) l
  | FunApp(_,l) ->
      List.iter (collect_bargs args_accu b) l
  | ReplIndex _ -> ()
  | TestE(t1,t2,t3) ->
      collect_bargs args_accu b t1;
      collect_bargs args_accu b t2;
      collect_bargs args_accu b t3
  | _ -> 
      raise TooComplex

(* [check_usage seen_accu b lidx facts X] checks that [X] cannot leak
   [ b[lidx] ] when [facts] holds. [seen_accu] contains the values of
   [b] already seen, to avoid loops. *)

let rec check_usage_term cur_array seen_accu b lidx facts t =
  match t.t_desc with
    Var(b',l) ->
      if b' == b then 
	begin
	  (* Dependency on b[l] 
	     let 'rename' replace cur_array with fresh indices
	     facts union (rename Facts.get_facts_at t.t_facts) union (lidx = rename l) implies a contradiction *)
	  try
	    let eq_index = List.map2 Terms.make_equal lidx l in 
	    let (lidx', (all_indices, simp_facts, defined_refs, _)) = add_facts_at facts cur_array eq_index (DTerm t) in
	    let facts2 = 
	      if !Settings.elsefind_facts_in_success then
		Simplify1.get_facts_of_elsefind_facts (!whole_game) all_indices simp_facts defined_refs 
	      else
		[]
	    in
	    ignore (Terms.auto_cleanup (fun () -> Facts.simplif_add_list Facts.no_dependency_anal simp_facts facts2));
	    (* For debugging*)
	    add_leak_for_current_restr (Leak(b, [t.t_occ]));
	    (* print_string "Known facts:\n";
	    Facts.display_facts simp_facts; 
	    print_string "Defined variables:\n";
	    List.iter (fun (b,l) -> Display.display_var b l; print_newline()) defined_refs;	    
	    print_string "Added using elsefind:\n";
	    List.iter (fun t -> Display.display_term t; print_newline()) facts2; *)
	    raise Not_found
	  with Contradiction -> ()
	end;
      List.iter (check_usage_term cur_array seen_accu b lidx facts) l
  | ReplIndex _ -> ()	
  | FunApp(f,l) ->
      List.iter (check_usage_term cur_array seen_accu b lidx facts) l
  | TestE(t1,t2,t3) ->
      check_usage_term cur_array seen_accu b lidx facts t1;
      check_usage_term cur_array seen_accu b lidx facts t2;
      check_usage_term cur_array seen_accu b lidx facts t3
  | FindE(l0,t3,_) ->
      List.iter (fun (bl,def_list,t1,t2) ->
	List.iter (fun (_,l) -> List.iter (check_usage_term cur_array seen_accu b lidx facts) l) def_list;
	check_usage_term cur_array seen_accu b lidx facts t1;
	check_usage_term cur_array seen_accu b lidx facts t2) l0;
      check_usage_term cur_array seen_accu b lidx facts t3
  | LetE(PatVar b', t1, t2, _) ->
      begin
	try 
	  let args_accu = ref [] in
	  collect_bargs args_accu b t1;
	  if (!args_accu) != [] then
	    begin
	      if List.memq b' seen_accu then
		begin
		  add_leak_for_current_restr (CyclicDep b');
		  raise Not_found
		end;
	      List.iter (fun l ->
	        (* b[l] occurs in t1, so the cells b'[lidx'] with lidx = l may
		   depend on b[lidx]. We check that they do not leak information *)
		begin
		  try
	            (* let 'rename' replace b'.args_at_creation with fresh indices
	               facts' = facts union (rename get_facts_at t2.t_facts) union (lidx = rename l) 
	               lidx' = rename b'.args_at_creation *)
		    let eq_index = List.map2 Terms.make_equal lidx l in 
		    let (lidx', facts') = add_facts_at facts cur_array eq_index (DTerm t2) in
		    check_usage_full_process (b'::seen_accu) b' lidx' facts'
		  with Contradiction -> 
	              (* current program point unreachable *)
		      ()
		end;
		List.iter (check_usage_term cur_array seen_accu b lidx facts) l
		  ) (!args_accu)
	    end
	with TooComplex ->
	  (* Either [t1] may depend on [b] in a too complex way.
	     Check directly that it does not depend on [b]. *)
	  check_usage_term cur_array seen_accu b lidx facts t1
      end;
      check_usage_term cur_array seen_accu b lidx facts t2
  | LetE(pat, t1, t2, topt) ->
      begin
	check_usage_pat cur_array seen_accu b lidx facts pat;
	check_usage_term cur_array seen_accu b lidx facts t1;
	check_usage_term cur_array seen_accu b lidx facts t2;
	match topt with
	  None -> ()
	| Some t3 -> check_usage_term cur_array seen_accu b lidx facts t3
      end
  | ResE(b,t) ->
      check_usage_term cur_array seen_accu b lidx facts t
  | EventAbortE _ | EventE _ | GetE _ | InsertE _ ->
      Parsing_helper.internal_error "Event, event_abort, get, insert should have been expanded"
      
and check_usage_pat cur_array seen_accu b lidx facts = function
    PatVar _ -> ()
  | PatTuple (f,l) -> List.iter (check_usage_pat cur_array seen_accu b lidx facts) l
  | PatEqual t -> check_usage_term cur_array seen_accu b lidx facts t

and check_usage_process cur_array seen_accu b lidx facts p =
  match p.i_desc with
    Nil -> ()
  | Par(p1,p2) -> 
      check_usage_process cur_array seen_accu b lidx facts p1;
      check_usage_process cur_array seen_accu b lidx facts p2
  | Repl(ri,p) ->
      check_usage_process (ri::cur_array) seen_accu b lidx facts p
  | Input((c, tl), pat, p) ->
      List.iter (check_usage_term cur_array seen_accu b lidx facts) tl;
      check_usage_pat cur_array seen_accu b lidx facts pat;
      check_usage_oprocess cur_array seen_accu b lidx facts p

and check_usage_oprocess cur_array seen_accu b lidx facts p =
  match p.p_desc with
    Yield | EventAbort _ -> ()
  | Restr(_,p) ->
      check_usage_oprocess cur_array seen_accu b lidx facts p
  | Test(t,p1,p2) ->
      check_usage_term cur_array seen_accu b lidx facts t;
      check_usage_oprocess cur_array seen_accu b lidx facts p1;
      check_usage_oprocess cur_array seen_accu b lidx facts p2
  | Find(l0,p2,_) ->
      List.iter (fun (bl,def_list, t, p1) ->
	List.iter (fun (_,l) -> 
	  List.iter (check_usage_term cur_array seen_accu b lidx facts) l) def_list;
	check_usage_term cur_array seen_accu b lidx facts t;
	check_usage_oprocess cur_array seen_accu b lidx facts p1) l0;
      check_usage_oprocess cur_array seen_accu b lidx facts p2
  | Let(PatVar b', t, p1, _) ->
      begin
	try 
	  let args_accu = ref [] in
	  collect_bargs args_accu b t;
	  if (!args_accu) != [] then
	    begin
	      if List.memq b' seen_accu then
		begin
		  add_leak_for_current_restr (CyclicDep b');
		  raise Not_found
		end;
	      List.iter (fun l ->
	        (* b[l] occurs in t1, so the cells b'[lidx'] with lidx = l may
	           depend on b[lidx]. We check that they do not leak information *)
		begin
		  try
	            (* let 'rename' replace b'.args_at_creation with fresh indices
		       facts' = facts union (rename (get_facts_at p1.p_facts)) union (lidx = rename l)
		       lidx' = rename b'.args_at_creation *)
		    let eq_index = List.map2 Terms.make_equal lidx l in 
		    let (lidx', facts') = add_facts_at facts cur_array eq_index (DProcess p1) in
		    check_usage_full_process (b'::seen_accu) b' lidx' facts' 
		  with Contradiction -> 
	              (* Current program point unreachable *)
		      ()
		end;
		List.iter (check_usage_term cur_array seen_accu b lidx facts) l
		  ) (!args_accu)
	    end
	with TooComplex ->
	  (* Either [t] does not depend on [b], or it may depend on [b]
	     in a too complex way. Check directly that [t] does not depend on [b]. *)
	  check_usage_term cur_array seen_accu b lidx facts t
      end;
      check_usage_oprocess cur_array seen_accu b lidx facts p1
  | Let(pat,t,p1,p2) ->
      check_usage_pat cur_array seen_accu b lidx facts pat;
      check_usage_term cur_array seen_accu b lidx facts t;
      check_usage_oprocess cur_array seen_accu b lidx facts p1;
      check_usage_oprocess cur_array seen_accu b lidx facts p2
  | Output((c, tl),t2,p) ->
      List.iter (check_usage_term cur_array seen_accu b lidx facts) tl;
      check_usage_term cur_array seen_accu b lidx facts t2;
      check_usage_process cur_array seen_accu b lidx facts p
  | EventP(t,p) ->
      (* Events are ignored when checking secrecy
	 This assumes that LetE/FindE have been expanded, so that
	 they do not occur in t *)
      check_usage_oprocess cur_array seen_accu b lidx facts p
  | Get _|Insert _ -> Parsing_helper.internal_error "Get/Insert should not appear here"

and check_usage_full_process seen_accu b lidx facts =
  if List.memq b (!public_vars) then
    begin
      add_leak_for_current_restr (PublicVar b);
      raise Not_found
    end
  else
    check_usage_process [] seen_accu b lidx facts (!whole_game).proc


let has_assign b =
  List.exists (fun def ->
    match def.definition with
      DProcess { p_desc = Let _ } | DTerm { t_desc = LetE _} -> true
    | _ -> false) b.def

(* [check_secrecy b pub_vars] proves one-session secrecy of [b]
   with public variables [pub_vars].
   It returns [(true, proba)] when one-session secrecy of [b]
   holds up to probability [proba].
   It returns [(false, _)] when the proof of one-session secrecy
   of [b] failed. *)

let check_secrecy b pub_vars =
  let ty = ref None in
  Simplify1.reset [] (!whole_game);
  advise := [];
  detected_leaks := [];
  try
    List.iter (fun d -> 
      match d.definition with
	DProcess { p_desc = Let(PatVar _,{ t_desc = Var(b',l) },_,_) }
      |	DTerm { t_desc = LetE(PatVar _, { t_desc = Var(b',l) },_,_) } ->
	  if has_assign b' then
	    begin
	      add_leak (NotOnlyRestr b');
	      advise := Terms.add_eq (RemoveAssign (OneBinder b')) (!advise)
	    end
	  else if Terms.is_restr b' then
	    begin
	      current_restr := Some b';
	      public_vars := pub_vars;
	      (match !ty with
		None -> ty := Some b'.btype
	      |	Some ty' -> if ty' != b'.btype then 
		  Parsing_helper.internal_error ("Variable " ^ (Display.binder_to_string b) ^ " has definitions of different types"));
	      try
		let (lidx, facts) = add_facts_at ([],([],[],[]),[],[]) b.args_at_creation [] d.definition_success in
		let rename = Terms.subst b.args_at_creation lidx in
		check_usage_full_process [b'] b' (List.map rename l) facts
	      with
		Not_found ->
		  if List.length b'.def > 1 then
		    advise := Terms.add_eq (SArenaming b') (!advise)
		  else
		    raise Not_found
	      |	Contradiction ->
		  (* Current program point unreachable *)
		  ()
	    end
	  else
	    begin
	      add_leak (NotOnlyRestr b');
	      raise Not_found
	    end
      |	DProcess { p_desc = Restr(_, _) } ->
	  (match !ty with
	    None -> ty := Some b.btype
	  | Some ty' -> if ty' != b.btype then 
	      Parsing_helper.internal_error ("Variable " ^ (Display.binder_to_string b) ^ " has definitions of different types"));
	  begin
	    try
	      current_restr := Some b;
	      public_vars := pub_vars;
	      let (lidx, facts) = add_facts_at ([],([],[],[]),[],[]) b.args_at_creation [] d.definition_success in
	      check_usage_full_process [b] b lidx facts
	    with Contradiction ->
	      (* Current program point unreachable *)
	      ()
	  end
      |	_ ->
	  add_leak NotRestrOrAssign;
	  raise Not_found) b.def;
    if (!advise) == [] then
      begin
	print_string "Proved one-session secrecy of ";
	Display.display_binder b;
	print_newline();
	detected_leaks := [];
	current_restr := None;
	public_vars := [];
	(true, Simplify1.final_add_proba())
      end
    else
      begin
	display_leaks b;
	List.iter (fun i -> 
	  Settings.advise := Terms.add_eq i (!Settings.advise)) (!advise);
	advise := [];
	detected_leaks := [];
	current_restr := None;
	public_vars := [];
	(false, [])
      end
  with Not_found -> 
    display_leaks b;
    advise := [];
    detected_leaks := [];
    current_restr := None;
    public_vars := [];
    (false, [])

(* [check_secrecy_memo b l] does the same as [check_secrecy b l] 
   but uses [proved_one_session_secrets] to avoid redoing work 
   when it is called several times with the same variable [b]
   and list [l]. *)

let check_secrecy_memo b l =
  try
    let (_,_,res) = List.find (fun (b',l',res) -> (b == b') && (Terms.equal_lists (==) l l')) 
	(!proved_one_session_secrets) 
    in 
    res
  with Not_found ->
    let res = check_secrecy b l in
    proved_one_session_secrets := (b, l, res) :: (!proved_one_session_secrets);
    res

(* [check_query q] proves the query [q]. 
   It returns [(true, proba)] when [q] holds up to probability [proba].
   It returns [(false, _)] when the proof of [q] failed.*)

let check_query event_accu = function
    (QSecret1 (b,l),_) -> check_secrecy_memo b l
  | (QSecret (b,l),_) -> 
      let (r1, proba1) = check_secrecy_memo b l in
      if r1 then
	let (r2, proba2) = Check_distinct.check_distinct b (!whole_game) in
	if r2 then
	  begin
	    let proba = proba1 @ proba2 in
	    print_string "Proved secrecy of ";
	    Display.display_binder b;
	    if proba != [] then
	      begin
		print_string " Probability: ";
		Display.display_set proba
	      end;
	    print_newline();
	    (true, proba)
	  end
	else (false, [])
      else (false, [])
  | (QEventQ(t1,t2),gn) ->
      let (r, proba) = Check_corresp.check_corresp event_accu (t1,t2) (!whole_game) in
      if r then
	begin
	  print_string "Proved query ";
	  Display.display_query3 (QEventQ(t1,t2));
	  if proba != [] then
	    begin
	      print_string " Probability: ";
	      Display.display_set proba
	    end;
	  print_newline();
	  (true, proba)
	end
      else (false, [])
  | (AbsentQuery,_) -> (false, [])	

(* [check_query_list state qlist] takes a list of queries [qlist], tries to prove
   those that are not proved yet, and returns
    - the list of queries it proved with the associated probability of success of an attack.
    - the updated list of all queries with their proofs
    - a boolean which is true when all queries have been proved *)

let rec check_query_list event_accu state = function
    [] -> ([],[],true)
  | ((a, poptref, popt)::l) ->
      let (l',l'',b) = check_query_list event_accu state l in
      match popt with
	Some _ -> (* The query was already proved before *)
	  (l', (a, poptref, popt)::l'', b)
      |	None -> (* We need to prove the query *)
	  let (res, proba) = check_query event_accu a in
	  if res then 
	    (* The query is proved *)
	    ((a,proba)::l', (a,poptref,Some(proba, state))::l'', b) 
	  else 
	    (* The query is not proved *)
	    (l', (a, poptref,popt)::l'', false)

(* [is_event_query f g q] returns [true] when the query [q] is 
   [event f ==> false] (that is, "the event [f] can never be executed") 
   in game [g] *)

let is_event_query f g = function
    ((QEventQ([false, { t_desc = FunApp(f',[{ t_desc = FunApp(f_tuple, []) }]) }], QTerm t_false),g'), _,_) -> 
      g == g' && f == f' && Terms.is_false t_false
  | _ -> false 

(* [update_full_proof query_list (q, poptref, popt)] updates [poptref]
   with the proof of query [q] when [q] is fully proved.
   Indeed, when we introduce events during the proof of a query [q],
   it is not enough to prove [q] on the final game, we must also 
   bound the probability that the events introduced during the proof happen. 
   [popt = Some(proba, state)] records the proof that the query [q] 
   is proved in the final game of [state], so that it holds up to 
   probability [proba] in the initial game. 
   However, [proba] may refer to the probabilities of events introduced
   during the proof. 
   [update_full_proof] sets [poptref] to [proba] when the probability
   of these events has also been bounded. *)

let rec update_full_proof query_list ((_,g), poptref, popt) =
  match popt, !poptref with
    Some(proba, state), None ->
      if is_full_proof query_list g proba state then
	poptref := Some(proba, state)
  | _ -> ()

and is_full_proof query_list g proba state =
  (List.for_all (is_full_proba query_list) proba) &&
  (is_full_state query_list g state)

and is_full_state query_list g state =
  if state.game == g then
    true
  else
    match state.prev_state with
      None -> Parsing_helper.internal_error "Game not found"
    | Some(_, proba, _, s') ->
        (List.for_all (is_full_proba query_list) proba) &&
	(is_full_state query_list g s')

and is_full_proba query_list = function
    SetProba _ -> true
  | SetEvent(f,g,poptref) ->
      match !poptref with
	Some _ -> true
      |	None ->
	  try
	    let query = List.find (is_event_query f g) query_list in
	    update_full_proof query_list query;
	    match !poptref with
	      Some _ -> true
	    | None -> false
	  with Not_found -> false
    
(* [is_success state] tries to prove queries that still need to be
   proved in [state]. It updates the proofs of the queries inside
   [state] and returns the list of newly proved queries (with the
   associated probability of success of an attack) as well as boolean
   which is true when all queries are proved. *)

let is_success state =
  let g = state.game in
  whole_game := g;
  proved_one_session_secrets := [];
  let vcounter = !Terms.vcounter in
  let event_accu = ref [] in
  Simplify1.improved_def_process (Some event_accu) true g.proc;
  let (proved_queries, all_queries, all_proved) = check_query_list (!event_accu) state g.current_queries in
  g.current_queries <- all_queries; (* Updated queries *)
  List.iter (update_full_proof all_queries) all_queries;
  Terms.vcounter := vcounter; (* Forget created variables *)
  proved_one_session_secrets := [];
  Simplify1.empty_improved_def_process true g.proc;
  whole_game := Terms.empty_game;
  proved_queries, all_proved


      

	  
