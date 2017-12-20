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

(*****
   [check_distinct b g] shows that elements of the array [b] 
   at different indices are always different (up to negligible probability).
   [g] is the full game.
   This is useful for showing secrecy of a key.
 *****)


let make_indexes cur_array =
  List.map Terms.new_repl_index cur_array

let collect_facts def bindex index =
  let facts = Facts.get_facts_at def.definition_success in
  let def_vars = Facts.get_def_vars_at def.definition_success in
  let elsefind_facts = Facts.get_elsefind_facts_at def.definition_success in
  (* Rename session identifiers in facts, variables, and elsefind facts *)
  List.iter2 (fun b t -> b.ri_link <- (TLink t)) bindex index;
  let new_facts = List.map (Terms.copy_term Terms.Links_RI) facts in
  let new_def_vars = Terms.copy_def_list Terms.Links_RI def_vars in
  let new_elsefind_facts = List.map Terms.copy_elsefind elsefind_facts in
  List.iter (fun b -> b.ri_link <- NoLink) bindex;
  (new_facts, new_def_vars, new_elsefind_facts)
  
let collect_facts_list bindex index1 defs =
  List.fold_left (fun accu d ->
    try
      (d, collect_facts d bindex index1)::accu
    with Contradiction ->
      accu) [] defs
    
let check_distinct b g =
  Proba.reset [] g;
  Simplify1.improved_def_process None false g.proc;
  let r_index1 = make_indexes b.args_at_creation in
  let r_index2 = make_indexes b.args_at_creation in
  let index1 = List.map Terms.term_from_repl_index r_index1 in
  let index2 = List.map Terms.term_from_repl_index r_index2 in
  let diff_index = Terms.make_or_list (List.map2 Terms.make_diff index1 index2) in
  let bindex = b.args_at_creation in
  let d1withfacts = collect_facts_list bindex index1 b.def in
  let d2withfacts = collect_facts_list bindex index2 b.def in
  let r = 
  List.for_all (fun (d1,(d1facts,d1def_vars,d1elsefind_facts)) ->
    List.for_all (fun (d2,(d2facts,d2def_vars,d2elsefind_facts)) ->
      match d1.definition, d2.definition with
	DProcess { p_desc = Restr _ }, DProcess { p_desc = Restr _} -> true
      | DProcess { p_desc = Restr _ }, 
	    (DProcess { p_desc = Let(PatVar _,{ t_desc = Var(b',l) },_,_)}
	    |DTerm { t_desc = LetE(PatVar _, { t_desc = Var(b',l) },_,_) }) ->
		if not (Terms.is_restr b') then
		  Parsing_helper.internal_error "restriction should be checked when testing secrecy";
		(b != b') || 
		(
		try
		  let eq_b = Terms.make_and_list 
		      (List.map2 Terms.make_equal index1 (List.map (Terms.subst bindex index2) l))
		  in
		  let facts1 = diff_index :: eq_b :: (List.rev_append d1facts d2facts) in
		  let simp_facts1 = Facts.simplif_add_list Facts.no_dependency_anal ([],[],[]) facts1 in
		  let def_vars = List.rev_append d1def_vars d2def_vars in
		  let facts2 = 
		    if !Settings.elsefind_facts_in_success then
		      Simplify1.get_facts_of_elsefind_facts g (r_index1 @ r_index2) simp_facts1 
			def_vars
		    else
		      []
		  in
		  ignore (Facts.simplif_add_list Facts.no_dependency_anal simp_facts1 facts2);
		  (* The following part is commented out because it is too costly. 

		  let simp_facts2 = [code above] in
		     When the restriction and the let are the same value,
		     the let must have been executed after the restriction.
		     Hence the elsefind facts at the let hold. 
		  let (subst, facts, _) = simp_facts2 in
		  let simp_facts3 = (subst, facts, d2elsefind_facts) in
		  ignore (Simplify1.convert_elsefind Facts.no_dependency_anal def_vars simp_facts3);*)
		  false
		with Contradiction -> true
		    )
      |	(DProcess { p_desc = Let(PatVar _,{ t_desc = Var(b',l) },_,_)}
        |DTerm { t_desc = LetE(PatVar _, { t_desc = Var(b',l) },_,_) }), 
		DProcess { p_desc = Restr _ } ->
	  true (* The symmetric case will be checked by the previous pattern *)
      |	(DProcess { p_desc = Let(PatVar _,{ t_desc = Var(b1',l1) },_,_)}
        |DTerm { t_desc = LetE(PatVar _, { t_desc = Var(b1',l1) },_,_) }),
	  (DProcess {p_desc = Let(PatVar _,{ t_desc = Var(b2',l2) },_,_)}
          |DTerm { t_desc = LetE(PatVar _, { t_desc = Var(b2',l2) },_,_) }) ->
		if not ((Terms.is_restr b1') && (Terms.is_restr b2')) then
		  Parsing_helper.internal_error "restriction should be checked when testing secrecy";
		(b1' != b2') || 
		(
		try
		  let eq_b = Terms.make_and_list 
		      (List.map2 Terms.make_equal 
			 (List.map (Terms.subst bindex index1) l1) 
			 (List.map (Terms.subst bindex index2) l2))
		  in
		  let facts1 = diff_index :: eq_b :: (List.rev_append d1facts d2facts) in
		  let simp_facts1 = Facts.simplif_add_list Facts.no_dependency_anal ([],[],[]) facts1 in
		  let def_vars = List.rev_append d1def_vars d2def_vars in
		  let facts2 = 
		    if !Settings.elsefind_facts_in_success then
		      Simplify1.get_facts_of_elsefind_facts g (r_index1 @ r_index2) simp_facts1 
			def_vars
		    else
		      []
		  in
		  ignore (Facts.simplif_add_list Facts.no_dependency_anal simp_facts1 facts2);
		  (* The following part is commented out because it is too costly. 

		  let simp_facts2 = [code above] in
		     We assume that the 2nd Let is executed after the 1st one.
		     The other case will be checked symmetrically since we
		     scan the whole lists d1withfacts and d2withfacts
		     Hence the elsefind facts at the 2nd let hold. 
		  let (subst, facts, _) = simp_facts2 in
		  let simp_facts3 = (subst, facts, d2elsefind_facts) in
		  ignore (Simplify1.convert_elsefind Facts.no_dependency_anal def_vars simp_facts3);*)
		  false
		with Contradiction -> true
		    )
      | _ -> 
	  Parsing_helper.internal_error "definition cases should be checked when testing secrecy"
	  ) d2withfacts
      ) d1withfacts
  in
  (* Must not empty, because may be used by other queries;
     Will be emptied in success.ml
     Simplify1.empty_improved_def_process false g.proc; *)
  if r then
    (* Add probability for eliminated collisions *)
    (true, Proba.final_add_proba[])
  else
    begin
      print_string ("Proof of secrecy of " ^ 
		    (Display.binder_to_string b) ^ " failed:\n");
      print_string "  Proved one-session secrecy but not secrecy.\n";
      (false, [])
    end
        (*
        print_string "Facts for check_distinct 1:\n";
        List.iter (fun t -> Display.display_term t; print_newline()) facts1;

        print_string "Facts for check_distinct 2:\n";
        display_facts facts;
        *)


