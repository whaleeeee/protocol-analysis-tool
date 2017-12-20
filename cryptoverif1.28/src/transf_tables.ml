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

(* First pass: transform the insert in let calls and return all the created binders *)

let rec has_insert t =
  (match t.t_desc with
  | InsertE _ -> true
  | _ -> false) ||
  (Terms.exists_subterm has_insert (fun _ -> false) has_insert_pat t)

and has_insert_pat pat =
  Terms.exists_subpat has_insert has_insert_pat pat

let check_no_insert t =
  if has_insert t then
    Parsing_helper.internal_error "Insert must not occur in conditions of get or find" 

let rec has_get t =
  (match t.t_desc with
  | GetE _ -> true
  | _ -> false) ||
  (Terms.exists_subterm has_get (fun _ -> false) has_get_pat t)

and has_get_pat pat =
  Terms.exists_subpat has_get has_get_pat pat

let check_no_get t =
  if has_get t then
    Parsing_helper.internal_error "Insert must not occur in conditions of get or find" 

let rec trf_insert_term accu cur_array t =
  match t.t_desc with
    Var(b, tl) ->
      Terms.build_term2 t (Var(b, List.map (trf_insert_term accu cur_array) tl))
  | FunApp(f, tl) ->
      Terms.build_term2 t (FunApp(f, List.map (trf_insert_term accu cur_array) tl))
  | ReplIndex _ -> t
  | TestE(t1, t2, t3) ->
      Terms.build_term2 t (TestE(trf_insert_term accu cur_array t1,
				 trf_insert_term accu cur_array t2,
				 trf_insert_term accu cur_array t3))
  | FindE(l,t3,find_info) ->
      Terms.build_term2 t (FindE(List.map (fun (bl,def_list,t1,t2) ->
        check_no_insert t1;
	(bl, def_list, t1, trf_insert_term accu cur_array t2)) l,
				 trf_insert_term accu cur_array t3,
				 find_info))
  | LetE(pat, t1, t2, topt) ->
      Terms.build_term2 t (LetE(trf_insert_pat accu cur_array pat,
				trf_insert_term accu cur_array t1,
				trf_insert_term accu cur_array t2,
				match topt with
				  None -> None
				| Some t3 -> Some (trf_insert_term accu cur_array t3)))
  | ResE(b,t1) ->
      Terms.build_term2 t (ResE(b, trf_insert_term accu cur_array t1))
  | EventAbortE _ -> t
  | EventE(t1,p) ->
      Terms.build_term2 t (EventE(trf_insert_term accu cur_array t1,
				  trf_insert_term accu cur_array p))
  | InsertE(tbl, tl, p) ->
      Settings.changed := true;
      let tl' = List.map (trf_insert_term accu cur_array) tl in 
      let p' = trf_insert_term accu cur_array p in
      let bl = List.map (fun ty -> Terms.create_binder tbl.tblname (Terms.new_vname()) ty cur_array) tbl.tbltype in
      let p'' = List.fold_right2 (fun b t p ->
        Terms.build_term_type p.t_type (LetE(PatVar(b),t,p,None))
          ) bl tl' p' in
      accu := (tbl,Some bl) :: (!accu);
      p''
  | GetE(tbl,patl,topt,p1,p2) ->
      begin
	match topt with
	  None -> ()
	| Some t -> check_no_insert t
      end;
      accu := (tbl,None)::(!accu);
      Terms.build_term2 t 
	(GetE(tbl,List.map (trf_insert_pat accu cur_array) patl,
	      topt,trf_insert_term accu cur_array p1,
	      trf_insert_term accu cur_array p2))

and trf_insert_pat accu cur_array = function
    PatVar b -> PatVar b
  | PatTuple(f,l) -> PatTuple(f,List.map (trf_insert_pat accu cur_array) l)
  | PatEqual t -> PatEqual (trf_insert_term accu cur_array t)
      
  
let rec trf_insert_iprocess accu cur_array p =
  match p.i_desc with
    | Nil -> Terms.iproc_from_desc Nil
    | Par(p1,p2) -> 
        Terms.iproc_from_desc (Par(trf_insert_iprocess accu cur_array p1,
				   trf_insert_iprocess accu cur_array p2))
    | Repl(b,p) ->
        Terms.iproc_from_desc
	  (Repl(b,trf_insert_iprocess accu (b::cur_array) p))
    | Input((c,tl),pat,p) ->
	List.iter check_no_insert tl;
	if has_insert_pat pat then
	  let b = Terms.create_binder "patv" (Terms.new_vname()) 
	      (Terms.get_type_for_pattern pat) cur_array
	  in
	  let bterm = Terms.term_from_binder b in
	  let p' = Terms.oproc_from_desc (Let(pat, bterm, p,
					      Terms.oproc_from_desc Yield))
	  in
	  Terms.iproc_from_desc
	    (Input((c,tl),PatVar b, trf_insert_oprocess accu cur_array p'))
	else
          Terms.iproc_from_desc
	    (Input((c,tl),pat,trf_insert_oprocess accu cur_array p))

and trf_insert_oprocess accu cur_array p =
  match p.p_desc with
    | Yield -> Terms.oproc_from_desc Yield
    | EventAbort f -> Terms.oproc_from_desc (EventAbort f)
    | Restr(b,p) ->
        Terms.oproc_from_desc (Restr(b,trf_insert_oprocess accu cur_array p))
    | Test(t,p1,p2) ->
        Terms.oproc_from_desc (Test(trf_insert_term accu cur_array t,
				    trf_insert_oprocess accu cur_array p1,
				    trf_insert_oprocess accu cur_array p2))
    | Find(bl,p,fi) ->
        let bl' = List.map 
	    (fun (bl,d,t,p)->
	      check_no_insert t;
              (bl,d,t,trf_insert_oprocess accu cur_array p)
		) bl
	in
        Terms.oproc_from_desc (Find(bl',trf_insert_oprocess accu cur_array p,fi))
    | Output((c,tl),t,p) ->
	List.iter check_no_insert tl;
        Terms.oproc_from_desc (Output((c,tl),trf_insert_term accu cur_array t,
				      trf_insert_iprocess accu cur_array p))
    | Let(pat,t,p1,p2) ->
        Terms.oproc_from_desc (Let(trf_insert_pat accu cur_array pat,
				   trf_insert_term accu cur_array t,
				   trf_insert_oprocess accu cur_array p1,
				   trf_insert_oprocess accu cur_array p2))
    | EventP(t,p) ->
        Terms.oproc_from_desc (EventP(trf_insert_term accu cur_array t,
				      trf_insert_oprocess accu cur_array p))
    | Insert(tbl,tl,p) ->
        Settings.changed := true;
	let tl' = List.map (trf_insert_term accu cur_array) tl in 
        let p' = trf_insert_oprocess accu cur_array p in
        let bl = List.map (fun ty -> Terms.create_binder tbl.tblname (Terms.new_vname()) ty cur_array) tbl.tbltype in
        let p'' = List.fold_right2 (fun b t p ->
          Terms.oproc_from_desc (Let(PatVar(b),t,p,Terms.oproc_from_desc Yield))
            ) bl tl' p' in
	accu := (tbl,Some bl) :: (!accu);
        p''
    | Get(tbl,patl,topt,p1,p2) ->
	begin
	  match topt with
	    None -> ()
	  | Some t -> check_no_insert t
	end;
	accu := (tbl,None)::(!accu);
        Terms.oproc_from_desc
	  (Get(tbl,List.map (trf_insert_pat accu cur_array) patl,
	       topt,trf_insert_oprocess accu cur_array p1,
	       trf_insert_oprocess accu cur_array p2))

let transform_insert p =
  let accu = ref [] in
  let p' = trf_insert_iprocess accu [] p in
  (p', !accu)

(* Second pass: transform the Get calls into Find calls *)

let rec get_info_for tbl = function
    [] -> []
  | (tbl', Some i)::r when tbl == tbl' -> i::(get_info_for tbl r)
  | _::r -> get_info_for tbl r

let rec get_find_branch_term brl patl t =
  match brl,patl with 
    | [],[] -> t
    | [],_ | _,[] -> Parsing_helper.internal_error "get_find_branch_term: lists not of the same size"
    | br::brl',pat::patl' ->
        let t1=Terms.term_from_binderref br in
          (match pat with
            | PatVar (b) -> 
               let subst = Terms.OneSubst(b,t1,ref false) in
               let patl'' = List.map (Terms.copy_pat subst) patl' in
               let t' = Terms.copy_term subst t in
                 get_find_branch_term brl' patl'' t'
            | PatEqual (t2) -> 
                let t' = get_find_branch_term brl' patl' t in
                  Terms.make_and (Terms.make_equal t1 t2) t'
            | PatTuple _ ->
                let t' = get_find_branch_term brl' patl' t in
                  Terms.build_term_type Settings.t_bool 
                    (LetE(pat, t1, t', Some (Terms.make_false ()))))

let rec get_find_branch_then_process brl patl p =
  match brl,patl with 
    | [],[] -> p
    | [],_ | _,[] -> Parsing_helper.internal_error "get_find_branch_then_process: lists not of the same size"
    | br::brl',pat::patl' ->
        let t1=Terms.term_from_binderref br in
        (match pat with
           | PatVar (b) ->
	       let subst = Terms.OneSubst(b,t1,ref false) in
	       let patl'' = List.map (Terms.copy_pat subst) patl' in
	       let p' = Terms.copy_oprocess subst p in
               get_find_branch_then_process brl' patl'' p'
           | PatEqual (t2) ->
               (* at this point, this is always true *)
               get_find_branch_then_process brl' patl' p
           | _ ->
               let p' = get_find_branch_then_process brl' patl' p in
               Terms.oproc_from_desc (Let(pat, t1, p', Terms.oproc_from_desc Yield)))

let rec get_find_branch_then_term brl patl p =
  match brl,patl with 
    | [],[] -> p
    | [],_ | _,[] -> Parsing_helper.internal_error "get_find_branch_process: lists not of the same size"
    | br::brl',pat::patl' ->
        let t1=Terms.term_from_binderref br in
        (match pat with
           | PatVar (b) ->
	       let subst = Terms.OneSubst(b,t1,ref false) in
	       let patl'' = List.map (Terms.copy_pat subst) patl' in
	       let p' = Terms.copy_term subst p in
               get_find_branch_then_term brl' patl'' p'
           | PatEqual (t2) ->
               (* at this point, this is always true *)
               get_find_branch_then_term brl' patl' p
           | _ ->
               let p' = get_find_branch_then_term brl' patl' p in
               Terms.build_term_type p'.t_type (LetE(pat, t1, p', None)))

let get_find_branch get_find_branch_then patl topt p cur_array bl =
  let ac = (List.hd bl).args_at_creation in
  let vars = List.map (fun a -> Terms.create_binder "u" (Terms.new_vname ()) a.ri_type cur_array) ac in
  let vars_t = List.map Terms.term_from_binder vars in
  let repl_indices = List.map (fun a -> Terms.create_repl_index "u" (Terms.new_vname ()) a.ri_type) ac in
  let repl_indices_t = List.map Terms.term_from_repl_index repl_indices in
  let brl = List.map (fun b -> (b,repl_indices_t)) bl in
  let t = get_find_branch_term brl patl (match topt with None -> Terms.make_true () | Some t -> t) in
  let brl' = List.map (fun b -> (b,vars_t)) bl in
  let p' = get_find_branch_then brl' patl p in
  (List.combine vars repl_indices,brl,
   Terms.update_args_at_creation (repl_indices @ cur_array) t,p')

(* When a pattern =M of [get] does not contain a simple term, 
   first store the term M in a variable before evaluating the [get]. 
   That's necessary to avoid having new/event/insert or variables
   with array references inside conditions of [find]. *)
    
let rec trf_expand_pat accu cur_array = function
    PatVar b -> PatVar b
  | PatTuple(f,tl) -> PatTuple(f, List.map (trf_expand_pat accu cur_array) tl)
  | PatEqual t ->
      if not (Terms.check_simple_term t) then
	let b = Terms.create_binder "patv" (Terms.new_vname()) 
	    t.t_type cur_array
	in
	accu := (PatVar b, t) :: (!accu);
	PatEqual (Terms.term_from_binder b)
      else
	PatEqual t
    
let rec trf_get_term l cur_array t =
  match t.t_desc with
    Var(b, tl) ->
      Terms.build_term2 t (Var(b, List.map (trf_get_term l cur_array) tl))
  | FunApp(f, tl) ->
      Terms.build_term2 t (FunApp(f, List.map (trf_get_term l cur_array) tl))
  | ReplIndex _ -> t
  | TestE(t1, t2, t3) ->
      Terms.build_term2 t (TestE(trf_get_term l cur_array t1,
				 trf_get_term l cur_array t2,
				 trf_get_term l cur_array t3))
  | FindE(l0,t3,find_info) ->
      let l0' =
	List.map (fun (bl,def_list,t1,t2) ->
	  let cur_array' = (List.map snd bl) @ cur_array in
	  (bl, def_list, trf_get_term l cur_array' t1,
	   trf_get_term l cur_array t2)
	    ) l0
      in
      Terms.build_term2 t (FindE(l0', trf_get_term l cur_array t3, find_info))
  | LetE(pat, t1, t2, topt) ->
      Terms.build_term2 t (LetE(trf_get_pat l cur_array pat,
				trf_get_term l cur_array t1,
				trf_get_term l cur_array t2,
				match topt with
				  None -> None
				| Some t3 -> Some (trf_get_term l cur_array t3)))
  | ResE(b,t1) ->
      Terms.build_term2 t (ResE(b, trf_get_term l cur_array t1))
  | EventAbortE _ -> t
  | EventE(t1,p) ->
      Terms.build_term2 t (EventE(trf_get_term l cur_array t1,
				  trf_get_term l cur_array p))
  | InsertE _ ->
      Parsing_helper.internal_error "Insert should have been removed by previous transformation"
  | GetE(tbl,patl,topt,p1,p2) ->
      Settings.changed := true;
      let accu = ref [] in
      let patl' = List.map (trf_expand_pat accu cur_array) patl in
      let let_bindings = List.map (fun (pat, t) ->
	(pat, trf_get_term l cur_array t)) !accu
      in
      let topt' =
	match topt with
	  None -> None
	| Some t -> Some (trf_get_term l cur_array t)
      in
      let p1'=trf_get_term l cur_array p1 in
      let p2'=trf_get_term l cur_array p2 in
      Terms.put_lets_term let_bindings
	(Terms.build_term2 t (FindE (List.map (get_find_branch get_find_branch_then_term patl' topt' p1' cur_array) (get_info_for tbl l), p2', Nothing))) None

and trf_get_pat l cur_array = function
    PatVar b -> PatVar b
  | PatTuple(f,patl) -> PatTuple(f,List.map (trf_get_pat l cur_array) patl)
  | PatEqual t -> PatEqual (trf_get_term l cur_array t)


let rec trf_get_iprocess l cur_array p =
  Terms.iproc_from_desc (
    match p.i_desc with
      | Nil -> Nil
      | Par(p1,p2) -> 
          Par(trf_get_iprocess l cur_array p1,
              trf_get_iprocess l cur_array p2)
      | Repl(b,p) ->
          Repl(b,trf_get_iprocess l (b::cur_array) p)
      | Input((c,tl),pat,p) ->
	  List.iter check_no_get tl;
	  if has_get_pat pat then
	    let b = Terms.create_binder "patv" (Terms.new_vname()) 
		(Terms.get_type_for_pattern pat) cur_array
	    in
	    let bterm = Terms.term_from_binder b in
	    let p' = Terms.oproc_from_desc (Let(pat, bterm, p,
						Terms.oproc_from_desc Yield))
	    in
	    Input((c,tl),PatVar b, trf_get_oprocess l cur_array p')
	  else
            Input((c,tl),pat,trf_get_oprocess l cur_array p))

and trf_get_oprocess l cur_array p =
  match p.p_desc with
  | Yield | EventAbort _ -> p
  | Restr(b,p) ->
      Terms.oproc_from_desc (Restr(b,trf_get_oprocess l cur_array p))
  | Test(t,p1,p2) ->
      Terms.oproc_from_desc
	(Test(trf_get_term l cur_array t,
	      trf_get_oprocess l cur_array p1,
	      trf_get_oprocess l cur_array p2))
  | Find(l0,p,fi) ->
      let l0'=List.map 
          (fun (bl,d,t,p)->
	    let cur_array' = (List.map snd bl) @ cur_array in
            (bl,d,trf_get_term l cur_array' t,
	     trf_get_oprocess l cur_array p)) l0 in
      Terms.oproc_from_desc (Find(l0',trf_get_oprocess l cur_array p,fi))
  | Output((c, tl),t,p) ->
      List.iter check_no_get tl;
      Terms.oproc_from_desc
	(Output((c, tl),trf_get_term l cur_array t,
		trf_get_iprocess l cur_array p))
  | Let(pat,t,p1,p2) ->
      Terms.oproc_from_desc
	(Let(trf_get_pat l cur_array pat,
	     trf_get_term l cur_array t,
	     trf_get_oprocess l cur_array p1,
	     trf_get_oprocess l cur_array p2))
  | EventP(t,p) ->
      Terms.oproc_from_desc
	(EventP(trf_get_term l cur_array t,
		trf_get_oprocess l cur_array p))
  | Insert _ ->
      Parsing_helper.internal_error "Insert should have been removed by previous transformation"
  | Get(tbl,patl,topt,p1,p2) ->
      Settings.changed := true;
      let accu = ref [] in
      let patl' = List.map (trf_expand_pat accu cur_array) patl in
      let let_bindings = List.map (fun (pat, t) ->
	(pat, trf_get_term l cur_array t)) !accu
      in
      let topt' =
	match topt with
	  None -> None
	| Some t -> Some (trf_get_term l cur_array t)
      in
      let p1'=trf_get_oprocess l cur_array p1 in
      let p2'=trf_get_oprocess l cur_array p2 in
      Terms.put_lets let_bindings
        (Terms.oproc_from_desc (Find (List.map (get_find_branch get_find_branch_then_process patl' topt' p1' cur_array) (get_info_for tbl l), p2', Nothing)))
	(Terms.oproc_from_desc Yield)
          
let transform_get p l =
  trf_get_iprocess l [] p

let reduce_tables g =
  Terms.array_ref_process g.proc;
  let (p,l) = transform_insert g.proc in
  let tables = ref [] in
  List.iter (fun (tbl,_) ->
    if not (List.memq tbl (!tables)) then tables := tbl :: (!tables)) l;
  let g1 = { proc = transform_get p l; game_number = -1; current_queries = g.current_queries } in
  Terms.cleanup_array_ref();
  let (g', proba, renames) = Transf_auto_sa_rename.auto_sa_rename g1 in
  (g', proba, renames @ (List.map (fun tbl -> DExpandGetInsert tbl) (!tables)))


