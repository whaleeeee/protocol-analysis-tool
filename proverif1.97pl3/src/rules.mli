(*************************************************************
 *                                                           *
 *  Cryptographic protocol verifier                          *
 *                                                           *
 *  Bruno Blanchet, Vincent Cheval, and Marc Sylvestre       *
 *                                                           *
 *  Copyright (C) INRIA, CNRS 2000-2017                      *
 *                                                           *
 *************************************************************)

(*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details (in file LICENSE).

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*)
open Types
  
val add_not : fact -> unit
val add_elimtrue : int * fact -> unit
val add_equiv : fact list * fact * int -> unit

val display_debug : bool ref

val implies : reduction -> reduction -> bool
val reorder : fact list -> fact list

val completion : reduction list -> unit
val resolve_hyp : reduction -> reduction list
val query_goal_std : fact -> reduction list

val main_analysis : reduction list -> fact list -> unit
val bad_derivable : reduction list -> reduction list
val sound_bad_derivable : reduction list -> reduction list


val reset : unit -> unit
