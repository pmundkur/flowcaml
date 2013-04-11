(**************************************************************************)
(*                                                                        *)
(*                                  Dalton                                *)
(*                      an efficient implementation of                    *)
(*                 type inference with structural subtyping               *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002, 2003 Institut National de Recherche en Informatique   *)
(*  et en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(*  Author contact: Vincent.Simonet@inria.fr                              *)
(*  Software page: http://cristal.inria.fr/~simonet/soft/dalton/          *)
(*                                                                        *)
(**************************************************************************)

(* $Id: dalton.ml,v 1.16 2003/09/24 15:25:35 simonet Exp $ *)

open Dalton_aux
open Dalton_sig
open Dalton_lib
open Format

let (!!) = Proxy.desc
module Tree = Dalton_list

type t

module NdMask = struct

  let create =
    let i = ref (-1) in
    fun () -> incr i; 1 lsl !i

  let positive =    create ()
  let negative =    create ()
  let decomposed =  create ()
  let unification = create ()
  let simplify =    create ()
  let reduction =   create ()
  let reduction' =  create ()
  let terminal =    create ()
  let terminal' =   create ()
  let wpred =       create ()
  let spred =       create ()
  let wsucc =       create ()
  let ssucc =       create ()
  let closed =      create ()

  let expand = create ()

  let bipolar =     positive lor negative
  let closure =     wpred lor spred lor wsucc lor ssucc

end

module SkMask = struct

  let create =
    let i = ref (-1) in
    fun () -> incr i; 1 lsl !i

  let polar =       create ()
  let decomposed =  create ()
  let unification = create ()
  let simplify =    create ()
  let reduction =   create ()
  let reduction' =  create ()
  let terminal =    create ()
  let terminal' =   create ()
  let wpred =       create ()
  let wsucc =       create ()

  let closure =     wpred lor wsucc

end



(** The solver is parametrized by three modules whose signatures are
    given in the module {!Dalton_sig}.
 *)
module Make (Ground: GROUND) (Print: PRINT) (Draw: DRAW) (Report: ERROR_REPORT) 
    = struct

(***************************************************************************)
(** {2 The first order logic}

    This section defines datatypes for representing variables
    (i.e. nodes) and formulas (i.e. constraints) of the first order
    logic.  It also provides basic operations on these data
    structures.  The definition of the first order logic naturally
    relies on the ground signature {!G}.
   *)

  open Ground



  (*-----------------------------------------------------------------------*)
  (** {3 Datatypes definitions} *)

  (** Descriptors allow representing small terms.  The [descriptor] type
      is paremtrized by that of the sub-terms.
   *)
  type 'a descriptor = 
      Variable
    | Type of 'a Type.t
    | Row of Label.t * 'a * 'a



  (** A node (of type [node]) represents a multi-equation.
      Information about the multi-equation is stored in a [node_desc]
      record which is encapsuled into a proxy in order to allow low-cost
      unification of nodes.  Because multi-equations are immediatly 
      decomposed they involve at most one small term, which is stored in the
      [nd_descriptor] field.
  *) 
  type node = 
      node_desc Proxy.t


  (** A [node_desc] record gives all informations about a node.
   *)
  and node_desc = {

      nd_skeleton: skeleton;
      (** Every node carry a pointer to the skeleton it belongs to. *)

      mutable nd_descriptor: node descriptor;
      (** If the node as some known structure (i.e. the multi-equation
	  contains a non-variable small term), it is recorded in this 
	  field thanks to a descriptor. *)      

      mutable nd_spred: node Tree.t;
      mutable nd_ssucc: node Tree.t;
      mutable nd_wpred: node_or_skeleton Tree.t;
      mutable nd_wsucc: node_or_skeleton Tree.t;
      mutable nd_lb: Lb.t;
      mutable nd_ub: Ub.t;
      (** Inequalities involving the current node are recorded in these
	  fields.
       *)

      mutable nd_flags: int;

      mutable nd_rigid: node_rigid option;

      mutable nd_copy: node option;
      (** This transient field is used by the {!Copy} module. *)

      mutable nd_tarjan: int;
      (** This transient field is used by the {!Node_tarjan} and the
	  {!Node_tarjan_atomic} modules. *)

      mutable nd_print: nd_print_info option;
      (** This transient field is used by the {!Printing} module. *)

      nd_stamp: int
      (** Every node carries an unique integer stamp. *);

    } 

  and node_rigid = {
      ndr_spred: node Tree.t;
      ndr_ssucc: node Tree.t;
      ndr_wpred: node_or_skeleton Tree.t;
      ndr_wsucc: node_or_skeleton Tree.t;
      ndr_lb: Lb.t;
      ndr_ub: Ub.t
    } 


  (** A skeleton (of type [skeleton]) represents a multi-skeleton.
      Information about the multi-skeleton is stored in a
      [skeleton_desc] record which is encapsuled into a proxy in order
      to allow low-cost unification of skeletons.
   *)
  and skeleton = skeleton_desc Proxy.t

  (** A [skeleton_desc] record gives all informations about a skeleton.
   *)
  and skeleton_desc = {

      sk_cset: cset;
      (** Every skeleton carry a pointer to the constraint set it
	  belongs to. *)

      mutable sk_nodes: node Tree.t;
      (** Every skeleton carry the list of its nodes. *)

      mutable sk_kind: kind;
      (** This field records the common kind of all the nodes of the
	  skeleton. *)

      mutable sk_descriptor: skeleton descriptor;
      (** If the current skeleton has some known structure (because one
	  of its nodes has so), it is recorded in this field. *)

      mutable sk_wpred: node_or_skeleton Tree.t;
      mutable sk_wsucc: node_or_skeleton Tree.t;
      mutable sk_lb: Lb.t;
      mutable sk_ub: Ub.t;
      (** Inequalities involving the current skeleton are recorded in these
	  fields.  Let [sk] be the current skeleton.  A constraint
	  [d(sk) < rhs] is recorded by an entry [rhs] in the [sk_succ] list.
	  Similarly, a constraint [lhs < d(sk)] is recorded by [lhs] in
	  [sk_pred].
       *)

      mutable sk_flags: int;

      mutable sk_rigid: skeleton_rigid option;

      mutable sk_copy: skeleton option;
      (** This transient field is used by the {!Copy} module. *)

      mutable sk_topological: int;
      (** This transient field is used by the {!SkWalk} {!WeakTarjan} 
	  modules. *)

      mutable sk_print: sk_print_info option;
      (** This transient field is used by the {!Printing} module. *)

      sk_stamp: int;
      (** Every skeleton carries an unique integer stamp. *)
    } 

  and skeleton_rigid = {
      skr_wpred: node_or_skeleton Tree.t;
      skr_wsucc: node_or_skeleton Tree.t;
      skr_lb: Lb.t;
      skr_ub: Ub.t
    } 


  (** A constraint set is represented by a value of type [cset].  
      Information about the set is recorded in a record of type [cset_desc] 
      which is encapsulated into a proxy in order to allow low-cost merging
      of constraint sets.
   *)
  and cset = cset_desc Proxy.t

  (** A [cset_desc] record gives the information about a constraints set.
   *)
  and cset_desc = {

      mutable cs_skeletons: skeleton Tree.t;
      (** Every constraints set carries the list of its skeletons. *)

      mutable cs_copy: cset option
      (** This transient field is used by the [!Copy] module. *)
    } 



  (** 
   *)
  and node_or_skeleton =
      Nd of node
    | Sk of skeleton



  (** Transient information for pretty-printing of nodes are stored
      in a [nd_print_info] record. *)
  and nd_print_info = {
      mutable ndpi_occurences: int;
      mutable ndpi_negative: bool;
      mutable ndpi_positive: bool;
      mutable ndpi_noghost: bool;
      
      mutable ndpi_wpred: node_or_skeleton Tree.t;
      mutable ndpi_wsucc: node_or_skeleton Tree.t;

      mutable ndpi_output_mode: nd_print_output_mode;
      mutable ndpi_name: int option;
      mutable ndpi_tag: string option;
      ndpi_wcc: int ref Proxy.t;
      ndpi_scc: int ref Proxy.t;

      (* For drawing: *)
      mutable ndpi_positions: (variance * int) list
    }	

  (** Transient information for pretty-printing of skeletons are stored
      in a [sk_print] record. *)
  and sk_print_info = {
      mutable skpi_wpred: node_or_skeleton Tree.t;
      mutable skpi_wsucc: node_or_skeleton Tree.t;
      skpi_wcc: int ref Proxy.t;
      mutable skpi_name: int option;
      mutable skpi_tag: string option
    } 

  and nd_print_output_mode =
      OmGhostVariable
    | OmVariable
    | OmTerm
    | OmLb of Lb.t
    | OmUb of Ub.t
    | OmLub of Lb.t * Ub.t



  (*-----------------------------------------------------------------------*)
  (** {3 Operations on descriptors} *)

  (** The module [Descriptor] implements iterators on descriptors.  They 
      are mostly extensions of those on types provided by the module
      [Ground.Type]. 
   *)
  module Descriptor = struct

    let row_arg =
      { variance = Covariant;
	kind = Katom;
	ldestr = true;
	rdestr = true
      }	

    (** [map f d] returns the descriptor obtained by replacing every 
        sub-term [t] of [d] by [f i t]. 
     *)
    let map f = function
	Variable -> Variable
      |	Type t -> Type (Type.map f t)
      |	Row (lbl, x, y) -> Row (lbl, f row_arg x, f row_arg y)

    (** [iter f d] applies [f] on every sub-term of [d].
     *)
    let iter f = function
	Variable -> ()
      |	Type t -> Type.iter f t
      |	Row (_, x, y) -> f row_arg x; f row_arg y

    (** Given two descriptors of the same form, [iter2 f d1 d2] applies
        [f] on every pair of corresponding sub-terms of [d1] and [d2].
     *)
    let iter2 f d1 d2 =
      match d1, d2 with
	Variable, Variable -> ()
      |	Type t1, Type t2 -> Type.iter2 f t1 t2
      |	Row (lbl1, nd_lbl1, nd'1), Row (lbl2, nd_lbl2, nd'2) ->
	  assert (Label.compare lbl1 lbl2 = 0);
	  f row_arg nd_lbl1 nd_lbl2;
	  f row_arg nd'1 nd'2
      |	_ -> assert false

    (** Given two compatible descriptors [d1] and [d2], [map2 f d1 d2]...
        The result is not specified if [d1] and [d2] do not correspond. 
     *)
    let map2 f d1 d2 = 
      match d1, d2 with
	Variable, Variable -> Variable
      |	Type t1, Type t2 -> Type (Type.map2 f t1 t2)
      |	Row (lbl1, x1, y1), Row(lbl2, x2, y2) ->
	  assert (Label.compare lbl1 lbl2 = 0);
	  Row (lbl1, f row_arg x1 x2, f row_arg y1 y2)
      |	_ -> assert false

    (** [for_all2 f d1 d2] tests wether the two descriptors have
        compatible forms and for all pair of corresponding sub-terms
        [t1] and [t2], [f t1 t2] is true.  If one of the descriptor is
        [Variable], the function returns [false].
     *)
    let for_all2 f d1 d2 =
      match d1, d2 with
	Type t1, Type t2 -> Type.for_all2 f t1 t2
      |	Row (lbl1, x1, y1), Row (lbl2, x2, y2) -> 
	  (Label.compare lbl1 lbl2 = 0) && f row_arg x1 x2 && f row_arg y1 y2
      |	_ -> false

    let ldestr = function
	Variable | Row _ -> true
      |	Type t -> Type.ldestr t

    let rdestr = function
	Variable | Row _ -> true
      |	Type t -> Type.rdestr t

  end






  (*-----------------------------------------------------------------------*)
  (** {3 Operations on skeletons} *)

  let skd_set mask skd =
    skd.sk_flags <- (skd.sk_flags lor mask)
  let skd_unset mask skd =
    skd.sk_flags <- skd.sk_flags land (lnot mask)
  let skd_test mask skd = 
    (skd.sk_flags land mask) <> 0
  let skd_not mask skd =
    (skd.sk_flags land mask) = 0

  let sk_set mask sk = skd_set mask (!! sk)
  let sk_unset mask sk = skd_unset mask (!! sk)
  let sk_test mask sk = skd_test mask (!! sk)
  let sk_not mask sk = skd_not mask (!! sk)

  let skd_mark mask skd =
    let flags = skd.sk_flags in
    let flags' = skd.sk_flags lor mask in
    if flags = flags' then false else begin
      skd.sk_flags <- flags';
      true
    end

  let sk_mark mask sk =
    let skd = !! sk in
    let flags = skd.sk_flags in
    let flags' = skd.sk_flags lor mask in
    if flags = flags' then false else begin
      skd.sk_flags <- flags';
      true
    end



  (** [sk_node nd] returns one (arbitrary) node of the skeleton [sk].
   *)
  let sk_node sk =
    assert (not (Tree.is_empty (!! sk).sk_nodes));
    Tree.choose (!! sk).sk_nodes

  (** [sk_terminal sk] returns [true] if the skeleton [sk] is terminal, i.e. if 
      its descriptor is [Variable].
   *)
  let sk_terminal sk =
    (!! sk).sk_descriptor = Variable

  (** [sk_atomic nd] returns [true] if and only if [sk] has kind [Katom].
   *)
  let sk_atomic sk =
    Kind.atomic (!! sk).sk_kind

  (** [sk_rigid sk] returns [true] if and only if [sk] is a rigid skeleton.
   *)
  let sk_rigid sk =
    match (!! sk).sk_rigid with
      None -> false
    | Some _ -> true

  (** [sk_same sk1 sk2] returns [true] if and only if the two skeletons
      [sk1] and [sk2] are the same. *)
  let sk_same sk1 sk2 =
    (!! sk1) == (!! sk2)

  (** [sk_compare sk1 sk2] compares the stamps of the skeletons [sk1] and [sk2]
   *)
  let sk_compare sk1 sk2 =
    compare (!! sk1.sk_stamp) (!! sk2.sk_stamp)



  (*-----------------------------------------------------------------------*)
  (** {3 Operations on nodes} *)

  let ndd_set mask ndd = 
    ndd.nd_flags <- ndd.nd_flags lor mask
  let ndd_unset mask ndd = 
    ndd.nd_flags <- ndd.nd_flags land (lnot mask)
  let ndd_test mask ndd =
    (ndd.nd_flags land mask) <> 0
  let ndd_not mask ndd = 
    (ndd.nd_flags land mask) = 0

  let nd_set mask nd = ndd_set mask (!! nd)
  let nd_unset mask nd = ndd_unset mask (!! nd)
  let nd_test mask nd = ndd_test mask (!! nd)
  let nd_not mask nd = ndd_not mask (!! nd)

  let ndd_mark mask ndd =
    let flags = ndd.nd_flags in
    let flags' = ndd.nd_flags lor mask in
    if flags = flags' then false else begin
      ndd.nd_flags <- flags';
      true
    end

  let nd_mark mask nd =
    let ndd = !! nd in
    let flags = ndd.nd_flags in
    let flags' = ndd.nd_flags lor mask in
    if flags = flags' then false else begin
      ndd.nd_flags <- flags';
      true
    end


  (** [nd_skeleton nd] returns the skeleton of the node [nd].
   *)
  let nd_skeleton nd =
    (!! nd).nd_skeleton

  (** [nd_kind nd] returns the kind of the node [nd].
   *)
  let nd_kind nd =
    (!! (nd_skeleton nd)).sk_kind

  (** [nd_atomic nd] returns [true] if and only if [nd] is atomic.
   *)
  let nd_atomic nd =
    Kind.atomic (nd_kind nd)

  (** [nd_rigid nd] returns [true] if and only if [nd] is a rigid node.
   *)
  let nd_rigid nd =
    match (!! nd).nd_rigid with
      None -> false
    | Some _ -> true

  (** [nd_same nd1 nd2] returns [true] if and only if the two nodes
      [nd1] and [nd2] are the same. *)
  let nd_same nd1 nd2 =
    (!! nd1) == (!! nd2)

  (** [nd_compare nd1 nd2] compares the stamps of the nodes [nd1] and [nd2]
   *)
  let nd_compare nd1 nd2 =
    compare (!! nd1.nd_stamp) (!! nd2.nd_stamp)



  (*-----------------------------------------------------------------------*)
  (** {3 Operations on nodes or skeletons} *)

  let ns_mark nd_mask sk_mask = function
      Nd nd -> nd_mark nd_mask nd
    | Sk sk -> sk_mark sk_mask sk

  let ns_unset nd_mask sk_mask = function
      Nd nd -> nd_unset nd_mask nd
    | Sk sk -> sk_unset sk_mask sk

  let ns_same ns1 ns2 =
    match ns1, ns2 with
      Nd nd1, Nd nd2 -> nd_same nd1 nd2
    | Sk sk1, Sk sk2 -> nd_same sk1 sk2
    | (Nd _, Sk _) | (Sk _, Nd _) -> false

  let ns_notdecomposed = function
      Nd nd -> nd_not NdMask.decomposed nd
    | Sk sk -> sk_not SkMask.decomposed sk

  let ns_negative = function
      Nd nd -> nd_test NdMask.negative nd
    | Sk sk -> sk_test SkMask.polar sk

  let ns_positive = function
      Nd nd -> nd_test NdMask.positive nd
    | Sk sk -> sk_test SkMask.polar sk

  let ns_lb = function
      Nd nd -> (!! nd).nd_lb
    | Sk sk -> (!! sk).sk_lb

  let ns_ub = function
      Nd nd -> (!! nd).nd_ub
    | Sk sk -> (!! sk).sk_ub

  let ns_compare ns1 ns2 =
    match ns1, ns2 with
      Nd nd1, Nd nd2 -> nd_compare nd1 nd2
    | Sk sk1, Sk sk2 -> sk_compare sk1 sk2
    | Nd _, Sk _ -> -1
    | Sk _, Nd _ -> 1


  (*-----------------------------------------------------------------------*)
  (** {3 Basic operations on constraint sets} *)

  (** [merge_cset cs1 cs2] unifies the two constraint sets [cs1] and [cs2].
   *)
  let merge_cset cs1 cs2 =
    let cs1d = !! cs1
    and cs2d = !! cs2 in
    if cs1d != cs2d then begin
      cs1d.cs_skeletons <- Tree.union cs1d.cs_skeletons cs2d.cs_skeletons;
      Proxy.linksto cs2 cs1
    end


  (** [reset_transient_fields cs] resets transient fields of nodes and 
      skeletons using throughout solving of the constraint set [cs].
   *)
  let reset_transient_fields cs =
    Tree.iter (function sk ->
      let skd = !! sk in
      skd_unset SkMask.decomposed skd;
      skd_unset SkMask.polar skd;
      Tree.iter (function nd ->
	let ndd = !! nd in
	ndd_unset NdMask.negative ndd;
	ndd_unset NdMask.positive ndd;
	ndd_unset NdMask.decomposed ndd;
      ) skd.sk_nodes
    ) (!! cs).cs_skeletons



  (*-----------------------------------------------------------------------*)
  (** {3 Internal function for creating nodes, skeletons and constraints
         sets}

      In this section defines low-level functions for creating fresh
      skeletons and nodes.  (User level functions will be provided in
      the next section.)  
   *)

  (** [cs_create ()] returns a fresh empty constraint set. 
   *)
  let cs_create () =
    Proxy.create
      { cs_skeletons = Tree.empty;
	cs_copy = None
      }



  (** Every node or skeleton must carry an unique integer stamp (e.g. for
      hash-consing).  Each invokation of [stamp ()] returns a distinct
      integer stamp.
   *)
  let stamp =
    let counter = ref 0 in
    fun () -> incr counter; ! counter



  (** [sk_create cs kind desc] returns a fresh skeleton in the constraint set
      [cs], with the given kind and descriptor.
   *)
  let sk_create cs kind desc =
   
    let sk =
      Proxy.create      
	{ sk_cset = cs;
          sk_nodes = Tree.empty;
	  sk_kind = kind;
	  sk_descriptor = desc;
	  sk_wpred = Tree.empty;
	  sk_wsucc = Tree.empty;
	  sk_lb = Lb.bottom;
	  sk_ub = Ub.top;
	  sk_rigid = None;
	  sk_flags = 0;
	  sk_copy = None;
	  sk_topological = 0;
	  sk_print = None;
	  sk_stamp = stamp ()
	}
    in

    let csd = !! cs in
    csd.cs_skeletons <- Tree.add sk csd.cs_skeletons;

    sk



  (** [nd_create_in_sk sk desc] creates a fresh node in the skeleton [sk]
      with the descriptor [desc].
   *)
  let nd_create_in_sk sk desc =

    assert (Dalton_debug.Stat.node ());
    assert (if desc = Variable then true 
    else Dalton_debug.Stat.node_variable ());

    let nd =
      Proxy.create
	{ nd_skeleton = sk;
	  nd_descriptor = desc;
	  nd_wpred = Tree.empty;
	  nd_wsucc = Tree.empty;
	  nd_spred = Tree.empty;
	  nd_ssucc = Tree.empty;
	  nd_lb = Lb.bottom;
	  nd_ub = Ub.top;
	  nd_flags = 0;
	  nd_rigid = None;
	  nd_copy = None;
	  nd_tarjan = 0;
	  nd_print = None;
	  nd_stamp = stamp ()
	} 
	
    in

    let skd = !! sk in
    skd.sk_nodes <- Tree.add nd skd.sk_nodes;
    
    nd



  (** [nd_create cs kind desc] returns a fresh node in the constraint set
      [cs], with the given kind and descriptor.  This function also creates
      a fresh skeleton with the same structure as the node. 
   *)
  let nd_create cs kind desc =

    let sk = 
      sk_create cs kind
	(Descriptor.map (fun _ nd -> (!! nd).nd_skeleton) desc)
    in

    nd_create_in_sk sk desc



  (*-----------------------------------------------------------------------*)
  (** {3 User level functions for creating nodes}

      The following functions are user-level primitives for creating fresh
      constraint sets and nodes.
   *)

  (** [cs_create ()] returns a fresh empty constraint set. *)
  let cset () =
    cs_create ()

  (** [variable cs kind] returns a fresh node of kind [kind] in the 
      constraint set [cs].
   *)
  let variable cs kind =
    assert (Dalton_debug.Stat.node_ext ());
    nd_create cs kind Variable

  (** [variable_in_sk nd] returns a fresh node in the same skeleton as [nd]. 
   *)
  let variable_in_sk nd =
    assert (Dalton_debug.Stat.node_ext ());
    nd_create_in_sk (!! nd).nd_skeleton Variable

  (** [typ cs t] returns a fresh node in the constraint set [cs] whose
      descriptor is [Type t]. *)
  let typ cs t =
    assert (Dalton_debug.Stat.node_ext ());
    nd_create cs Ktype (Type t)

  (** [row cs (lbl, nd_lbl, nd')] returns a fresh node in the contraint set [cs]
      whose descriptor is [Row (lbl, nd_lbl, nd')].
    *)
  let row cs (lbl, nd_lbl, nd') =
    assert (Dalton_debug.Stat.node_ext ());
    nd_create cs (Krow (nd_kind nd_lbl)) (Row (lbl, nd_lbl, nd'))



(***************************************************************************)
(** {2 Copying nodes and skeletons} 

    The module [Copy] provides functions for copying constraint sets. 
    The constraint set [cs] can be copied as follows:
    - Invoke [make cs].  This creates the copy of the entire [cs] in memory
      and return a pointer to the new constraint set.  As a side effect,
      every node or skeleton of the original set contains a pointer to its
      copy.
    - Access to the copy of nodes/skeletons you need by [nd_get_copy]
      and [sk_get_copy].
    - Invoke [clean cs] to clean every transient field.

    Functions of this module use standard iterators on list (i.e. we
    assume that the copyied structure is clean).
*)

  (** A substitution may be applied on constant bounds, type constructors
      and row labels throughout copy.  Such a substitution is given
      by four functions stored in a record of type [subst].
   *)
  type subst = {
      lb: Lb.t -> Lb.t;
      ub: Ub.t -> Ub.t;
      typ: 'a. 'a Type.t -> 'a Type.t;
      label: Label.t -> Label.t
    } 



module Copy = struct

  (** The substitution [identity] allows computing copy identical to
      the original.  It is the default substitution for all the following
      functions.
   *)
  let identity =
    { lb = (function lb -> lb);
      ub = (function ub -> ub);
      typ = (function typ -> typ);
      label = (function lbl -> lbl)
    } 

  
  (** [subst_descriptor subst d] returns the descriptor obtained by
      applying the substitution [subst] on [d].
   *)
  let subst_descriptor subst = function
      Variable -> Variable
    | Type t -> Type (subst.typ t)
    | Row (lbl, x, y) -> Row (subst.label lbl, x, y)



  (** [sk_get_copy sk] returns the current copy of the skeleton [sk]. *)
  let sk_get_copy sk =
    match (!! sk).sk_copy with
      None -> assert false
    | Some sk' -> sk'

  (** [nd_get_copy nd] returns the current copy of the node [nd]. *)
  let nd_get_copy nd =
    match (!! nd).nd_copy with
      None -> assert false
    | Some nd' -> nd'

  (** [ns_get_copy ns] returns the current copy of the node-or-skeleton [ns]. *)
  let ns_get_copy = function
      Sk sk -> Sk (sk_get_copy sk)
    | Nd nd -> Nd (nd_get_copy nd)



  (** [make ?subst cs] makes the copy of the constraint set [cs].  Every 
      skeleton or node of the cset is copied, and a reference to the copy 
      is made in the field [nd_copy] or [sk_copy] of the original.
      The substition [subst] is applied while copying, if no substitution is
      provided, the identity is used.
   *)
  let make ?(subst=identity) cs =

    let csd = !! cs in

    assert (csd.cs_copy = None);

    let cs' = cs_create () in
    csd.cs_copy <- Some cs';

    (* Copy of skeletons and nodes *)

    Tree.iter (function sk ->

      let skd = !! sk in
      assert (skd.sk_copy = None);
      let sk' = sk_create cs' skd.sk_kind Variable in
      skd.sk_copy <- Some sk';

      Tree.iter (fun nd -> 
	let ndd = !! nd in
	assert (ndd.nd_copy = None);
	let nd' = nd_create_in_sk sk' Variable in
	ndd.nd_copy <- Some nd';
      ) skd.sk_nodes

    ) csd.cs_skeletons;

    (* Copy of constraints *)

    Tree.iter (function sk ->

      let skd = !! sk in
      let sk' = sk_get_copy sk in
      let sk'd = !! sk' in
      sk'd.sk_descriptor <-
	(subst_descriptor subst 
	   (Descriptor.map (fun _ sk -> sk_get_copy sk) skd.sk_descriptor));
      sk'd.sk_wpred <- Tree.map ns_get_copy skd.sk_wpred;
      sk'd.sk_wsucc <- Tree.map ns_get_copy skd.sk_wsucc;
      sk'd.sk_lb <- subst.lb skd.sk_lb;
      sk'd.sk_ub <- subst.ub skd.sk_ub;

      Tree.iter (function nd ->

	let ndd = !! nd in
	let nd' = nd_get_copy nd in
	let nd'd = !! nd' in
	nd'd.nd_descriptor <- 
	  (subst_descriptor subst
	     (Descriptor.map (fun _ nd -> nd_get_copy nd) ndd.nd_descriptor));
	nd'd.nd_spred <- Tree.map nd_get_copy ndd.nd_spred;
	nd'd.nd_ssucc <- Tree.map nd_get_copy ndd.nd_ssucc;
	nd'd.nd_wpred <- Tree.map ns_get_copy ndd.nd_wpred;
	nd'd.nd_wsucc <- Tree.map ns_get_copy ndd.nd_wsucc;
	nd'd.nd_lb <- subst.lb ndd.nd_lb;
	nd'd.nd_ub <- subst.ub ndd.nd_ub

      ) skd.sk_nodes;
      
    ) csd.cs_skeletons;
    
    cs'



  (** [clean cs] resets every transient field created during the copy of
      the constraint set [cs].
   *)
  let clean cs =

    let csd = !! cs in
    csd.cs_copy <- None;

    Tree.iter (function sk ->
      let skd = !! sk in
      skd.sk_copy <- None;
      Tree.iter (function nd -> (!! nd).nd_copy <- None) skd.sk_nodes
    ) csd.cs_skeletons



  let make_skeletons cs =

    let csd = !! cs in

    assert (csd.cs_copy = None);

    let cs' = cs_create () in
    csd.cs_copy <- Some cs';

    (* Copy of skeletons *)

    Tree.iter (function sk ->

      let skd = !! sk in
      assert (skd.sk_copy = None);
      let sk' = sk_create cs' skd.sk_kind Variable in
      skd.sk_copy <- Some sk'

    ) csd.cs_skeletons;

    (* Copy of descriptors *)

    Tree.iter (function sk ->

      let skd = !! sk in
      let sk' = sk_get_copy sk in
      let sk'd = !! sk' in
      sk'd.sk_descriptor <- 
	Descriptor.map (fun _ sk -> sk_get_copy sk) skd.sk_descriptor
      
    ) csd.cs_skeletons;

    cs'



  let make_nodes cs =

    let csd = !! cs in

    assert (csd.cs_copy <> None);

    (* Copy of nodes *)

    Tree.iter (function sk ->

      let skd = !! sk in
      let sk' = sk_get_copy sk in

      Tree.iter (fun nd -> 
	let ndd = !! nd in
	let nd' = nd_create_in_sk sk' Variable in
	ndd.nd_copy <- Some nd';
      ) skd.sk_nodes

    ) csd.cs_skeletons;

    (* Copy of descriptors *)

    Tree.iter (function sk ->

      Tree.iter (function nd ->

	let ndd = !! nd in
	let nd' = nd_get_copy nd in
	let nd'd = !! nd' in
	nd'd.nd_descriptor <-
	  Descriptor.map (fun _ nd -> nd_get_copy nd) ndd.nd_descriptor

      ) (!! sk).sk_nodes
      
    ) csd.cs_skeletons


end



(***************************************************************************)
(** {2 Pretty-print} 

    The module [Printing] implements pretty-printing facilities for terms and
    constraint sets.  Outputs are performed through the module [Format]
    of the Objective Caml standard library.
*)

module Printing = struct

  (*-----------------------------------------------------------------------*)
  (** {3 Configuration} *)
  
  let all_variables = ref false
  let ghost_variables = ref true
  let polarities = ref false



  (*-----------------------------------------------------------------------*)
  (** {3 Preliminary operations on transient fields}

      Pretty-printing of a constraints set or a term requires several
      preliminary calculus.  The results of these operation are directly
      stored in nodes (in the transient field [nd_print]) and skeletons
      (in the transient field [sk_print]).
   *)

  (** [sk_info sk] returns the transient record storing printing informations 
      of the skeleton [sk].  If [sk] does not yet carry such a record, one
      is created on the fly.
   *)
  let sk_info sk =

    let skd = !! sk in

    match skd.sk_print with

      None ->
	let skpi = 
	  { skpi_wpred = Tree.empty;
	    skpi_wsucc = Tree.empty;
	    skpi_wcc = Proxy.create (ref 0);
	    skpi_name = None;
	    skpi_tag = None
	  } 
	in
	skd.sk_print <- Some skpi;
	skpi

    | Some skpi -> skpi



  (** [nd_info nd] returns the transient record storing printing informations 
      of the node [nd].  If [nd] does not yet carry such a record, one
      is created on the fly.
   *)
  let nd_info nd =

    let ndd = !! nd in

    match ndd.nd_print with

      None ->
	let rec pr =
	  { ndpi_occurences = 0;
	    ndpi_negative = false;
	    ndpi_positive = false;
	    ndpi_noghost = false;

	    ndpi_wpred = Tree.empty;
	    ndpi_wsucc = Tree.empty;

	    ndpi_output_mode = OmGhostVariable;
	    ndpi_name = None;
	    ndpi_tag = None;
	    ndpi_wcc = Proxy.create (ref 0);
	    ndpi_scc = Proxy.create (ref 0);

	    ndpi_positions = []
	  }
	in
	ndd.nd_print <- Some pr;
	pr

    | Some pr ->
	pr



  (** [cs_reset cs] resets all the fields storing transient informations
      for pretty-print in skeletons and nodes of the constraint set [cs].
   *)
  let cs_reset cs =

    Tree.iter (function sk ->

      let skd = !! sk in
      skd.sk_print <- None;

      Tree.iter (function nd -> (!! nd).nd_print <- None) skd.sk_nodes

    ) (!! cs).cs_skeletons



  (*-----------------------------------------------------------------------*)
  (** {4 First step: registering occurences} 

      This firsts steps consists in walking throught the term(s) and update
      some information in printed nodes:
      - number of occurences,
      - polarities of the occurences.
   *)



  (** [nd_occurence v nd] registers an occurence of the node [nd]
      at a variance [v].  Hence:
      - it updates the fields concerning polarity ([ndpi_positive] and
        [ndpi_negative]),
      - it increases the occurences counter (field [ndpi_occurences]),
      The function is called recursively on the sub nodes of [nd].
   *)
  let rec nd_occurence variance nd =

    let pr = nd_info nd in

    pr.ndpi_occurences <- pr.ndpi_occurences + 1;
    if variance <> Contravariant then pr.ndpi_positive <- true;
    if variance <> Covariant then pr.ndpi_negative <- true;

    match (!! nd).nd_descriptor with
      Variable -> ()
    | (Type _ | Row _) as d -> 
	Descriptor.iter (fun arg nd -> 
	  nd_occurence (Variance.combine arg.variance variance) nd
	) d



  (*-----------------------------------------------------------------------*)
  (** {4 Second step: dealing with constraints} 

      The second step of pre-calculus deals with constraint.  It implements
      an heuristic in order to determinate a reasonably good (i.e. concise and
      readable) manner to pretty-prints constraints.  The main points of the
      heurisitic are the following:

      - Some weak inequalities involving skeletons do not need to be printed.
        Such inequalities are detected by th function [filter_weak] (the 
        basic idea is that a constraint [d~(sk) < lhs] does not need to
        be printed if there is a node [nd] in [sk] such that [d(nd) < lhs] 
        holds.

      - In every skeleton, connex parts for strong inequalities are
        calculated.  Same-skeletons constraints may be printed by choosing
        only one representant in each part.  Moreover, for every part, an
        integer "delta" is calculated: it is the difference between the number
        of nodes of the part which have strong predecessors and those which
        have strong successors.  Therefore, if the delta of a part is negative,
        we choose to pretty-prints the strong inequalities carried by nodes
        of this part considering successors (i.e. for every node [nd] whose
        successors are [nd1, ..., ndn]), [nd < nd1, ..., ndn] is printed).
        Otherwise, predecessors are considered.

     -  Similarly, we compute the connex parts for weak inequalities and
        calculate for each of them a "delta".

     -  Non-positive (resp. non-negative) atomic nodes (of kind
        [Katom] or [Krow Katom] or ...) which have only a constant lower
        bound (resp. upper bound) may be pretty-printed as this constant.
        Similarly positive and negative nodes whose lower and upper bound
        are equal may be printed as this constant.        

     - Non-positive or non-negative) atomic nodes w.o. any predecessor
       or successor or positive and negative nodes w.o. any predecessor
       or successor which have at most one occurence has no significance
       and do not need to be printed.

     All functions of this module iterates on lists of nodes and skeletons
     using the standard function of the [List] module, assuming that
     the lists have previously be cleaned by solving.
   *)



  (** [filter_weak hs1 hs2] returns a boolean indicating wether the
      constraint [hs1 < hs2] needs to be printed.  Such an inequality is
      "useless" if it is implied by another constraint: e.g. the constraint
      [d~(sk) < rhs] is useless if there exists a node [nd] in [sk] such 
      that [d(nd) < rhs] holds.
   *)
  let filter_weak lhs rhs =

    begin match lhs with
      Nd _ -> true
    | Sk sk ->
	not (Tree.exists (function nd ->
	  Tree.exists (fun rhs' -> ns_same rhs rhs') (!! nd).nd_wsucc
        ) (!! sk).sk_nodes)
    end

    &&

    begin match rhs with
      Nd _ -> true
    | Sk sk ->
	not (Tree.exists (function nd ->
	  Tree.exists (fun lhs' -> ns_same lhs lhs') (!! nd).nd_wpred
        ) (!! sk).sk_nodes)
    end



  (** Connex parts for strong or weak inequalities are computed by
      an union-find mechanism:  every node carries two proxies 
      representing the parts it belongs to.  Proxies point to a
      reference on an integer representing the "delta" of the part, as
      explained above.  [unify_cc cc1 cc2] unifies the proxies [cc1] and 
      [cc2].
   *)
  let unify_cc cc1 cc2 =
    let cc1d = !! cc1
    and cc2d = !! cc2 in
    if cc1d != cc2d then begin
      cc1d := !cc1d + !cc2d;
      Proxy.linksto cc2 cc1
   end



  (** [unify_scc cc nd_list] registers that all nodes in the list [nd_list]
      belong to the same connex part for strong inequality.
   *)
  let unify_scc cc nd_list =
    Tree.iter (function nd ->
      unify_cc cc (nd_info nd).ndpi_scc
    ) nd_list



  (** [unify_wcc cc ns_list] registers that all nodes and skeletons in the
      list [ns_list] belong to the same connex part for weak inequality.
   *)
  let unify_wcc cc ns_list =
    Tree.iter (function
	Nd nd -> unify_cc cc (nd_info nd).ndpi_wcc
      |	Sk sk -> unify_cc cc (sk_info sk).skpi_wcc
    ) ns_list



  (** [delta_cc cc pred succ] update the delta of the proxy [cc]
      registering a node.  [pred] (resp. [succ]) is the list of predecessors
      (resp. successors) of the node.
   *)
  let delta_cc cc pred succ =
    let ccd = !! cc in
    ccd := !ccd +
	(if Tree.is_empty pred then 0 else 1)
	- (if Tree.is_empty succ then 0 else 1)



  (** [prepare_constraints cs] considers every constraint in the [cs] and
      update the transient fields of nodes and skeletons accordingly,
      in order to be able to print constraints in a good manner,
      as explained above.
   *)
  let prepare_constraints cs =

    let csd = !! cs in

    Tree.iter (function sk ->

      let skd = !! sk in
      let skpi = sk_info sk in

      skpi.skpi_wpred <- 
	Tree.filter (fun lhs -> filter_weak lhs (Sk sk)) skd.sk_wpred;
      skpi.skpi_wsucc <-
	Tree.filter (fun rhs -> filter_weak (Sk sk) rhs) skd.sk_wsucc;

      unify_wcc skpi.skpi_wcc skpi.skpi_wpred;
      delta_cc skpi.skpi_wcc skpi.skpi_wpred skpi.skpi_wsucc;

      Tree.iter (function nd ->

	let ndd = !! nd in
	let ndpi = nd_info nd in

	match 
	  ndpi.ndpi_noghost, ndpi.ndpi_occurences,

	  ndpi.ndpi_negative, 
	  Tree.is_empty ndd.nd_ssucc, Tree.is_empty ndd.nd_wsucc,

	  ndpi.ndpi_positive, 
	  Tree.is_empty ndd.nd_spred, Tree.is_empty ndd.nd_wpred
	with
	  false, _, true, true, true, false, true, true ->
	    if not (Ub.is_top ndd.nd_ub) then
	      ndpi.ndpi_output_mode <- OmUb ndd.nd_ub
	| false, _, false, true, true, true, true, true ->
	    if not (Lb.is_bottom ndd.nd_lb) then
	      ndpi.ndpi_output_mode <- OmLb ndd.nd_lb
	| false, 1, true, true, true, true, true, true ->
	    if not (Lb.is_bottom ndd.nd_lb && Ub.is_top ndd.nd_ub)
	    then ndpi.ndpi_output_mode <- OmLub (ndd.nd_lb, ndd.nd_ub)
	| _ ->
	    ndpi.ndpi_wpred <-
	      Tree.filter (fun lhs -> filter_weak lhs (Nd nd)) ndd.nd_wpred;
	    ndpi.ndpi_wsucc <-
	      Tree.filter (fun rhs -> filter_weak (Nd nd) rhs) ndd.nd_wsucc;
	    unify_scc ndpi.ndpi_scc ndd.nd_spred;
	    delta_cc ndpi.ndpi_scc ndd.nd_spred ndd.nd_ssucc;
	    unify_wcc ndpi.ndpi_wcc ndpi.ndpi_wpred;
	    delta_cc ndpi.ndpi_wcc ndpi.ndpi_wpred ndpi.ndpi_wsucc;
	    ()

      ) skd.sk_nodes

    ) csd.cs_skeletons



  (*-----------------------------------------------------------------------*)
  (** {4 Third step: assigning names} 

      Variables are pretty-printed using litteral names ('a, 'b, etc.).
      This third steps assigns such names to variables.  Simultaneously,
      it updates the fields [ndpi_output_mode] of nodes, according to
      the following principles:
      - every type term must be printed, so its output mode is set to
        [OmTerm],
      - every row term whose both sons are printed ghostly may be printed
        ghostly too, so its output mode is set to [OmGhost].
   *)

  (** Distinct litteral names are generated using a global index stored in 
      the reference [names_index].
   *)
  let names_index =
    ref (-1)

  (** Every invocation of [fresh_name ()] returns a distinct integer 
      representing a different litteral name.
   *)
  let fresh_name () =
    incr names_index;
    ! names_index

  (** [reset_names ()] resets the name space by reseting the reference
      [names_index].
   *)
  let reset_names () =
    names_index := -1

  (** [name_of_int i] translates an integer into a string denoting a
      litteral variable name.
   *)
  let name_of_int i =
    if i < 26
    then String.make 1 (Char.chr (i+97))
    else String.make 1 (Char.chr ((i mod 26) + 97)) ^ string_of_int (i/26)



  (** [nd_name nd] assigns names to the node [nd] and recursively it sub-nodes
      as explained above.
   *)
  let nd_name nd =

    let rec nd_name_rec nd =

      let ndd = !! nd in
      let ndpi = nd_info nd in

      match ndd.nd_descriptor with

	Type t ->
	  Type.iter (fun _ nd' -> ignore (nd_name_rec nd')) t;
	  ndpi.ndpi_output_mode <- OmTerm;
	  false

      | Row (_, nd', nd'') ->
	  let g' = nd_name_rec nd' in
	  let g'' = nd_name_rec nd'' in
	  if not (g' && g'') then ndpi.ndpi_output_mode <- OmTerm;
	  g' && g''

      |	Variable ->
	  match ndpi.ndpi_output_mode with
	    OmGhostVariable ->
	      if 
		!all_variables
		  or (not (nd_atomic nd))
		  or (ndpi.ndpi_negative 
			&& ndpi.ndpi_positive
			&& ndpi.ndpi_occurences > 1)
		  or ndpi.ndpi_noghost
		  or not (Tree.is_empty ndd.nd_spred) 
		  or not (Tree.is_empty ndd.nd_ssucc)
		  or not (Tree.is_empty ndpi.ndpi_wpred)
		  or not (Tree.is_empty ndpi.ndpi_wsucc)
	      then begin
		ndpi.ndpi_output_mode <- OmVariable;
		false
	      end
	      else true
	  | _ -> false

    in

    ignore (nd_name_rec nd)



  (*-----------------------------------------------------------------------*)
  (** {4 Customizing printing}  

      Pretty-prints may be customized in several way in applying the 
      following functions on nodes and skeletons before the preliminray
      computations.
   *)

  (** [sk_tag s sk] sets the string [s] as tag for the skeleton [sk].
   *)
  let sk_tag s sk =

    let skpi = sk_info sk in
    skpi.skpi_tag <- Some s



  (** [nd_tag s nd] sets the string [s] as tag for the node [nd].
   *)
  let nd_tag s nd =

    let ndpi = nd_info nd in
    ndpi.ndpi_tag <- Some s



  (** [nd_noghost nd] disallows ghosting node [nd].
   *)
  let nd_noghost nd =

    let ndpi = nd_info nd in
    ndpi.ndpi_noghost <- true



  (** [ns_noghost ns] disallows ghosting node or skeleton [ns].
   *)
  let ns_noghost = function

    Nd nd -> nd_noghost nd
  | Sk sk -> ()
   


  (*-----------------------------------------------------------------------*)
  (** {3 Nodes descriptors} *)

  (** [nd_parenthesize position sk] returns [true] if the node [nd] must
      be parenthesized in position [position]
   *)
  let nd_parenthesize position nd =

    match (!! nd).nd_descriptor with

      Variable | Row _ -> false

    | Type t -> Type.parenthesize position t



  (** [nd_ghost nd] returns a boolean indicating wether the node [nd] may
      be printed ghostly.
   *)
  let nd_ghost nd =
    match (nd_info nd).ndpi_output_mode with
      OmGhostVariable -> true
    | _ -> false



  (** [node ppf nd] pretty prints the term represented by the node [nd],
      using the current transient pieces of information.
   *)
  let rec node opt_variance ppf nd =

    let ndd = !! nd in
    let ndpi = nd_info nd in

    let aux ppf =

      match ndpi.ndpi_output_mode with

	OmGhostVariable when !ghost_variables -> 
	  fprintf ppf "%s" Print.ghost

      |	OmGhostVariable | OmVariable ->
	  let i =
	    match ndpi.ndpi_name with
	      None -> 
		let i = fresh_name () in
		ndpi.ndpi_name <- Some i;
		i
	    | Some i -> i
	  in
	  let symbol, tag =
	    match opt_variance with
	      None -> "", ""
	    | Some Covariant -> "+", "+"
	    | Some Contravariant -> "-", "-"
	    | Some Invariant -> "=", "="
	  in
	  fprintf ppf "@{<%s>'%s%s@}" 
	    tag
	    (name_of_int i)
	    (if !polarities then symbol else "")

      |	OmLb lb ->
	  Lb.fprint_in_term (Kind.rows (nd_kind nd)) ppf lb

      |	OmUb ub ->
	  Ub.fprint_in_term (Kind.rows (nd_kind nd)) ppf ub

      |	OmLub (lb, ub) ->
	  Lub.fprint_in_term (Kind.rows (nd_kind nd)) ppf lb ub

      |	OmTerm ->
	  match ndd.nd_descriptor with
	    Variable -> assert false
	  | Type t ->
	      Type.fprint ppf nd_ghost (fun arg position ppf nd' -> 
		let variance' =
		  match opt_variance with
		    None -> None
		  | Some variance -> 
		      Some (Variance.combine variance arg.variance)
		in
		if nd_parenthesize position nd'
		then fprintf ppf "@[<1>(%a)@]"  (node variance') nd'
		else fprintf ppf "%a"  (node variance') nd'
              ) t
	  | Row (lbl, nd_lbl, nd') ->
	      if nd_ghost nd' then
		fprintf ppf "@[%a: %a@]" 
		  Label.fprint lbl  
		  (node opt_variance) nd_lbl
	      else
		fprintf ppf "@[%a: %a@];@ %a" 
		  Label.fprint lbl  
		  (node opt_variance) nd_lbl
		  (node opt_variance) nd'

    in

    match ndpi.ndpi_tag with
      None -> fprintf ppf "%t" aux
    | Some tag -> fprintf ppf "@{<%s>%t@}" tag aux



  let skeleton_as_node ppf sk =
    let skd = !! sk in
    assert (not (Tree.is_empty skd.sk_nodes));
    node None ppf (Tree.choose skd.sk_nodes)




  (*-----------------------------------------------------------------------*)
  (** {3 Constraints} *)

  (** [same_skel ppf list] pretty-prints the "same-skeleton" constraint
      represented by the list of nodes [list].
   *)
  let same_skel ppf list =
    Print.same_skel (node None) ppf list



  (** [left_hand_side ppf lhs] pretty print the left-hand-side [lhs] of
      a weak inequality. 
   *)
  let left_hand_side ppf = function
      Nd nd -> 
	begin match nd_kind nd with
	  Katom -> fprintf ppf "%a" (node (Some Contravariant)) nd
	| Ktype | Krow _ -> 
	    Print.left_destructor (node (Some Contravariant)) ppf nd
	end
    | Sk sk ->
	Print.left_destructor_skel skeleton_as_node ppf sk



  (** [right_hand_side ppf lhs] pretty print the right-hand-side [rhs] of
      a weak inequality. 
   *)
  let right_hand_side ppf = function
      Nd nd -> 
	begin match nd_kind nd with
	  Katom -> fprintf ppf "%a" (node (Some Covariant)) nd
	| Ktype | Krow _ -> Print.right_destructor (node (Some Covariant)) ppf nd
	end
    | Sk sk ->
	Print.right_destructor_skel skeleton_as_node ppf sk



  (** Sorting functions for hand-sides. *)
  let nd_get_name nd =
    match (nd_info nd).ndpi_name with
      None -> - (!! nd).nd_stamp
    | Some i -> i
  let ns_get_name = function
      Nd nd -> nd_get_name nd
    | Sk sk -> 
	assert (not (Tree.is_empty (!! sk).sk_nodes));
	nd_get_name (Tree.choose (!! sk).sk_nodes)

  let ns_sort list =
    List.sort (fun ns1 ns2 -> compare (ns_get_name ns1) (ns_get_name ns2)) list
  let nd_sort list =
    List.sort (fun nd1 nd2 -> compare (nd_get_name nd1) (nd_get_name nd2)) list



  (** [weak_leq ppf (lhs, rhs)] pretty-prints the weak inequalities
      represented by the two lists of hand-sides [lhs] and [rhs].
   *)
  let weak_leq ppf (lhs, rhs) =
    Print.leq (Print.lhs left_hand_side) (Print.rhs right_hand_side) ppf 
      (ns_sort lhs) (ns_sort rhs)

  let lower_bound ppf (lb, rhs) =
    Print.leq Lb.fprint (Print.rhs right_hand_side) ppf lb (ns_sort rhs)

  let upper_bound ppf (lhs, ub) =
    Print.leq (Print.lhs left_hand_side) Ub.fprint ppf (ns_sort lhs) ub

  (** [strong_leq ppf (left, right)] pretty-prints the strong
      inequalities represented by the two lists of nodes [lhs] and [rhs].
   *)
  let strong_leq ppf (lhs, rhs) =
    Print.leq 
      (Print.lhs (node (Some Contravariant))) 
      (Print.rhs (node (Some Covariant))) ppf (nd_sort lhs) (nd_sort rhs)



  (** [sk_same_skel printer ppf sk] prints the same-skeleton constraint
      carried by the skeleton [sk]. 
   *)
  let sk_same_skel ppf sk =

    if not (sk_atomic sk) && sk_terminal sk then begin

      match
	Tree.to_list (Tree.filter (function nd ->
	  Proxy.principal (nd_info nd).ndpi_scc
        ) (!! sk.sk_nodes))
      with
	[] | [_] -> ()
      |	list -> Print.cset_item same_skel ppf list

    end



  (** [sk_inequalities printer ppf sk] prints the inequalities carried by
      the skeleton [sk].
   *)
  let sk_inequalities ppf sk =

    let skd = !! sk in
    let skpi = sk_info sk in

    if ! (!! (skpi.skpi_wcc)) < 0 then begin
      if not (Tree.is_empty skpi.skpi_wpred)
      then Print.cset_item weak_leq ppf (Tree.to_list skpi.skpi_wpred, [Sk sk])
    end
    else begin
      if not (Tree.is_empty skpi.skpi_wsucc)
      then Print.cset_item weak_leq ppf ([Sk sk], Tree.to_list skpi.skpi_wsucc)
    end;

    if not (Lb.is_bottom skd.sk_lb) then
      Print.cset_item lower_bound ppf (skd.sk_lb, [Sk sk]);
    if not (Ub.is_top skd.sk_ub) then
      Print.cset_item upper_bound ppf ([Sk sk], skd.sk_ub)




  (** [nd_inequalities ppf nd] prints the inequalities carried by the
      node [nd].
   *)
  let nd_inequalities ppf nd =

    let ndd = !! nd in
    let ndpi = nd_info nd in

    match ndpi.ndpi_output_mode with

      OmGhostVariable | OmTerm | OmLb _ | OmUb _ | OmLub _ -> ()

    | OmVariable ->

	if ! (!! (ndpi.ndpi_scc)) < 0 then begin
	  if not (Tree.is_empty ndd.nd_spred)
	  then Print.cset_item strong_leq ppf (Tree.to_list ndd.nd_spred, [nd])
	end
	else begin
	  if not (Tree.is_empty ndd.nd_ssucc)
	  then Print.cset_item strong_leq ppf ([nd], Tree.to_list ndd.nd_ssucc);
	end;

	if ! (!! (ndpi.ndpi_wcc)) < 0 then begin
	  if not (Tree.is_empty ndpi.ndpi_wpred)
	  then Print.cset_item weak_leq ppf 
	      (Tree.to_list ndpi.ndpi_wpred, [Nd nd])
	end
	else begin
	  if not (Tree.is_empty ndpi.ndpi_wsucc)
	  then Print.cset_item weak_leq ppf
	      ([Nd nd], Tree.to_list ndpi.ndpi_wsucc)
	end;

	if not (Lb.is_bottom ndd.nd_lb) then
	  Print.cset_item lower_bound ppf (ndd.nd_lb, [Nd nd]);
	if not (Ub.is_top ndd.nd_ub) then
	  Print.cset_item upper_bound ppf ([Nd nd], ndd.nd_ub)



  (** [cset ppf cs] prints the constraints of [cs].
   *)
  let cset ppf cs =

    Print.cset_begin ppf;

    Tree.iter (function sk ->
      sk_same_skel ppf sk;
      sk_inequalities ppf sk;
      Tree.iter (nd_inequalities ppf) (!! sk.sk_nodes)
    ) (!! cs.cs_skeletons);

    Print.cset_end ppf




  (*-----------------------------------------------------------------------*)
  (** {3 Skeleton descriptors} *)

  (** [sk_ghost sk] returns a boolean indicating wether the skeleton [sk]
      may be printed ghostly.
   *)
  let sk_ghost sk =
    sk_atomic sk



  (** [sk_parenthesize position sk] returns [true] if the skeleton [sk] must
      be parenthesized in position [position]
   *)
  let sk_parenthesize position sk =
    match (!! sk).sk_descriptor with
      Variable | Row _ -> false
    | Type t -> Type.parenthesize position t



  (** [sk_name sk] returns the integer given the name for pretty-printing
      the skeleton variable [sk].  If the skeleton has no name, one is
      generated on the fly.
   *)
  let sk_name sk =
    let skpi = sk_info sk in
    match skpi.skpi_name with
      None ->
	let i = fresh_name () in
	skpi.skpi_name <- Some i;
	i
    | Some i ->
	i



  (** [skeleton ppf sk] print the descriptor of the skeleton [sk] using
      the formatter [ppf].
   *)
  let rec skeleton ppf sk =

    let skd = !! sk in

    let aux ppf =

      match skd.sk_descriptor with

	Variable ->
	  if sk_ghost sk then fprintf ppf "%s" Print.ghost
	  else fprintf ppf "~%s"  (name_of_int (sk_name sk))

      | Type t ->
	  Type.fprint ppf sk_ghost (fun _ position ppf sk' -> 
	    if sk_parenthesize position sk'
	    then fprintf ppf "@[<1>(%a)@]"  skeleton sk'
	    else fprintf ppf "%a"  skeleton sk'
	  ) t

      | Row (lbl, sk_lbl, sk') ->
	  fprintf ppf "@[%a: %a@];@;<1 -2>%a" 
	    Label.fprint lbl  skeleton sk_lbl  skeleton sk'
	      
    in

    match (sk_info sk).skpi_tag with
      None -> aux ppf
    | Some tag -> fprintf ppf "@{<%s>%t@}" tag aux

end



(***************************************************************************)
(** {2 Drawing} *)

module Drawing = struct

  (*-----------------------------------------------------------------------*)
  (** {3 Drawing parameters} *)

  type drawing_parameters =
      { mutable atom_size: int;
	mutable atom_sep: int;
	mutable box_margin: int;
	mutable box_sep: int;

	mutable rank_sep: int;
	strong_color: color;
	weak_color: color;
	mutable bullet_size: int;
	mutable arrow_width: int;
	mutable arrow_height: int;

	covariant_color: color;
	contravariant_color: color;
	invariant_color: color;

	text_color: color
      }	

  let param =
    { atom_size = 10;
      atom_sep = 1;
      box_margin = 3;
      box_sep = 1;

      rank_sep = 12;
      strong_color = Avl_graphics.black;
      weak_color = Avl_graphics.rgb 0 0 128;
      bullet_size = 6;
      arrow_width = 5;
      arrow_height = 8;

      covariant_color = Avl_graphics.rgb 250 130 98;
      contravariant_color = Avl_graphics.rgb 123 186 55;
      invariant_color = Avl_graphics.rgb 205 155 29;

      text_color = Avl_graphics.black
    } 


  let update_param win =
    let w, h = Draw.text_size win "X" in
    param.atom_size <- w;
    param.box_margin <- h / 4;
    param.rank_sep <- h + 4;
    param.bullet_size <- w;
    param.arrow_width <- w;
    param.arrow_height <- w


  (*-----------------------------------------------------------------------*)
  (** {3 Drawing primitives} *)

  type inequality_kind =
      Strong
    | Weak

  let ineq_color = function
      Strong -> param.strong_color
    | Weak -> param.strong_color

  let variance_color = function
      Covariant -> param.covariant_color
    | Contravariant -> param.contravariant_color
    | Invariant -> param.invariant_color

  let draw_atom window variance x y =
    let r = param.atom_size / 2 in
    Draw.fill_ellipse window
      ~color:(variance_color variance) ~x ~y ~rx:r ~ry:r

  let draw_bullet window ineq x y =
    let r = param.bullet_size / 2 in
    Draw.fill_ellipse window
      ~color:(ineq_color ineq) ~x ~y ~rx:r ~ry:r

  let draw_left_arrow window ineq x y =
    Draw.fill_poly window ~color:(ineq_color ineq)
      [x, y; 
       x-param.arrow_width, y-param.arrow_height/2;
       x-param.arrow_width, y+param.arrow_height/2]

  let draw_right_arrow window ineq x y =
    Draw.fill_poly window ~color:(ineq_color ineq)
      [x, y; 
       x+param.arrow_width, y-param.arrow_height/2;
       x+param.arrow_width, y+param.arrow_height/2]



  (*-----------------------------------------------------------------------*)
  (** {3 Preliminrary treatments} *)

  let nd_info = Printing.nd_info
  let sk_info = Printing.sk_info



  (** Distinct litteral names are generated using a global index stored in 
      the reference [names_index].
   *)
  let names_index =
    ref (-1)



  (** Every invocation of [fresh_name ()] returns a distinct integer 
      representing a different litteral name.
   *)
  let fresh_name () =
    incr names_index;
    ! names_index



  (** [nd_skeleton_name nd] returns the litteral name of the skeleton of
      node [nd].
   *)
  let nd_skeleton_name nd =

    let sk = nd_skeleton nd in
    let skpi = sk_info sk in

    match skpi.skpi_name with
      None ->
	let i = fresh_name () in
	skpi.skpi_name <- Some i;
	Printing.name_of_int i
    | Some i ->
	Printing.name_of_int i



  (*-----------------------------------------------------------------------*)
  (** {4 First step: registering occurences} 

      This firsts steps consists in walking throught the term(s) and update
      some information in printed nodes:
      - number of occurences,
      - polarities of the occurences.
   *)

  let nd_occurence = Printing.nd_occurence



  (*-----------------------------------------------------------------------*)
  (** {4 Second step} *) 

  let nd_name nd =

    let rec nd_name_rec nd =

      let ndd = !! nd in
      let ndpi = nd_info nd in

      match ndd.nd_descriptor with

	Type t ->
	  Type.iter (fun _ nd' -> ignore (nd_name_rec nd')) t;
	  ndpi.ndpi_output_mode <- OmTerm;
	  false

      | Row (_, nd', nd'') ->
	  let g' = nd_name_rec nd' in
	  let g'' = nd_name_rec nd'' in
	  if not (g' && g'') then ndpi.ndpi_output_mode <- OmTerm;
	  g' && g''

      |	Variable ->
	  match ndpi.ndpi_output_mode with
	    OmGhostVariable ->
	      if 
		!Printing.all_variables
		  or not (nd_atomic nd)
		  or (ndpi.ndpi_negative 
			&& ndpi.ndpi_positive
			&& ndpi.ndpi_occurences > 1)
		  or ndpi.ndpi_noghost
		  or not (Tree.is_empty ndd.nd_spred)
		  or not (Tree.is_empty ndd.nd_ssucc)
		  or not (Tree.is_empty ndd.nd_wpred)
		  or not (Tree.is_empty ndd.nd_wsucc)
		  or not (Lb.is_bottom ndd.nd_lb)
		  or not (Ub.is_top ndd.nd_ub)
	      then begin
		ndpi.ndpi_output_mode <- OmVariable;
		false
	      end
	      else true
	  | _ -> false

    in

    ignore (nd_name_rec nd)



  (*-----------------------------------------------------------------------*)
  (** {3 Formatters} *)

  module DF = struct

    type t = 
	{ ppf: formatter;
	  mutable x: int;
	  mutable y: int;
	  tags: (int, variance * node) Hashtbl.t;
	  mutable tag_index: int
	} 

    let create ~window ~x ~y =
      let buffer = Buffer.create 7 in
      let out s pos len =
	Buffer.add_substring buffer s pos len
      in
      let ppf = make_formatter out (fun () -> ()) in
      let df =
	{ ppf = ppf;
	  x = x;
	  y = y;
	  tags = Hashtbl.create 7;
	  tag_index = 0
	} 
      in

      let flush () =
	let s = Buffer.contents buffer in
	Buffer.clear buffer;
	let w, _ = Draw.text_size window s in
        Draw.draw_text window ~color:param.text_color ~x:df.x ~y:df.y s;
	df.x <- df.x + w
      in

      let open_tag tag =
	let i = int_of_string tag in
	let variance, nd = Hashtbl.find df.tags i in
	
	flush ();

	let x =
	  if nd_atomic nd then begin
	    let x = df.x + param.atom_size/2 + param.atom_sep in
	    draw_atom window variance 
	      x (df.y + param.atom_size/2 + param.rank_sep / 5);
	    df.x <- df.x + param.atom_size + 2 * param.atom_sep;
	    x
	  end
	  else begin
	    let s = "~" ^ (nd_skeleton_name nd) in
	    let w, h = Draw.text_size window s in
	    df.x <- df.x + param.box_sep;
	    Draw.fill_rect window ~color:(variance_color variance)
	      ~x:df.x ~y:(df.y - param.box_margin) 
	      ~w:(w + 2 * param.box_margin) ~h:(h + 2 * param.box_margin);
	    Draw.draw_text window ~color:Avl_graphics.white
	      ~x:(df.x + param.box_margin) ~y:df.y s;
	    df.x <- df.x + w + 2 * param.box_margin + param.box_sep;
	    df.x - w/2 - param.box_margin - param.box_sep
	  end
	in

	let ndpi = nd_info nd in
	ndpi.ndpi_positions <- (variance, x) :: ndpi.ndpi_positions;
	
	""
      in

      pp_set_formatter_output_functions ppf out flush;
      pp_set_margin ppf 999999998;
      pp_set_formatter_tag_functions ppf
	{ mark_open_tag = open_tag;
	  mark_close_tag = (function _ -> "");
	  print_open_tag = (function _ -> ());
	  print_close_tag = (function _ -> ()) };
      pp_set_tags ppf true;

      df



    let tag_register df variance nd =
      df.tag_index <- df.tag_index + 1;
      Hashtbl.add df.tags df.tag_index (variance, nd);
      string_of_int df.tag_index

  end


  (*-----------------------------------------------------------------------*)
  (** {3 Printing the skeleton of the type} *)

  let nd_ghost nd =

    match (nd_info nd).ndpi_output_mode with
      OmGhostVariable -> true
    | _ -> false



  let skeleton df variance ppf nd =

    let rec node_rec variance ppf nd =

      let ndd = !! nd in

      match ndd.nd_descriptor with
	Variable -> 
	  let tag = DF.tag_register df variance nd in
	  fprintf ppf "@{<%s>@}" tag
	  
      | Type t ->
	  Type.fprint ppf nd_ghost (fun arg position ppf nd' -> 
	    if Printing.nd_parenthesize position nd'
	    then
	      fprintf ppf "@[<1>(%a)@]"  
		(node_rec (Variance.combine variance arg.variance)) nd'
	    else
	      fprintf ppf "%a"  
		(node_rec (Variance.combine variance arg.variance)) nd'
          ) t

      | Row (lbl, nd_lbl, nd') ->
	  if nd_ghost nd' then
	    fprintf ppf "@[%a: %a@]" Label.fprint lbl  (node_rec variance) nd_lbl
	  else
	    fprintf ppf "@[%a: %a@];@;<1 -2>%a" 
	      Label.fprint lbl
	      (node_rec variance) nd_lbl  (node_rec variance) nd'

    in

    node_rec variance df.DF.ppf nd



  (*-----------------------------------------------------------------------*)
  (** {3 Constraints} *)

  let constraints window x y x' cs =

    let sort = List.sort Pervasives.compare in
    let merge = List.merge Pervasives.compare in

    (* Recherche des contraintes *)

    let bounds_tbl = Hashtbl.create 7 in
    let accu = ref [] in

    let add flag lhs rhs =
      let lhs' = 
	Standard.filter_map
	  (function Covariant, _ -> None | _,x -> Some x) lhs
      and rhs' =
	Standard.filter_map 
	  (function Contravariant, _ -> None | _, x -> Some x) rhs
      in
      if lhs' <> [] && rhs' <> [] && not (List.length lhs' = 1 && lhs' = rhs')
      then accu := (flag, sort lhs', sort rhs') :: ! accu
    in

    let csd = !! cs in

    Tree.iter (function sk ->
      let skd = !! sk in
      Tree.iter (function nd ->
	let ndd = !! nd in
	let ndpi = nd_info nd in

	(* Strong inequalities *)
	let ssucc =
	  List.flatten (List.map (fun nd' -> (nd_info nd').ndpi_positions) 
			  (Tree.to_list ndd.nd_ssucc))
	in
	add Strong ndpi.ndpi_positions ssucc;

	(* Weak inequalities *)
	let wsucc =
	  List.flatten (List.map (function
	      Nd nd' -> (nd_info nd').ndpi_positions
	    | Sk sk' -> []
		 ) (Tree.to_list ndd.nd_wsucc))
	in
	add Weak ndpi.ndpi_positions wsucc;

	(* Constant bounds *) 
	let lb_occurences =
	  Standard.filter_map 
	    (function Contravariant, _ -> None | _, x -> Some x) 
	    ndpi.ndpi_positions
	and ub_occurences =
	  Standard.filter_map 
	    (function Covariant, _ -> None | _, x -> Some x) 
	    ndpi.ndpi_positions
	in

	if lb_occurences <> [] then
	  List.iter (function lbs ->
	    let lhs, rhs =
	      try Hashtbl.find bounds_tbl lbs with Not_found -> [], [] 
	    in
	    Hashtbl.replace bounds_tbl lbs
	      (lhs, List.rev_append lb_occurences rhs)
          ) (Lb.draw ndd.nd_lb);
	if ub_occurences <> [] then
	  List.iter (function lbs ->
	    let lhs, rhs = 
	      try Hashtbl.find bounds_tbl lbs with Not_found -> [], [] 
	    in
	    Hashtbl.replace bounds_tbl lbs
	      (List.rev_append ub_occurences lhs, rhs)
          ) (Ub.draw ndd.nd_ub);

	(* Occurences *)
	add (if nd_kind nd = Katom then Weak else Strong)
	  ndpi.ndpi_positions ndpi.ndpi_positions

      ) skd.sk_nodes

    ) csd.cs_skeletons;

    (* Fusion des traits *)

    let cmp (f, _, rhs) (f', _, rhs') =
      let c = Pervasives.compare f f' in
      if c <> 0 then c 
      else Standard.compare_list
	  (Pervasives.compare : int -> int -> int) rhs rhs'
    in
    let rec aux = function
	([] | [_]) as list -> list
      | ((f, lhs, rhs) as hd) :: ((_, lhs', _) as hd') :: tl 
	when cmp hd hd' = 0 ->
	  aux ((f, merge lhs lhs', rhs) :: tl)
      | ((_, _, rhs) as hd) :: tl -> 
	  hd :: aux tl
    in
    let accu' = aux (List.sort cmp !accu) in

    (* Trace des traits verticaux *)

    let heights = Hashtbl.create 7 in
    let y' = ref (y + param.rank_sep) in

    List.iter (function ineq, lhs, rhs ->
      y' := !y' + param.rank_sep;
      List.iter (function x -> Hashtbl.replace heights x !y') lhs;
      List.iter (function x -> Hashtbl.replace heights x !y') rhs
    ) accu';

    Hashtbl.iter (fun lbs (lhs, rhs) ->
      y' := !y' + param.rank_sep;
      List.iter (function x -> Hashtbl.replace heights x !y') lhs;
      List.iter (function x -> Hashtbl.replace heights x !y') rhs
    ) bounds_tbl;

    let box_height = snd (Draw.text_size window "X") + 2 * param.box_margin in
    let y0 = y + box_height in

    Tree.iter (function sk ->
      let skd = !! sk in
      Tree.iter (function nd ->
	let ndpi = nd_info nd in
	List.iter (function variance, x ->
	  try
	    Draw.draw_vertical_dots window ~color:(variance_color variance)
	      ~x:x ~y:y0 ~y':(Hashtbl.find heights x)
	  with 
	    Not_found -> ()
        ) ndpi.ndpi_positions
      ) skd.sk_nodes
    ) csd.cs_skeletons;

    (* Trace des traits horizontaux *)

    let y' = ref (y + param.rank_sep) in

    let rec last = function
	[] -> assert false
      | [x] -> x
      | _ :: tl -> last tl
    in

    List.iter (function ineq, lhs, rhs ->
      y' := !y' + param.rank_sep;
      let lhs_origin = List.hd lhs
      and lhs_tail = last lhs in
      let origin = min lhs_origin (List.hd rhs) in
      let tail = max lhs_tail (last rhs) in
      begin match ineq with
	Strong ->
	  Draw.draw_lines window ~color:(ineq_color ineq) ~lw:0
	    [origin, !y'; tail, !y'];
      | Weak ->
	  Draw.draw_horizontal_dots window ~color:(ineq_color ineq)
	    ~x:origin ~x':tail ~y:!y'
      end;
      List.iter (function x -> 
	if rhs <> [x] then draw_bullet window ineq x !y'
      ) lhs;
      List.iter (function x ->
	if x < lhs_tail then 
	  draw_right_arrow window ineq 
	    (if List.mem x lhs && rhs <> [x] then x + 3 else x) !y';
	if x > lhs_origin then
	  draw_left_arrow window ineq
	    (if List.mem x lhs && rhs <> [x] then x - 3 else x) !y'
      ) rhs
    ) accu';

    let x_max = ref x' in

    Hashtbl.iter (fun bound (lhs, rhs) ->
      y' := !y' + param.rank_sep;     
      let w, h = Draw.text_size window bound in
      x_max := max !x_max (x' + w + 5);

      Draw.draw_text window ~color:param.text_color 
	~x:(x' + 5) ~y:(!y' - h/2) bound;

      let x0 = min (List.fold_left min x' lhs) (List.fold_left min x' rhs) in

      Draw.draw_horizontal_dots window ~color:(ineq_color Weak)
        ~x:x0 ~x':x' ~y:!y';

      List.iter (fun x'' -> draw_left_arrow window Weak x'' !y') lhs;
      List.iter (fun x'' -> draw_right_arrow window Weak x'' !y') rhs

    ) bounds_tbl;

    !x_max, !y'



  (** [reset_names ()] resets the name space by reseting the reference
      [names_index].
   *)
  let reset_names () =
    names_index := -1

      

end



(***************************************************************************)
(** {2 Unification}

    This section implements unification of nodes and skeletons.  The main
    function are [nd_unify] (which allows unifying two nodes) and [sk_unify]
    (which allows unifying two skeletons).

    Note: Functions of this module internally uses the transient field 
    [nd_mark_unification] of nodes and [sk_mark_unification] of skeletons.
 *)

module Unification = struct

  exception Incompatible of skeleton * skeleton
  exception Cycle of skeleton * skeleton

  exception Expand_rigid_skeleton of skeleton * skeleton
  exception Unify_rigid_skeleton of skeleton * skeleton
  exception Unify_rigid_node of node * node



  (** [expand_manifest] is a reference on the function which allows
      expanding types according to manifest equations.  The reference
      is initialized with the function correponding to the absence of
      any equation.
   *)
  let expand_manifest : (skeleton Type.t -> (node * node) option) ref =
    ref (fun _ -> None)



  (*-----------------------------------------------------------------------*)
  (** {3 Iterators}
      Throughout this module, we call iterators on nodes, skeletons and
      "skeletons or nodes" provided by the following modules.  Because they
      use the same internal mark in nodes and skeletons, they must not be 
      called recursively.
   *)

  module NdTree = Tree.Marked (struct
    type t = node
    let mark = nd_mark NdMask.unification
    let unmark = nd_unset NdMask.unification
  end)

  module SkTree = Tree.Marked (struct
    type t = skeleton
    let mark = sk_mark SkMask.unification
    let unmark = sk_unset SkMask.unification
  end)

  module NSTree = Tree.Marked (struct
    type t = node_or_skeleton
    let mark = ns_mark NdMask.unification SkMask.unification
    let unmark = ns_unset NdMask.unification SkMask.unification
  end)



  (*-----------------------------------------------------------------------*)
  (** {3 Marking skeletons and nodes as rigid} *)

  (** [sk_rigid sk] marks the skeleton [nd and (recursively) all its sons
      as rigid. 
   *)
  let rec sk_rigidify sk =

    if not (sk_rigid sk) then begin

      let skd = !! sk in

      skd.sk_rigid <- Some { skr_wpred = Tree.empty;
			     skr_wsucc = Tree.empty;
			     skr_lb = Lb.bottom;
			     skr_ub = Ub.top
			   } ;

      Descriptor.iter (fun _ sk -> sk_rigidify sk) skd.sk_descriptor

    end



  (** [nd_rigidify nd] sets the node [nd] (and its skeleton) as rigid.
   *)
  let rec nd_rigidify nd =

    if not (nd_rigid nd) then begin

      let ndd = !! nd in
      sk_rigidify ndd.nd_skeleton;

      ndd.nd_rigid <- Some { ndr_spred = Tree.empty;
			     ndr_ssucc = Tree.empty;
			     ndr_wpred = Tree.empty;
			     ndr_wsucc = Tree.empty;
			     ndr_lb = Lb.bottom;
			     ndr_ub = Ub.top
			   } ;

      Descriptor.iter (fun _ nd -> nd_rigidify nd) ndd.nd_descriptor

    end



  (** {3 Unification of skeletons} *)

  (** [occur_check sk sk0] tests recursively wether the skeleton [sk]
      appears in [sk0] description or not.  If the test is positive, an
      exception is raised.  Otherwise nothing is done.
   *)
  let occur_check sk sk0 =

    let skd = !! sk in

    let rec occur_check_rec sk0' =
      
      let sk0'd = !! sk0' in
      if skd == sk0'd then raise (Cycle (sk, sk0));
      Descriptor.iter (fun _ sk' -> occur_check_rec sk') sk0'd.sk_descriptor
	
    in
    
    occur_check_rec sk0



  (** [sk_unify sk1 sk2] unifies the two skeletons [sk1] and [sk2].  Every
      non-transient field of the skeletons are merged, according to their
      semantics.  As a consequence, sub-skeletons appearing in the
      descriptors are recursively unified.
   *)
  let rec sk_unify sk1 sk2 = 

    sk_scrape sk1 sk2;

    let sk1d = !! sk1
    and sk2d = !! sk2 in

    if sk1d != sk2d then begin

      assert (sk1d.sk_kind = sk2d.sk_kind);
      assert (!! (sk1d.sk_cset) == !! (sk2d.sk_cset));

      (* Recursive unification of sub-skeletons. *)

      begin match sk1d.sk_descriptor, sk2d.sk_descriptor with

	Variable, Variable ->
	  if sk_rigid sk1 && sk_rigid sk2 && not (Kind.atomic sk1d.sk_kind)
	  then raise (Unify_rigid_skeleton (sk1, sk2));
	  occur_check sk2 sk1

      |	Type _, Variable ->
	  if sk_rigid sk2 then raise (Expand_rigid_skeleton (sk2, sk1));
	  occur_check sk2 sk1

      |	Row (_, sk1', sk1''), Variable ->
	  if sk_rigid sk2 then (sk_rigidify sk1'; sk_rigidify sk1'');
	  occur_check sk2 sk1

      |	Variable, Type _ ->
	  if sk_rigid sk1 then raise (Expand_rigid_skeleton (sk1, sk2));
	  occur_check sk1 sk2;
	  sk1d.sk_descriptor <- sk2d.sk_descriptor

      |	Variable, Row (_, sk2', sk2'') ->
	  occur_check sk1 sk2;
	  if sk_rigid sk1 then (sk_rigidify sk2'; sk_rigidify sk2'');
	  sk1d.sk_descriptor <- sk2d.sk_descriptor

      | Type t1, Type t2 ->
	  Type.iter2 (fun _ sk1 sk2 -> sk_unify sk1 sk2) t1 t2

      |	Row (lbl1, sk1_lbl, sk1'), Row (lbl2, sk2_lbl, sk2') ->
	  assert (Label.compare lbl1 lbl2 = 0);
	  sk_unify sk1_lbl sk2_lbl;
	  sk_unify sk1' sk2'

      (* Thanks to kinding, other cases cannot arise. *)

      |	_ -> assert false

      end;

      if skd_test SkMask.decomposed sk2d then skd_set SkMask.decomposed sk1d;
      if skd_test SkMask.polar sk2d then skd_set SkMask.polar sk1d;
      sk1d.sk_nodes <- Tree.union sk2d.sk_nodes sk1d.sk_nodes;

      (* Merging rigid informations. *)

      begin match sk2d.sk_rigid with
	None -> ()
      |	(Some _) as r ->
	  assert (sk1d.sk_rigid = None or true); (* TEMPORARY *)
	  sk1d.sk_rigid <- r
      end;

      (* Merging constraints *)
      sk1d.sk_wpred <- Tree.union sk2d.sk_wpred sk1d.sk_wpred;
      sk1d.sk_wsucc <- Tree.union sk2d.sk_wsucc sk1d.sk_wsucc;
      sk1d.sk_lb <- Lb.union sk1d.sk_lb sk2d.sk_lb;
      sk1d.sk_ub <- Ub.inter sk1d.sk_ub sk2d.sk_ub;


      (* sk2 is now a link pointing on sk1. *)

      Proxy.linksto sk2 sk1

    end



  (** [sk_scrape sk1 sk2] prepares the descriptors of the skeletons 
      [sk1] and [sk2] for unification.  Using equations on type constructors
      and permutations of row labels, this function rewrites the descriptors
      until they have the same head.  If it is not possible, the exception
      [Incompatible_nodes (sk1, sk2)] is raised.
   *)
  and sk_scrape sk1 sk2 =

    let sk1d = !! sk1
    and sk2d = !! sk2 in

    match sk1d.sk_descriptor, sk2d.sk_descriptor with

      Row (lbl1, sk1_lbl1, sk1'), Row (lbl2, sk2_lbl2, sk2') ->
	let c = Label.compare lbl1 lbl2 in
	if c = 0 then ()
	else
	  if c < 0 then sk_rearrange_row sk2 (!! sk2_lbl2.sk_kind) lbl1
	  else sk_rearrange_row sk1 (!! sk1_lbl1.sk_kind) lbl2

    | Type t1, Type t2 ->
	if not (Type.compatible t1 t2) then begin
	  begin 
	    match !expand_manifest t1 with
	      Some exp -> sk_rearrange_type sk1 exp
	    | None ->
		match !expand_manifest t2 with
		  Some exp -> sk_rearrange_type sk2 exp
		| None -> raise (Incompatible (sk1, sk2))
	  end;
	  sk_scrape sk1 sk2
	end

    | _ -> ()



  (** [sk_rearrange_type sk (nd_from0, nd_to0) rewrites the descriptor of
      the skeleton [sk] using the equation [nd_from0 = nd_to0].
   *)
  and sk_rearrange_type sk (nd_from0, nd_to0) =

    let skd = !! sk in
    let cs = skd.sk_cset in
    let sk_from0 = (!! nd_from0).nd_skeleton
    and sk_to0 = (!! nd_to0).nd_skeleton in
    let cs0 = (!! sk_from0).sk_cset in

    merge_cset cs (Copy.make_skeletons cs0);

    let sk_from = Copy.sk_get_copy sk_from0
    and sk_to = Copy.sk_get_copy sk_to0 in
    
    sk_unify sk sk_from;

    skd.sk_nodes <- NdTree.compact skd.sk_nodes;

    Tree.iter (function nd ->

      Copy.make_nodes cs0;
      let nd_from = Copy.nd_get_copy nd_from0
      and nd_to = Copy.nd_get_copy nd_to0 in

      nd_unify nd nd_from;
      (!! nd).nd_descriptor <- (!! nd_to).nd_descriptor

    ) skd.sk_nodes;
    Copy.clean cs0;

    skd.sk_descriptor <- (!! sk_to).sk_descriptor



  (** [sk_rearrange_row sk fields_kind lbl] rewrites the descriptor of the
      skeleton [sk] so that its head label is [lbl].
   *)
  and sk_rearrange_row sk fields_kind lbl =

    let skd = !! sk in

    let sk_lbl = sk_create skd.sk_cset fields_kind Variable
    and sk' = sk_create skd.sk_cset skd.sk_kind Variable
    and sk'' = sk_create skd.sk_cset skd.sk_kind Variable in

    skd.sk_nodes <- NdTree.compact skd.sk_nodes;

    Tree.iter (function nd ->

      let ndd = !! nd in

      match ndd.nd_descriptor with

	Row (lbl0, nd_lbl0, nd0') ->
	  assert (Label.compare lbl lbl0 <> 0);

	  let nd_lbl = nd_create_in_sk sk_lbl Variable
	  and nd' = nd_create_in_sk sk' Variable
	  and nd'' = nd_create_in_sk sk'' Variable in

	  ndd.nd_descriptor <- Row (lbl, nd_lbl, nd');
	  nd_unify nd' 
	    (nd_create skd.sk_cset skd.sk_kind (Row (lbl0, nd_lbl0, nd'')));
	  nd_unify nd0' 
	    (nd_create skd.sk_cset skd.sk_kind (Row (lbl, nd_lbl, nd'')))
	  
      | Variable -> ()

      | Type _ -> assert false

    ) skd.sk_nodes;

    skd.sk_descriptor <- Row (lbl, sk_lbl, sk')



  (** {3 Unification of nodes} *)

  (** [nd_unify nd1 nd2] unifies the two nodes [nd1] and [nd2].  Every
      non-transient field of the nodes are merged, according to their
      semantics.  As a consequence, sub-node appearing in the descriptors 
      are recursively unified.
   *)
  and nd_unify nd1 nd2 =

    let nd1d = !! nd1
    and nd2d = !! nd2 in

    if nd1d != nd2d then begin

      (* Unification of skeletons. *)

      sk_unify nd1d.nd_skeleton nd2d.nd_skeleton;

      (* Recursive unification of sub-nodes. *)

      begin match nd1d.nd_descriptor, nd2d.nd_descriptor with

	Variable, Variable -> 
	  if nd_rigid nd1 && nd_rigid nd2 then
	    raise (Unify_rigid_node (nd1, nd2))

      |	Type _, Variable ->
	  assert (not (nd_rigid nd2))

      |	Row (_, nd1', nd1''), Variable ->
	  if nd_rigid nd2 then (nd_rigidify nd1'; nd_rigidify nd1'')

      | Variable, Type _ -> 
	  assert (not (nd_rigid nd1));
	  nd1d.nd_descriptor <- nd2d.nd_descriptor

      |	Variable, Row (_, nd2', nd2'') ->
	  if nd_rigid nd1 then (nd_rigidify nd2'; nd_rigidify nd2'');
	  nd1d.nd_descriptor <- nd2d.nd_descriptor

      | Type t1, Type t2 ->
	  Type.iter2 (fun _ nd1 nd2 -> nd_unify nd1 nd2) t1 t2

      | Row (lbl1, nd_lbl1, nd1'), Row (lbl2, nd_lbl2, nd2') ->
	    assert (Label.compare lbl1 lbl2 = 0);
	    nd_unify nd_lbl1 nd_lbl2;
	    nd_unify nd1' nd2'

      (* Thanks to kinding, other cases cannot arise. *)

      | _ -> assert false

      end;

      (* Merging flags. *)

      if ndd_test NdMask.decomposed nd2d then ndd_set NdMask.decomposed nd1d;
      if ndd_test NdMask.negative nd2d then ndd_set NdMask.negative nd1d;
      if ndd_test NdMask.positive nd2d then ndd_set NdMask.positive nd1d;

      (* Merging rigid informations. *)

      begin match nd2d.nd_rigid with
	None -> ()
      |	(Some _) as r ->
	  assert (nd1d.nd_rigid = None);
	  nd1d.nd_rigid <- r
      end;

      (* Merging inequalities. *)

      nd1d.nd_spred <- Tree.union nd1d.nd_spred nd2d.nd_spred;
      nd1d.nd_ssucc <- Tree.union nd1d.nd_ssucc nd2d.nd_ssucc;
      nd1d.nd_wpred <- Tree.union nd1d.nd_wpred nd2d.nd_wpred;
      nd1d.nd_wsucc <- Tree.union nd1d.nd_wsucc nd2d.nd_wsucc;

      nd1d.nd_lb <- Lb.union nd1d.nd_lb nd2d.nd_lb;
      nd1d.nd_ub <- Ub.inter nd1d.nd_ub nd2d.nd_ub;

      (* nd2 is now a link pointing on nd1. *)
      
      Proxy.linksto nd2 nd1

    end


  exception Expand_manifest

  let rec sk_expand_manifest cond sk =

    let skd = !! sk in

    match skd.sk_descriptor with

      Variable -> ()

    | Type t ->
	if cond t then begin

	  match !expand_manifest t with
	    Some exp -> 
	      sk_rearrange_type sk exp;
	      sk_expand_manifest cond sk
	  | None -> 
	      raise Expand_manifest
		    
	end
	else Type.iter (fun _ -> sk_expand_manifest cond) t

    | Row (lbl, sk_lbl, sk') ->
	sk_expand_manifest cond sk_lbl;
	sk_expand_manifest cond sk'

end



(***************************************************************************)
(** {2 User level functions for setting constraints} *)

  (*-----------------------------------------------------------------------*)
  (** {3 Reporting errors} *)

  (** An unification failure is described by a tuple of type
      [unification_report].
      A report [(nd1, sk1), (nd2, sk2), flag] must be interpreted as follows:
      - [nd1] and [nd2] are the two nodes that are not unifiable,
      - [sk1] and [sk2] are the two sub-skeletons of the above nodes on
        which unification fails,
      - [flag] is a flag indication the kind of failure.
   *)
  type unification_report = 
    (node * skeleton) * (node * skeleton) * error_flag 

  (** Unification fails when two skeletons cannot be unified.  This may
      happen for two reasons: either they have incompatible descriptors
      or unifying then would create a cyclic (i.e. recursive) term. 
   *)
  and error_flag =
      Flag_incompatible
    | Flag_cycle


   
  (** Unification errors are reported by raising an exception 
      [UnificationError] with the appropriate report as argument. 
   *)
  exception UnificationError of unification_report



  (** [report_unification ppf r] pretty prints an error message on the
      formatter [ppf] describing the unification error reported by [r].
   *)
  let report_unification ppf ((nd1, sk1), (nd2, sk2), flag) =

    Printing.reset_names ();

    Printing.sk_tag "hl1" sk1;
    Printing.sk_tag "hl2" sk2;
    let term1 ppf = Printing.skeleton ppf (!! nd1).nd_skeleton
    and term2 ppf = Printing.skeleton ppf (!! nd2).nd_skeleton
    and explanation ppf =
      match flag with
	Flag_incompatible ->
	  Report.incompatible ppf 
	    ~term1:(fun ppf -> Printing.skeleton ppf sk1)
	    ~term2:(fun ppf -> Printing.skeleton ppf sk2)
      |	Flag_cycle -> 
	  let sk_var, sk_term =
	    if (!! sk1).sk_descriptor = Variable then sk1, sk2 else sk2, sk1
	  in
	  Report.cycle ppf
	    ~variable:(fun ppf -> Printing.skeleton ppf sk_var)
	    ~term:(fun ppf -> Printing.skeleton ppf sk_term)
    in
    Report.unification ppf ~term1 ~term2 ~explanation;
    Printing.cs_reset (!! sk1).sk_cset


  

  (** [equal nd1 nd2] sets the constraint [nd1 = nd2].  This function
      is nothing but a wrapper around [Unification.nd_unify] which 
      builds the appropriate error report if the unification fails.
   *)
  let equal nd1 nd2 =
    try
      Unification.nd_unify nd1 nd2
    with
      Unification.Incompatible (sk1, sk2) ->
	raise (UnificationError ((nd1, sk1), (nd2, sk2), Flag_incompatible))
    | Unification.Cycle (sk1, sk2) ->
	raise (UnificationError ((nd1, sk1), (nd2, sk2), Flag_cycle))
 
     

  (** [same_skel nd1 nd2] sets the constraint [nd1 ~ nd2].  This function
      is nothing but a wrapper around [Unification.sk_unify] which builds
      the appropriate error report if the unification fails.
   *)
  let same_skel nd1 nd2 =
    try
      Unification.sk_unify (!! nd1.nd_skeleton) (!! nd2.nd_skeleton)
    with
      Unification.Incompatible (sk1, sk2) ->
	raise (UnificationError ((nd1, sk1), (nd2, sk2), Flag_incompatible))
    | Unification.Cycle (sk1, sk2) ->
	raise (UnificationError ((nd1, sk1), (nd2, sk2), Flag_cycle))



  (** [unsafe_weak_leq lhs rhs] registers the weak inequality [lhs < rhs].
      The intend of this function is only to be invoked suitably by
      [weak_leq].
   *)
  let unsafe_weak_leq lhs rhs =

    begin match lhs with
      Nd nd ->
	let ndd = !! nd in
	ndd.nd_wsucc <- Tree.add rhs ndd.nd_wsucc
    | Sk sk ->
	let skd = !! sk in
	skd.sk_wsucc <- Tree.add rhs skd.sk_wsucc
    end;

    begin match rhs with
      Nd nd ->
	let ndd = !! nd in
	ndd.nd_wpred <- Tree.add lhs ndd.nd_wpred
    | Sk sk ->
	let skd = !! sk in
	skd.sk_wpred <- Tree.add lhs skd.sk_wpred
    end



  (** [weak_leq lhs rhs] registers the weak inequality [lhs < rhs].  
      In the case where one of the hand-sides is a node, it also sets
      the constraint involving its skeleton.  Several special cases are
      handled:
      - If both hand-sides are the same node of kind [Katom], the
        constraint (i.e. [nd < nd]) is a tautology, so nothing is
        registered.
      - If one of the hand-sides is a skeleton of atomic kind 
        (i.e. [Katom] or [Krow Katom], ...), again the nothing is
        registered.
   *)
  let weak_leq lhs rhs =

    match lhs, rhs with

      Nd nd1, Nd nd2 ->
	let nd1d = !! nd1
	and nd2d = !! nd2 in
	let sk1 = nd1d.nd_skeleton
	and sk2 = nd2d.nd_skeleton in
	let sk1d = !! sk1
	and sk2d = !! sk2 in
	if not (nd1d == nd2d && sk1d.sk_kind = Katom) then begin
	  unsafe_weak_leq lhs rhs;
	  if Type.ldestr_inv && not (Kind.atomic sk1d.sk_kind) then
	    unsafe_weak_leq (Sk sk1) rhs;
	  if Type.rdestr_inv && not (Kind.atomic sk2d.sk_kind) then begin
	    unsafe_weak_leq lhs (Sk sk2);
	    if Type.ldestr_inv && not (Kind.atomic sk1d.sk_kind) then
	      unsafe_weak_leq (Sk sk1) (Sk sk2)
	  end
	end

    | Nd nd1, Sk sk2 ->
	if not (sk_atomic sk2) then begin
	  unsafe_weak_leq lhs rhs;
	  if Type.ldestr_inv && not (nd_atomic nd1) then
	    unsafe_weak_leq (Sk (nd_skeleton nd1)) rhs
	end

    | Sk sk1, Nd nd2 ->
	if not (sk_atomic sk1) then begin
	  unsafe_weak_leq lhs rhs;
	  if Type.rdestr_inv && not (nd_atomic nd2) then
	    unsafe_weak_leq lhs (Sk (nd_skeleton nd2))
	end

    | Sk sk1, Sk sk2 ->
	if Type.ldestr_inv && Type.rdestr_inv
	    && not (sk_atomic sk1) && not (sk_atomic sk2) then
	  unsafe_weak_leq lhs rhs



  (** [lower_bound lb ns] registers the (weak) inequality [lb < ns].
   *)
  let rec lower_bound lb ns =
    match ns with
      Nd nd ->
	let ndd = !! nd in
	ndd.nd_lb <- Lb.union ndd.nd_lb lb;
	  lower_bound lb (Sk ndd.nd_skeleton)
    | Sk sk ->
	let skd = !! sk in
	if not (Kind.atomic skd.sk_kind) then
	  skd.sk_lb <- Lb.union skd.sk_lb lb



  (** [upper_bound lb ns] registers the (weak) inequality [lb < ns].
   *)
  let rec upper_bound ns ub =
    match ns with
      Nd nd ->
	let ndd = !! nd in
	ndd.nd_ub <- Ub.inter ndd.nd_ub ub;
	  upper_bound (Sk ndd.nd_skeleton) ub
    | Sk sk ->
	let skd = !! sk in
	if not (Kind.atomic skd.sk_kind) then
	  skd.sk_ub <- Ub.inter skd.sk_ub ub



  (** [strong_leq nd1 nd2] sets a strong inequality [nd1 < nd2].  There
      are two  particulare cases:
      - if [nd1] and [nd2] are the same node, nothing in done (reflexive
        strong inequalities are tautologies),
      - if [nd1] and [nd2] are of kind [Atom], the inequality is in fact 
        registered as weak
   *)
  let strong_leq nd1 nd2 =

   let nd1d = !! nd1
   and nd2d = !! nd2 in

   if nd1d != nd2d then begin

     match nd_kind nd1 with

       Katom -> 
	 nd1d.nd_wsucc <- Tree.add (Nd nd2) nd1d.nd_wsucc;
	 nd2d.nd_wpred <- Tree.add (Nd nd1) nd2d.nd_wpred

    | Ktype | Krow _ ->
        same_skel nd1 nd2;
	nd1d.nd_ssucc <- Tree.add nd2 nd1d.nd_ssucc;
	nd2d.nd_spred <- Tree.add nd1 nd2d.nd_spred

   end



(***************************************************************************)
(** {2 General simplifications}

    The module [Simplify] implements generic simplification techniques which
    may be applied on every skeleton throughout the two steps of the solving
    process (implemented by the modules [Reduction] and [Terminal].
 *)
module Simplify = struct

   (** The following functions uses marked iterators on nodes and skeletons
       provided by the following modules.
    *)
   module NdTree = Tree.Marked (struct
     type t = node
     let mark = nd_mark NdMask.simplify
     let unmark = nd_unset NdMask.simplify
   end)

   module SkTree = Tree.Marked (struct
     type t = skeleton
     let mark = sk_mark SkMask.simplify
     let unmark = sk_unset SkMask.simplify
   end)

   module NSTree = Tree.Marked (struct
     type t = node_or_skeleton
     let mark = ns_mark NdMask.simplify SkMask.simplify
     let unmark = ns_unset NdMask.simplify SkMask.simplify
   end)



  (*-----------------------------------------------------------------------*)
  (** {3 Cycles} 

      The first simplification consists in collapsing strong subtyping
      cycles of nodes.  This is done by computing the strong connex 
      composant of the graph denoted by strong subtyping on nodes of
      a skeleton thanks to the Tarjan's algorithm.
   *)

  (** The module [StrongTarjan] implements Tarjan's algorithm.
   *)
  module StrongTarjan = Avl_tarjan.Make (struct

    type graph = skeleton
    type node = node_desc Proxy.t

    (* Nous n'utilisons pas d'itérateur marqué dans les deux 
       fonctions suivantes car notre implémentation de Tarjan en fait
       des utilisation ré-entrantes.  De plus, l'algorithme de Tarjan
       ayant son propre mécanisme de marques sur les noeuds, cela 
       n'apporterait aucune amélioration.
     *)
    let iter_nodes f sk =
      Tree.iter f (!! sk).sk_nodes

    let iter_successors f nd =
      Tree.iter f (!! nd).nd_ssucc

    let get nd =
      (!! nd).nd_tarjan 

    let set nd i =
      (!! nd).nd_tarjan <- i

  end)



  (** [cycles sk] collapses all strong subtyping cycles involving nodes
      of the skeleton [sk]. 
   *)
  let cycles sk =

    StrongTarjan.unify (fun nd1 nd2 ->
      assert (Dalton_debug.Stat.cycle ());
      Unification.nd_unify nd1 nd2
    ) sk



  (*-----------------------------------------------------------------------*)
  (** {3 Forks}

      The second simplification techniques allows collapsing "forks":
      - any non-positive node variable may be unified with its unique
        upper bound,
      - any non-negative node variable may be unified with its unique
        lower bound.
   *)

  (** [forks sk] reduces all forks involving nodes of the skeleton [sk]. 
      We assume that the constraints sets is expanded "up to [sk]", i.e.
      that every node in a skeleton upper than [sk] (w.r.t. the
      sub-term order) carries no inequality.
   *)
  let forks sk =

    let skd = !! sk in
      
    (* On fait un List.iter sur l'ensemble des noeuds nettoyé,
       comme cela on applique une fois par noeud originel l'algo de
       fourchettes.  Normalement c'est assez.
       Notons que ce n'aurait pas grand sens de faire un [NdList.iter]
       sachant qu'on unifie des noeuds dans la boucle !
     *)

    skd.sk_nodes <- NdTree.compact skd.sk_nodes;

    let atom = (skd.sk_kind = Katom) in

    Tree.iter (function nd ->

      let ndd = !! nd in

      if ndd.nd_descriptor = Variable then begin

	ndd.nd_spred <- NdTree.compact_except nd ndd.nd_spred;
	ndd.nd_ssucc <- NdTree.compact_except nd ndd.nd_ssucc;
	if atom then begin
	  ndd.nd_wpred <- NSTree.compact_except (Nd nd) ndd.nd_wpred;
	  ndd.nd_wsucc <- NSTree.compact_except (Nd nd) ndd.nd_wsucc;
	end;

	match 
	  ndd_test NdMask.negative ndd, Tree.stest ndd.nd_spred,
	  Tree.stest ndd.nd_wpred, Lb.is_bottom ndd.nd_lb,

	  ndd_test NdMask.positive ndd, Tree.stest ndd.nd_ssucc,
	  Tree.stest ndd.nd_wsucc, Ub.is_top ndd.nd_ub,

	  atom
	with
	  false, Tree.SSingleton nd', Tree.SEmpty, 
          true, _    , _            , _               , _   , _
	| false, Tree.SEmpty, Tree.SSingleton (Nd nd'),
	  true, _           , _                       , _       , _   , true
        | _    , _          , _                       , _   , 
	  false, Tree.SSingleton nd', Tree.SEmpty, true, _
	| _    , _                  , _          , _   , 
          false, Tree.SEmpty, Tree.SSingleton (Nd nd'), true, true ->
	    (* On vérifie que nd et nd' appartiennent au même squelette: c'est
	       toujours vrai si la contrainte nd < nd' est strong.  Si elle
	       est weak, c'est une cns pour que nd' soit aussi un atome. 
	     *)
	    if !! (nd_skeleton nd') == skd then begin
	      assert (Dalton_debug.Stat.fork ());
	      Unification.nd_unify nd nd'
	    end
	| _ -> 
	    ()
      end

    ) skd.sk_nodes



  (*-----------------------------------------------------------------------*)
  (** {3 Garbage collection} *)

  let clean_nodes sk =

    let skd = !! sk in
    skd.sk_nodes <- NdTree.compact skd.sk_nodes



  let clean_inequalities sk =

    let skd = !! sk in

    let atom = (skd.sk_kind = Katom) in

    skd.sk_wpred <- NSTree.compact skd.sk_wpred;
    skd.sk_wsucc <- NSTree.compact skd.sk_wsucc;

    Tree.iter (function nd ->

      let ndd = !! nd in
    
      ndd.nd_spred <- NdTree.compact_except nd ndd.nd_spred;
      ndd.nd_ssucc <- NdTree.compact_except nd ndd.nd_ssucc;
      if atom then begin
	ndd.nd_wpred <- NSTree.compact_except (Nd nd) ndd.nd_wpred;
	ndd.nd_wsucc <- NSTree.compact_except (Nd nd) ndd.nd_wsucc
      end
      else begin
	ndd.nd_wpred <- NSTree.compact ndd.nd_wpred;
	ndd.nd_wsucc <- NSTree.compact ndd.nd_wsucc
      end

    ) skd.sk_nodes



  let clean_skeletons cs =

    let csd = !! cs in
    csd.cs_skeletons <- SkTree.compact csd.cs_skeletons



  (** [garbage_nodes sk] remove all non-polar nodes of then skeleton [sk].
   *)
  let garbage_nodes sk =

    let skd = !! sk in
    skd.sk_nodes <- Tree.filter (function nd ->
      let polar = nd_test NdMask.bipolar nd in
      assert (polar || Dalton_debug.Stat.gc ());
      polar
    ) skd.sk_nodes



  (** [garbage_skeletons cs] remove all non-polar skeleton of the
      constraint set [cs].
   *)
  let garbage_skeletons cs = 
    let csd = !! cs in
    csd.cs_skeletons <- Tree.filter (sk_test SkMask.polar) csd.cs_skeletons

end


(***************************************************************************)
(** {2 Reduction}

    First step of solving consists in rewriting the input constraint into
    an equivalent one which is "reduced": its satisfiability may be 
    determinated by considering only terminal variables.
 *)



module Reduction = struct

  (** The following functions uses marked iterators on nodes provided by
      the following modules.
   *)

  module SkTree = Tree.Marked (struct
    type t = skeleton
    let mark = sk_mark SkMask.reduction
    let unmark = sk_unset SkMask.reduction
  end)

  module NdTree = Tree.Marked (struct
    type t = node
    let mark = nd_mark NdMask.reduction
    let unmark = nd_unset NdMask.reduction
  end)

  module NdTree' = Tree.Marked (struct
    type t = node
    let mark = nd_mark NdMask.reduction'
    let unmark = nd_unset NdMask.reduction'
  end)

  module NSTree' = Tree.Marked (struct
    type t = node_or_skeleton
    let mark = ns_mark NdMask.reduction' SkMask.reduction'
    let unmark = ns_unset NdMask.reduction' SkMask.reduction'
  end)



  (*-----------------------------------------------------------------------*)
  (** {3 Expansion and decomposition}

      A constraints sets is reduced by applying three following steps on 
      every skeleton:
      1. expansion: every varible node appearing in a non-terminal skeleton
         are expanded by introducing fresh variable sons.
      2. propagation of polarities: polarities carried by every skeleton
         are propagated to its sons according to the variances: for instance
         every contravariant son of a positive node must be negative
      3. decomposition: inequalities carried by nodes of the skeleton
         are decomposed into ones involving their sons.
   *)

  (** [expand sk] expands the descriptor of every node in the skeleton [sk]
      which is originally [Variable]. Moreover, it unifies sub-nodes
      at invariant position.
   *)
  let expand sk = 

    let skd = !! sk in

    Tree.iter (function nd ->

      let ndd = !! nd in
      let rigid = nd_rigid nd in

      assert (if ndd_mark NdMask.expand ndd then 
	Dalton_debug.Stat.non_terminal () else true);

      match ndd.nd_descriptor with

	Variable -> 
	  assert (Dalton_debug.Stat.expanded ());
	  ndd.nd_descriptor <- Descriptor.map (fun _ sk' ->
	    let nd' = nd_create_in_sk sk' Variable in
	    assert (Dalton_debug.Stat.node_expand ());
	    if rigid then Unification.nd_rigidify nd';
	    nd'
          ) skd.sk_descriptor

      |	Type _ | Row _ ->
	  ()

    ) skd.sk_nodes;

    assert (not (Tree.is_empty skd.sk_nodes));
    let desc = (!! (Tree.choose skd.sk_nodes)).nd_descriptor in

    Tree.iter (function nd ->

      Descriptor.iter2 (fun arg nd1 nd2 ->
	match arg.variance with
	  Invariant -> Unification.nd_unify nd1 nd2
	| Covariant | Contravariant -> ()
      ) desc (!! nd).nd_descriptor

    ) skd.sk_nodes



  (** [propagate_polarities sk] propagate the polarities of every node of
      the skeleton [sk] to its sons. 
   *)
  let propagate_polarities sk =

    let skd = !! sk in

    if skd_test SkMask.polar skd then
      Descriptor.iter (fun arg sk' -> sk_set SkMask.polar sk') skd.sk_descriptor;

    Tree.iter (function nd ->

      let ndd = !! nd in

      Descriptor.iter (fun arg nd' ->

	let nd'd = !! nd' in

	match arg.variance with
	  Covariant ->
	    if ndd_test NdMask.negative ndd then ndd_set NdMask.negative nd'd;
	    if ndd_test NdMask.positive ndd then ndd_set NdMask.positive nd'd
	| Contravariant ->
	    if ndd_test NdMask.negative ndd then ndd_set NdMask.positive nd'd;
	    if ndd_test NdMask.positive ndd then ndd_set NdMask.negative nd'd
	| Invariant ->
	    if ndd_test NdMask.bipolar ndd then ndd_set NdMask.bipolar nd'd

      ) ndd.nd_descriptor

    ) skd.sk_nodes



  (** Decomposition may fail if a destructor is applied on a type which
      cannot be destructed.  In this case on of the following exception
      is raised.
   *)
  exception Ldestr of skeleton
  exception Rdestr of skeleton



  (** [decompose sk] decomposes the inequalities carried by the skeleton
      [sk] into inequalities involving subterms.  This function
      introduces a temporary incoherence between fields of predecessors
      and sucessors that are no more symmetric.  In order to be able to 
      recover the symmetry, each node (resp. skeleton) decomposed a field
      [nd_decomposed] (resp. [sk_decomposed]) indicating wether it has
      already been decomposed.
   *) 
  let decompose sk =

    let skd = !! sk in

    (* Decomposition of the constraints carried by the skeleton itself. *)

    if not (Tree.is_empty skd.sk_wpred || Descriptor.rdestr skd.sk_descriptor)
    then raise (Rdestr sk);
    if not (Tree.is_empty skd.sk_wsucc || Descriptor.ldestr skd.sk_descriptor)
    then raise (Ldestr sk)

    NSTree'.iter (function lhs ->
      if ns_notdecomposed lhs then
	Descriptor.iter (fun arg sk' ->
	  if arg.ldestr then match arg.variance with
	    Covariant -> weak_leq lhs (Sk sk')
	  | Invariant -> weak_leq lhs (Nd (sk_node sk'))
	  | Contravariant -> assert false
        ) skd.sk_descriptor
    ) skd.sk_wpred;

    if not (Lb.is_bottom skd.sk_lb) then begin
      Descriptor.iter (fun arg sk' ->
	if arg.ldestr then match arg.variance with
	  Covariant -> lower_bound skd.sk_lb (Sk sk')
	| Invariant -> lower_bound skd.sk_lb (Nd (sk_node sk'))
	| Contravariant -> assert false
      ) skd.sk_descriptor;
      skd.sk_lb <- Lb.bottom
    end;

    skd.sk_wpred <- Tree.empty;
    skd_set SkMask.decomposed skd;

    NSTree'.iter (function rhs ->
      if ns_notdecomposed rhs then
	Descriptor.iter (fun arg sk' ->
	  if arg.rdestr then match arg.variance with
	    Covariant -> weak_leq (Sk sk') rhs
	  | Invariant -> weak_leq (Nd (sk_node sk')) rhs
	  | Contravariant -> assert false
        ) skd.sk_descriptor
    ) skd.sk_wsucc;

    if not (Ub.is_top skd.sk_ub) then begin
      Descriptor.iter (fun arg sk' ->
	if arg.rdestr then match arg.variance with
	  Covariant -> upper_bound (Sk sk') skd.sk_ub
	| Invariant -> upper_bound (Nd (sk_node sk')) skd.sk_ub
	| Contravariant -> assert false
      ) skd.sk_descriptor;
      skd.sk_ub <- Ub.top
    end;

    skd.sk_wsucc <- Tree.empty;

    (* Decomposition of the constraints carried by the nodes. *)

    Tree.iter (function nd ->

      let ndd = !! nd in

      NdTree'.iter (function nd' ->
	let nd'd = !! nd' in
	if nd'd != ndd then 
	  Descriptor.iter2 (fun arg nd1 nd1' ->
	    match arg.variance with
	      Covariant -> strong_leq nd1 nd1'
	    | Contravariant -> strong_leq nd1' nd1
            | Invariant -> ()
        ) ndd.nd_descriptor nd'd.nd_descriptor
      ) ndd.nd_ssucc;

      ndd.nd_spred <- Tree.empty;
      ndd.nd_ssucc <- Tree.empty;

      NSTree'.iter (function lhs ->
	if ns_notdecomposed lhs then
	  Descriptor.iter (fun arg nd' ->
	    if arg.rdestr then weak_leq lhs (Nd nd')
          ) ndd.nd_descriptor
      ) ndd.nd_wpred;

      if not (Lb.is_bottom ndd.nd_lb) then begin
	Descriptor.iter (fun arg nd' ->
	  if arg.rdestr then lower_bound ndd.nd_lb (Nd nd')
        ) ndd.nd_descriptor;
	ndd.nd_lb <- Lb.bottom
      end;

      ndd.nd_wpred <- Tree.empty;
      ndd_set NdMask.decomposed ndd;

      NSTree'.iter (function rhs ->
	if ns_notdecomposed rhs then
	  Descriptor.iter (fun arg nd' ->
	    if arg.ldestr then weak_leq (Nd nd') rhs
          ) ndd.nd_descriptor
      ) ndd.nd_wsucc;

      if not (Ub.is_top ndd.nd_ub) then begin
	Descriptor.iter (fun arg nd' ->
	  if arg.ldestr then upper_bound (Nd nd') ndd.nd_ub
        ) ndd.nd_descriptor;
	ndd.nd_ub <- Ub.top
      end;

      ndd.nd_wsucc <- Tree.empty

    ) skd.sk_nodes



  (*-----------------------------------------------------------------------*)
  (** {3 Reduction of a constraint set} 

      A constraint set is reduced by appliying the three previous steps on
      every skeletons.  Skeletons are considered in the order defined 
      by the sub-term relation.
   *)

  module SkTerm = Avl_topo.Make (struct

    type graph = cset
    type node = skeleton

    let iter_nodes f cs =
      SkTree.iter f (!! cs).cs_skeletons

    let iter_successors f sk =
      Descriptor.iter (fun _ sk' -> f sk') (!! sk).sk_descriptor

    let get sk =
      (!! sk).sk_topological

    let set sk i =
      (!! sk).sk_topological <- i
     
  end)



  (** [reduce cs] reduces the constraint set [cs].  
      The optional parameter [comparing] indicates wether forks must be reduced
      (for solving) or not (for scheme comparison).
   *)
  let reduce comparing cs =

    SkTerm.iter (function sk ->
      if not (sk_terminal sk) then begin
	Simplify.cycles sk;
	if not comparing then Simplify.forks sk;
	Simplify.clean_nodes sk;
	expand sk;
	propagate_polarities sk;
	decompose sk;
	Simplify.garbage_nodes sk
      end
    ) cs



  (*-----------------------------------------------------------------------*)
  (** {3 Hash-consing} *)

  module SkHashConsing = Hash_consing (struct

    type t = skeleton

    (* NOTABENE On suppose ici que le hash-consing a lieu après
       l'expansion et qu'il n'y a pas de squelette vide.  De plus, on
       fait un truc bizarre avec les niveaux terminaux : ils ont tous
       le même stamp [Descriptor.Variable], mais seulement ceux qui
       sont atomiques sont hachés.  Ce pourrait être mieux de faire
       une fonction spécifique dans le Hash-Consing "hachable" et de
       supprimer [No_hash] ou tout simplement de ne donner accès
       qu'aux noeuds hachabless...
     *)

    let stamp sk =

      let skd = !! sk in

      assert (not (Tree.is_empty skd.sk_nodes));
      let ndd = !! (Tree.choose skd.sk_nodes) in

      Descriptor.map2 (fun arg sk' nd' ->
	match arg.variance with
	  Covariant | Contravariant -> (!! sk').sk_stamp
	| Invariant -> (!! nd').nd_stamp
      ) skd.sk_descriptor ndd.nd_descriptor

      

    let equal sk1 sk2 =
      Descriptor.for_all2 (fun _ i1 i2 -> i1 = i2) (stamp sk1) (stamp sk2)

    exception No_hash

    let hash sk =
      match stamp sk with
	Variable -> if (!! sk).sk_kind = Katom then 0 else raise No_hash
      |	Type t -> Type.hash t
      |	Row (_, x1, x2) -> Hashtbl.hash (x1, x2)

    let unify = Unification.sk_unify

  end)

  module NdHashConsing = Hash_consing (struct

    type t = node

    let equal nd1 nd2 = 
      Descriptor.for_all2 (fun _ nd1 nd2 -> !! nd1 == !! nd2) 
	(!! nd1).nd_descriptor
	(!! nd2).nd_descriptor

    exception No_hash

    let hash nd =
      match (!! nd).nd_descriptor with
	Variable -> assert false
      |	Type t -> Type.hash (Type.map (fun _ nd -> (!! nd).nd_stamp) t)
      |	Row (lbl, nd_lbl, nd') ->
	  Hashtbl.hash ((!! nd_lbl).nd_stamp, (!! nd').nd_stamp)

    let unify nd1 nd2 =
      assert (Dalton_debug.Stat.hash_consing ());
      Unification.nd_unify nd1 nd2

  end)



  (** [hash_consing cs] performs hash-consing on the constraint set [cs].
      This function makes the following assumptions on [cs] :
      - There is no empty skeleton.
      - Non-terminal skeletons do not carry any constraint (i.e. they
        have been decomposed to leaves).
   *)
  let hash_consing cs =

    (* Hash-consing of skeletons. *)
    let skeleton_list = SkTerm.list cs in
    SkHashConsing.f (fun f -> List.iter f skeleton_list) 7;

    (* Hash-consing of nodes. *)
    let skeleton_list = SkTerm.list cs in
    List.iter (function sk ->
      if not (sk_terminal sk) then begin
	let skd = !! sk in
	NdHashConsing.f (fun f -> NdTree.iter f skd.sk_nodes) 7
      end
    ) skeleton_list;

    let csd = !! cs in
    csd.cs_skeletons <- SkTree.compact csd.cs_skeletons;
    Tree.iter (function sk ->
      let skd = !! sk in
      skd.sk_nodes <- NdTree.compact skd.sk_nodes
    ) csd.cs_skeletons

end



(***************************************************************************)
(** {2 Terminal} *) 

module Terminal = struct

  module SkTree = Tree.Marked (struct
    type t = skeleton
    let mark = sk_mark SkMask.terminal
    let unmark = sk_unset SkMask.terminal
  end)

  module NdTree = Tree.Marked (struct
    type t = node
    let mark = nd_mark NdMask.terminal
    let unmark = nd_unset NdMask.terminal
  end)

  module NSTree' = Tree.Marked (struct    
    type t = node_or_skeleton
    let mark = ns_mark NdMask.terminal' SkMask.terminal'
    let unmark = ns_unset NdMask.terminal' SkMask.terminal'
  end)




  (*-----------------------------------------------------------------------*)
  (** {3 Removing inequalities involving decomposed nodes and skeletons}

      After the reduction of non-terminal skeletons and nodes, terminal ones
      can still carry inequalities involving skeletons or nodes marked as
      decomposed.  Because this is only garbage, it must be removed.
   *)

  (** [remove_decomposed term_sk] removes all inequalities carried by
      nodes or skeletons in [term_sk] which involve a node or a
      skeleton marked as decomposed.
   *) 
  let remove_decomposed sk_term =

    Tree.iter (function sk ->

      let skd = !! sk in
      skd.sk_wpred <- NSTree'.filter ns_notdecomposed skd.sk_wpred;
      skd.sk_wsucc <- NSTree'.filter ns_notdecomposed skd.sk_wsucc;

      NdTree.iter (function nd ->
	let ndd = !! nd in
	ndd.nd_wpred <- NSTree'.filter ns_notdecomposed ndd.nd_wpred;
	ndd.nd_wsucc <- NSTree'.filter ns_notdecomposed ndd.nd_wsucc	
      ) skd.sk_nodes

    ) sk_term



  (*-----------------------------------------------------------------------*)
  (** {3 Unifying atomic skeletons} *)

  (** [unify_atomic_skeletons cs] unifies all atomic skeletons in the 
      constraint set [cs].
   *)
  let unify_atomic_skeletons cs =

    let previous = ref None in

    SkTree.iter (function sk ->
      if !! sk.sk_kind = Katom then 
        match ! previous with
          None -> previous := Some sk
        | Some sk' -> Unification.sk_unify sk' sk
    ) (!! cs).cs_skeletons



  (*-----------------------------------------------------------------------*)
  (** {3 Collapsing atomic cycles} *)

  (** The module [WeakTarjan] implements Tarjan's algorithm on the graph
      defined by weak inequalities.
   *)
  module WeakTarjan = Avl_tarjan.Make (struct

    type graph = skeleton Tree.t
    type node = node_or_skeleton

    (* Nous n'utilisons pas d'itérateur marqué dans les deux 
       fonctions suivantes car notre implémentation de Tarjan en fait
       des utilisation ré-entrantes.  De plus, l'algorithme de Tarjan
       ayant son propre mécanisme de marques sur les noeuds, cela 
       n'apporterait aucune amélioration.
     *)

    let iter_nodes f term_sk =
      Tree.iter (function sk ->
	f (Sk sk);
	Tree.iter (function nd -> f (Nd nd)) (!! sk.sk_nodes)
      ) term_sk

    let iter_successors f nd_or_sk =
      let iter_weak succ =
	Tree.iter (function 
	  | Nd nd' -> f (Nd nd')
	  | Sk sk' -> f (Sk sk')
        ) succ
      in
      match nd_or_sk with
	Nd nd -> 
	  let ndd = !! nd in
	  Tree.iter (fun nd' -> f (Nd nd')) ndd.nd_ssucc;
	  iter_weak ndd.nd_wsucc
      |	Sk sk -> 
	  iter_weak (!! sk).sk_wsucc

    let get = function
	Nd nd -> (!! nd).nd_tarjan
      |	Sk sk -> (!! sk).sk_topological

    let set nd_or_sk i =
      match nd_or_sk with
	Nd nd -> (!! nd).nd_tarjan <- i
      |	Sk sk -> (!! sk).sk_topological <- i

  end)



  (** [atomic_cycles term_sk] collapses all atomic nodes of atomic cycles
      in terminal skeletons.  [term_sk] is the list of terminal skeletons.
   *)
  let atomic_cycles term_sk =

    WeakTarjan.fold
      None
      (fun ns accu ->
	match accu, ns with
	  None, Nd nd when nd_kind nd = Katom -> 
	    Some nd
	| Some nd0, Nd nd when nd_kind nd = Katom ->
	    Unification.nd_unify nd0 nd;
	    accu
	| _ ->
	    accu
    ) () (fun _ _ -> ()) term_sk



  (*-----------------------------------------------------------------------*)
  (** {3 Transitive closure} *)

  (** [strong_normalize set] normalizes a set of nodes (representing a
      list of (strong) predecessors or successors).
      Normalizing consists in removing multiple occurences of nodes
      and sorting them according to their stamps.
   *)
  let strong_normalize set =
    Tree.sort nd_compare set



  (** [weak_normalize set] normalizes a set of nodes or skeletons
      (representing a list of (weak) predecessors or successors).
      Normalizing consists in removing multiple occurences of nodes and
      skeletons and sorting them according to their stamps.
   *)
  let weak_normalize set =
    Tree.sort ns_compare set



  exception Inequality of Lb.t * Ub.t

  let polarized_closure term_sk =

    let nodes = Stack.create ()
    and skeletons = Stack.create () in

    let lb = ref Lb.bottom
    and spred = ref Tree.empty
    and wpred = ref Tree.empty
    in

    let ub = ref Ub.top
    and ssucc = ref Tree.empty
    and wsucc = ref Tree.empty
    in

    let init () =
      spred := Tree.empty;
      wpred := Tree.empty;
      ssucc := Tree.empty;
      wsucc := Tree.empty;
      Stack.iter (nd_unset NdMask.closure) nodes;
      Stack.iter (sk_unset SkMask.closure) skeletons;
      Stack.clear nodes;
      Stack.clear skeletons
    in


    let rec loop_wpred cont = function
	Nd nd -> loop_wpred_nd cont nd
      |	Sk sk -> loop_wpred_sk sk

    and loop_wpred_nd cont nd =

      let ndd = !! nd in

      if ndd_mark NdMask.wpred ndd then begin
	Stack.push nd nodes;

	lb := Lb.union !lb ndd.nd_lb;
	if ndd_test NdMask.negative ndd then wpred := Tree.add (Nd nd) !wpred;
 	if cont then begin
	  let cont' = ndd_not NdMask.closed ndd in
	  Tree.iter (loop_wpred_nd cont') ndd.nd_spred;
	  Tree.iter (loop_wpred cont') ndd.nd_wpred
	end

      end

    and loop_wpred_sk sk =

      let skd = !! sk in

      if skd_mark SkMask.wpred skd then begin
	Stack.push sk skeletons;
	lb := Lb.union !lb skd.sk_lb;
	if skd_test SkMask.polar skd then wpred := Tree.add (Sk sk) !wpred
      end

    and loop_spred cont nd =

      let ndd = !! nd in

      if ndd_mark NdMask.spred ndd then begin
	Stack.push nd nodes;

	lb := Lb.union !lb ndd.nd_lb;
	if ndd_test NdMask.negative ndd then spred := Tree.add nd !spred;
	if cont then begin
	  let cont' = ndd_not NdMask.closed ndd in
 	  Tree.iter (loop_spred cont') ndd.nd_spred;
	  Tree.iter (loop_wpred cont') ndd.nd_wpred
	end
      end

    in

    let rec loop_wsucc cont = function
	Nd nd -> loop_wsucc_nd cont nd
      |	Sk sk -> loop_wsucc_sk sk

    and loop_wsucc_nd cont nd =

      let ndd = !! nd in

      if ndd_mark NdMask.wsucc ndd then begin
	Stack.push nd nodes;

	ub := Ub.inter !ub ndd.nd_ub;
	if ndd_test NdMask.positive ndd then wsucc := Tree.add (Nd nd) !wsucc;
	if cont then begin
	  let cont' = ndd_not NdMask.closed ndd in
 	  Tree.iter (loop_wsucc_nd cont') ndd.nd_ssucc;
	  Tree.iter (loop_wsucc cont') ndd.nd_wsucc
	end

      end

    and loop_wsucc_sk sk =

      let skd = !! sk in

      if skd_mark SkMask.wsucc skd then begin
	Stack.push sk skeletons;
	ub := Ub.inter !ub skd.sk_ub;
	if skd_test SkMask.polar skd then wsucc := Tree.add (Sk sk) !wsucc
      end

    and loop_ssucc cont nd =

      let ndd = !! nd in

      if ndd_mark NdMask.ssucc ndd then begin
	Stack.push nd nodes;

	ub := Ub.inter !ub ndd.nd_ub;
	if ndd_test NdMask.positive ndd then ssucc := Tree.add nd !ssucc;
	if cont then begin
	  let cont' = ndd_not NdMask.closed ndd in
 	  Tree.iter (loop_ssucc cont') ndd.nd_ssucc;
	  Tree.iter (loop_wsucc cont') ndd.nd_wsucc
	end
	
      end

    in

    let nd_closure nd =
      let ndd = !! nd in
      Stack.push nd nodes;
      ndd_set (NdMask.spred lor NdMask.ssucc) ndd;
      if nd_kind nd = Katom then ndd_set (NdMask.wpred lor NdMask.wsucc) ndd;

      lb := ndd.nd_lb;
      Tree.iter (loop_spred true) ndd.nd_spred;
      Tree.iter (loop_wpred true) ndd.nd_wpred;
      ndd.nd_lb <- !lb;
      ndd.nd_spred <- strong_normalize !spred;
      ndd.nd_wpred <- weak_normalize !wpred;
	  
      ub := ndd.nd_ub;
      Tree.iter (loop_ssucc true) ndd.nd_ssucc;
      Tree.iter (loop_wsucc true) ndd.nd_wsucc;
      ndd.nd_ub <- !ub;
      ndd.nd_ssucc <- strong_normalize !ssucc;
      ndd.nd_wsucc <- weak_normalize !wsucc;

      ndd_set NdMask.closed ndd;

      (* Checking constant bounds *)
      if not (Lub.leq ndd.nd_lb ndd.nd_ub) then 
	raise (Inequality (ndd.nd_lb, ndd.nd_ub));

      init ()
    in

    let sk_closure sk =
      let skd = !! sk in
      if skd_test SkMask.polar skd then begin
	lb := skd.sk_lb;
	Tree.iter (loop_wpred true) skd.sk_wpred;
	skd.sk_wpred <- !wpred;
	skd.sk_lb <- !lb;
	ub := skd.sk_ub;
	Tree.iter (loop_wsucc true) skd.sk_wsucc;
	skd.sk_wsucc <- !wsucc;
	skd.sk_ub <- !ub
      end;
      init ()
    in

    (* TEMPORARY CA NA RIEN A FOUTRE ICI *)

    Tree.iter (function sk ->
      sk_closure sk;
      let skd = !! sk in
      let nodes = NdTree.compact skd.sk_nodes in
      Tree.iter nd_closure nodes;
      skd.sk_nodes <- Tree.filter (function nd ->
	let polar = nd_test NdMask.bipolar nd in
	assert (polar || Dalton_debug.Stat.gc ());
	polar
      ) nodes
    ) term_sk;

    (* Perform the closure of every skeleton or node. *)

    Tree.iter (function sk ->
      let skd = !! sk in
      if skd_not SkMask.polar skd then begin
	skd.sk_wpred <- Tree.empty;
	skd.sk_lb <- Lb.bottom;
	skd.sk_wsucc <- Tree.empty;
	skd.sk_ub <- Ub.top
      end;
      NdTree.iter (function nd ->
	let ndd = !! nd in
	if ndd_not NdMask.positive ndd then begin
	  ndd.nd_spred <- Tree.empty;
	  ndd.nd_wpred <- Tree.empty;
	  ndd.nd_lb <- Lb.bottom
	end;
	if ndd_not NdMask.negative ndd then begin
	  ndd.nd_ssucc <- Tree.empty;
	  ndd.nd_wsucc <- Tree.empty;
	  ndd.nd_ub <- Ub.top
	end;

	ndd_unset NdMask.closed ndd
      ) skd.sk_nodes
    ) term_sk


    



  (*-----------------------------------------------------------------------*)
  (** {3 Minimization} *)

  (** [minimization sk] applies minimization on nodes of the skeleton [sk].
      This function assumes that lists of weak or strong predecessors or
      successors are normalized.
   *)
  let minimization sk =

    let skd = !! sk in

    let compare_lo nd1 nd2 =
      let nd1d = !! nd1
      and nd2d = !! nd2 in
      let c = Lb.compare nd1d.nd_lb nd2d.nd_lb in
      if c <> 0 then c else begin
	let c' = 
	  Tree.compare nd_compare nd1d.nd_spred nd2d.nd_spred 
	in
	if c' <> 0 then c' else 
  	  Tree.compare ns_compare nd1d.nd_wpred nd2d.nd_wpred
      end
    and compare_hi nd1 nd2 =
      let nd1d = !! nd1
      and nd2d = !! nd2 in
      let c = Ub.compare nd1d.nd_ub nd2d.nd_ub in
      if c <> 0 then c else begin
	let c' = 
	  Tree.compare nd_compare nd1d.nd_ssucc nd2d.nd_ssucc 
	in
	if c' <> 0 then c' else 
  	  Tree.compare ns_compare nd1d.nd_wsucc nd2d.nd_wsucc
      end
    and filter_lo nd =
      let ndd = !! nd in
      ndd_test NdMask.positive ndd && ndd_not NdMask.negative ndd
    and filter_hi nd =
      let ndd = !! nd in
      ndd_test NdMask.negative ndd && ndd_not NdMask.positive ndd
    in

    let nd_unify_list = function
	[] | [_] -> ()
      | nd :: tail ->
	  List.iter (fun nd' ->
	    assert (Dalton_debug.Stat.minimization ());
	    Unification.nd_unify nd nd'
	  ) tail
    in

    List.iter nd_unify_list
      (Tree.classes compare_lo (NdTree.filter filter_lo skd.sk_nodes));
    List.iter nd_unify_list
      (Tree.classes compare_hi (NdTree.filter filter_hi skd.sk_nodes))



  (*-----------------------------------------------------------------------*)
  (** {3 Solving of terminal skeletons} *)

  let solve comparing cs =

    let csd = !! cs in

    let term_sk = Tree.filter sk_terminal csd.cs_skeletons in

    remove_decomposed term_sk;
    Tree.iter Simplify.cycles term_sk;
    atomic_cycles term_sk;
    if not comparing then Tree.iter Simplify.forks term_sk;
    polarized_closure term_sk;

    if not comparing then begin
      Tree.iter minimization term_sk;
      Tree.iter Simplify.forks term_sk;
      Tree.iter Simplify.clean_inequalities term_sk
    end;

    Tree.iter Simplify.garbage_nodes term_sk

end



(***************************************************************************)
(** {2 Schemes} *)

module type SCHEME_ROOT = sig
    
  type t

  val cset: t -> cset
  val copy: cset -> (node -> node) -> t -> t

  val iter: (variance -> node -> unit) -> t -> unit
  val iter2: (variance -> node -> node -> unit) -> t -> t -> unit

  val fprint: formatter -> cset printer -> 
    (variance -> formatter -> node -> unit) -> t -> unit

end


module Scheme (Root: SCHEME_ROOT) = struct

  module SkList = MList (struct
    type t = skeleton
    let mark = sk_mark SkMask.terminal
    let unmark = sk_unset SkMask.terminal
  end)

  module NdTree = Tree.Marked (struct
    type t = node
    let mark = nd_mark NdMask.terminal
    let unmark = nd_unset NdMask.terminal
  end)

  module NSTree = Tree.Marked (struct
    type t = node_or_skeleton
    let mark = ns_mark NdMask.terminal SkMask.terminal
    let unmark = ns_unset NdMask.terminal SkMask.terminal
  end)



  (*-----------------------------------------------------------------------*)
  (** {3 Pretty-print} *)

  (** [fprint ppf sh] pretty prints the scheme [sh] on the formatter 
      [ppf].  
   *)
  let prepare_scheme sh =
    Root.iter Printing.nd_occurence sh;
    Printing.prepare_constraints (Root.cset sh);
    Root.iter (fun _ nd -> Printing.nd_name nd) sh


  
  let fprint_unsafe ppf sh =
    Root.fprint ppf Printing.cset
      (fun variance ppf nd -> Printing.node (Some variance) ppf nd) sh



  let fprint ppf sh =

    let cs = Root.cset sh in

    (* Calculating transient fields. *)
    Printing.reset_names ();
    prepare_scheme sh;

    (* Printinging the scheme. *)
    fprint_unsafe ppf sh;

    (* Cleaning transient fields *)
    Printing.cs_reset cs



  (*-----------------------------------------------------------------------*)
  (** {3 Solving} *)


  type solve_report =
      Ldestr of skeleton
    | Rdestr of skeleton
    | Inequality of Lb.t * Ub.t

  let report_solve ppf = function
      Ldestr sk ->
	let term ppf = Printing.skeleton ppf sk in
	Report.ldestr ppf ~term;
	Printing.cs_reset (!! sk).sk_cset
 
    | Rdestr sk -> 
	let term ppf = Printing.skeleton ppf sk in
	Report.rdestr ppf ~term;
	Printing.cs_reset (!! sk).sk_cset

    | Inequality (lb, ub) -> 
	let explanation ppf = Lub.fprint_notleq ppf lb ub in
	Report.inequality ppf ~explanation



  (** [polarize_roots sh] sets the polarity flags of the roots of the scheme
      [sh].
   *)
  let polarize_roots sh =
    Root.iter (fun variance nd ->
      let ndd = !! nd in
      sk_set SkMask.polar ndd.nd_skeleton;
      match variance with
	Covariant -> ndd_set NdMask.positive ndd;
      |	Contravariant -> ndd_set NdMask.negative ndd
      |	Invariant -> ndd_set NdMask.positive ndd; ndd_set NdMask.negative ndd
    ) sh



  let unsafe_solve comparing sh =

    let cs = Root.cset sh in
    let csd = !! cs in
    Terminal.unify_atomic_skeletons cs;
    Simplify.clean_skeletons cs;

    polarize_roots sh;
    Reduction.reduce comparing cs;
    Terminal.solve comparing cs;
    Simplify.garbage_skeletons cs;
    if not comparing then Reduction.hash_consing cs;
    reset_transient_fields cs



  (** [solve sh] solves the scheme [sh].
   *)
  let solve sh =
    try
      unsafe_solve false sh;
      None
    with
      Reduction.Ldestr sk -> Some (Ldestr sk)
    | Reduction.Rdestr sk -> Some (Rdestr sk)
    | Terminal.Inequality (lb, ub) -> Some (Inequality (lb, ub))



  (*-----------------------------------------------------------------------*)
  (** {3 Copy} *)

  exception Copy_expand

  (** [copy sh] returns a copy of the scheme [sh].
   *)
  let copy ?(subst=Copy.identity) ?expand sh =

    let cs = Root.cset sh in
    let cs' = 
      try
	Copy.make ~subst cs
      with
	exn -> Copy.clean cs; raise exn
    in
    let sh' = Root.copy cs' Copy.nd_get_copy sh in
    Copy.clean cs;

    match expand with
      None -> sh'
    | Some cond ->
	begin try
	  Root.iter (fun _ nd ->
	    Unification.sk_expand_manifest cond (!! nd).nd_skeleton
          ) sh';
	with Unification.Expand_manifest -> raise Copy_expand
	end;

	begin match solve sh' with None -> () | Some _ -> assert false end;

	sh'



  (*-----------------------------------------------------------------------*)
  (** {3 Comparison} *)

  type comparison_report =
      Incompatible of Root.t * skeleton * Root.t * skeleton
    | Missing_desc of Root.t * skeleton * skeleton
    | Missing_same_skel of Root.t * skeleton * skeleton
    | Missing_equal of Root.t * node * node
    | Missing_sleq of Root.t * node * node
    | Missing_wleq of Root.t * node_or_skeleton * node_or_skeleton
    | Missing_lb of Root.t * Lb.t * Lb.t * node_or_skeleton
    | Missing_ub of Root.t * Ub.t * Ub.t * node_or_skeleton

  exception LowerBound of Lb.t * Lb.t * node_or_skeleton
  exception UpperBound of Ub.t * Ub.t * node_or_skeleton
  exception Sleq of node * node
  exception Wleq of node_or_skeleton * node_or_skeleton

  let skr_of_skd skd =
    { skr_wpred = skd.sk_wpred;
      skr_wsucc = skd.sk_wsucc;
      skr_lb = skd.sk_lb;
      skr_ub = skd.sk_ub
    }

  let skr_store sk =
    let skd = !! sk in
    skd.sk_rigid <- Some (skr_of_skd skd) 

  let skr_restore sk =
    let skd = !! sk in
    match skd.sk_rigid with
      None -> ()
    | Some skr ->
	let new_skr = skr_of_skd skd in
	skd.sk_wpred <- skr.skr_wpred;
	skd.sk_wsucc <- skr.skr_wsucc;
	skd.sk_lb <- skr.skr_lb;
	skd.sk_ub <- skr.skr_ub;
	skd.sk_rigid <- Some new_skr

  let ndr_of_ndd ndd =
    { ndr_spred = ndd.nd_spred;
      ndr_ssucc = ndd.nd_ssucc;
      ndr_wpred = ndd.nd_wpred;
      ndr_wsucc = ndd.nd_wsucc;
      ndr_lb = ndd.nd_lb;
      ndr_ub = ndd.nd_ub
    } 

  let ndr_store nd =
    let ndd = !! nd in
    ndd.nd_rigid <- Some (ndr_of_ndd ndd)

  let ndr_restore nd =
    let ndd = !! nd in
    match ndd.nd_rigid with
      None -> ()
    | Some ndr ->
	let new_ndr = ndr_of_ndd ndd in
	ndd.nd_spred <- ndr.ndr_spred;
	ndd.nd_ssucc <- ndr.ndr_ssucc;
	ndd.nd_wpred <- ndr.ndr_wpred;
	ndd.nd_wsucc <- ndr.ndr_wsucc;
	ndd.nd_lb <- ndr.ndr_lb;
	ndd.nd_ub <- ndr.ndr_ub;
	ndd.nd_rigid <- Some new_ndr
	


  let compare sh1_orig sh2_orig =

    let sh1 = copy sh1_orig
    and sh2 = copy sh2_orig in

    try

    let cs = Root.cset sh2 in

    (* Storing rigid constraints *)
    (* fprintf err_formatter "@[<v>(original)@ %a@]@.@." fprint sh2_orig; *)
    (* fprintf err_formatter "@[<v>(copy)@ %a@]@.@." fprint sh2; *)
    Tree.iter (function sk ->
      skr_store sk;
      Tree.iter ndr_store (!! sk).sk_nodes
    ) (!! cs).cs_skeletons;
    (* fprintf err_formatter "@[<v>(1)@ %a@]@.@." fprint sh2; *)

    (* Relating constraint sets *)
    merge_cset cs (Root.cset sh1);
    Root.iter2 (fun variance nd1 nd2 -> 
      match variance with
	Covariant -> strong_leq nd1 nd2
      | Contravariant -> strong_leq nd2 nd1
      | Invariant -> equal nd1 nd2
    ) sh1 sh2;
    unsafe_solve true sh2;
    
    (* Restoring rigid constraints *)
    Tree.iter (function sk ->
      skr_restore sk;
      Tree.iter ndr_restore (!! sk).sk_nodes
    ) (!! cs).cs_skeletons;
    (* fprintf err_formatter "@[<v>(2)@ %a@]@.@." fprint sh2; *)
    unsafe_solve true sh2;
    (* fprintf err_formatter "@[<v>(3)@ %a@]@.@." fprint sh2; *)
    (* A ce point là du code, les skd/ndd contiennent les contraintes
       originelles (i.e. celles de sh2) et les skr/ndr contiennent
       toutes les contraintes (i.e. les précédentes avec celles de sh1
       en plus).  
     *)

    (* Comparing. *)
    Tree.iter (function sk ->

      let skd = !! sk in

      match skd.sk_descriptor with

	Type _ | Row _ -> ()

      |	Variable ->
	  let spred = 
	    if Kind.atomic skd.sk_kind then (function ndd ->
	      Tree.union (Tree.filter_map (function
		  Nd nd' -> Some nd' | Sk _ -> None
                ) ndd.nd_wpred) ndd.nd_spred)
	    else (function ndd -> ndd.nd_spred)
	  in

	  Tree.iter (function nd ->
	    let ndd = !! nd in
	    match ndd.nd_rigid with
	      None -> assert false
	    | Some ndr ->
		if not (Lb.leq ndr.ndr_lb ndd.nd_lb) then
		  raise (LowerBound (ndd.nd_lb, ndr.ndr_lb, Nd nd));
		if not (Ub.geq ndr.ndr_ub ndd.nd_ub) then
		  raise (UpperBound (ndd.nd_ub, ndr.ndr_ub, Nd nd));
		NdTree.iter_diff (function nd' -> raise (Sleq (nd', nd)))
		  ndr.ndr_spred (spred ndd);
(*
		NSTree.iter_diff (function ns' -> 
		  if not (Lub.geq (ns_lb ns') ndd.nd_ub) then
		    raise (Wleq (Nd nd, ns'))
                  ) ndr.ndr_wsucc ndd.nd_wsucc
*)
		NSTree.iter_diff (function ns' -> 
		  if not (Lub.geq ndd.nd_lb (ns_ub ns')) then
		    raise (Wleq (ns', Nd nd))
                  ) ndr.ndr_wpred ndd.nd_wpred

          ) skd.sk_nodes;

	  match skd.sk_rigid with
	    None -> assert false
	  | Some skr ->
	      NSTree.iter_diff (function ns' -> 
		if not (Lub.geq skd.sk_lb (ns_ub ns')) then
		  raise (Wleq (ns', Sk sk))
   	      ) skr.skr_wpred skd.sk_wpred
	  
    ) (!! cs).cs_skeletons;

    
    None

    with

      UnificationError ((_, sk1), (_, sk2), _) ->
	Some (Incompatible (sh1, sk1, sh2, sk2))

    | Unification.Expand_rigid_skeleton (sk1, sk2) ->
	Some (Missing_desc (sh2, sk1, sk2))

    | Unification.Unify_rigid_skeleton (sk1, sk2) ->
	Some (Missing_same_skel (sh2, sk1, sk2))

    | Unification.Unify_rigid_node (nd1, nd2) ->
	Some (Missing_equal (sh2, nd1, nd2))

    | Sleq (nd1, nd2) ->
	Some (Missing_sleq (sh2, nd1, nd2))

    | Wleq (lhs, rhs) ->
	Some (Missing_wleq (sh2, lhs, rhs))

    | LowerBound (expected_lb, provided_lb, ns) ->
	Some (Missing_lb (sh2, expected_lb, provided_lb, ns))

    | UpperBound (expected_lb, provided_lb, ns) ->
	Some (Missing_ub (sh2, expected_lb, provided_lb, ns))

  let equivalent sh1 sh2 =
    (compare sh1 sh2 = None) && (compare sh2 sh1 = None)



  (*-----------------------------------------------------------------------*)
  (** {3 Minimal instances} *)

  type minimal_report = Root.t * node list

  let report_minimal ppf (sh, nodes) = 

    let cs = Root.cset sh in

    (* Calculating transient fields. *)
    Printing.reset_names ();
    List.iter Printing.nd_noghost nodes;
    List.iter (Printing.nd_tag "hl") nodes;
    prepare_scheme sh;

    (* Printinging the scheme. *)
    let scheme ppf = fprint_unsafe ppf sh in
    let variables ppf =
      let rec aux ppf = function
	  [] -> assert false
	| nd :: [] -> Printing.node None ppf nd
	| nd :: tl ->
	    Format.fprintf ppf "%a, %a" (Printing.node None) nd aux tl
      in
      aux ppf nodes
    in

    Report.minimal ppf ~scheme ~variables;

    (* Cleaning transient fields *)
    Printing.cs_reset cs


  (** [nd_polarize variance nd] sets the polarities of the node [nd] according
      to the variance [variance].  It recursively applies itself on the sons
      of the node.
   *)
  let rec nd_polarize variance nd =

    let ndd = !! nd in

    if variance <> Contravariant then ndd_set NdMask.positive ndd;
    if variance <> Covariant then ndd_set NdMask.negative ndd;

    Descriptor.iter (fun arg nd' ->
      nd_polarize (Variance.combine variance arg.variance) nd'
    ) ndd.nd_descriptor



  (** [has_minimal_instance sh] tests wether the scheme [sh] has a minimal
      instance.
   *)
  let has_minimal_instance sh =

    let cs = Root.cset sh in

    Root.iter nd_polarize sh;

    let nodes = ref [] in

    Tree.iter (function sk ->
      Tree.iter (function nd ->
	let ndd = !! nd in
	match ndd.nd_descriptor with
	  Variable when 
	    not (nd_atomic nd)
	      or
	    (ndd_test NdMask.negative ndd && ndd_test NdMask.positive ndd
	      && not (Lub.geq ndd.nd_lb ndd.nd_ub)) -> nodes := nd :: !nodes
	| _ -> ()
      ) (!! sk).sk_nodes
    ) (!! cs).cs_skeletons;

    reset_transient_fields cs;

    match !nodes with
      [] -> None
    | (_ :: _) as list -> Some (sh, list)



  let fprint_structure ppf sh =

    let cs = Root.cset sh in

    Root.fprint ppf (fun ppf _ -> ()) (fun _ ppf nd ->
      Printing.skeleton ppf (!! nd).nd_skeleton
    ) sh



  let report_comparison ppf = function

      Incompatible (sh1, sk1, sh2, sk2) ->
	Printing.reset_names ();
	Printing.sk_tag "hl1" sk1;
	Printing.sk_tag "hl2" sk2;
	let scheme1 ppf = fprint_structure ppf sh1
	and scheme2 ppf = fprint_structure ppf sh2
	and explanation ppf =
	  Report.incompatible ppf
	    ~term1:(fun ppf -> Printing.skeleton ppf sk1)
	    ~term2:(fun ppf -> Printing.skeleton ppf sk2)
	in
	Report.incompatible_schemes ppf ~scheme1 ~scheme2 ~explanation;
	Printing.cs_reset (Root.cset sh1)
	
    | Missing_desc (sh, sk_var, sk') ->
	Printing.reset_names ();
	Printing.sk_tag "hl1" sk_var;
	Printing.sk_tag "hl2" sk';
	let scheme ppf = fprint_structure ppf sh
	and variable ppf = Printing.skeleton ppf sk_var
	and term ppf = Printing.skeleton ppf sk' in
	Report.missing_desc ppf ~scheme ~variable ~term;
	Printing.cs_reset (Root.cset sh)

    | Missing_same_skel (sh, sk_var1, sk_var2) ->
	Printing.reset_names ();
	prepare_scheme sh;
	let scheme ppf = fprint_unsafe ppf sh
	and constrain ppf = 
	  Print.same_skel Printing.skeleton_as_node ppf [sk_var1; sk_var2]
	in
	Report.missing_constraint ppf ~scheme ~constrain;
	Printing.cs_reset (Root.cset sh)

    | Missing_equal (sh, nd_var1, nd_var2) ->
	Printing.reset_names ();
	prepare_scheme sh;
	let scheme ppf = fprint_unsafe ppf sh
	and constrain ppf =
	  Print.equal (Printing.node None) ppf [nd_var1; nd_var2]
	in
	Report.missing_constraint ppf ~scheme ~constrain

    | Missing_wleq (sh, lhs, rhs) ->
	Printing.reset_names ();
	Printing.ns_noghost lhs; 
	Printing.ns_noghost rhs;
	prepare_scheme sh;
	let scheme ppf = fprint_unsafe ppf sh
	and constrain ppf = 
	  Print.leq Printing.left_hand_side Printing.right_hand_side ppf lhs rhs
	in
	Report.missing_constraint ppf ~scheme ~constrain;
	Printing.cs_reset (Root.cset sh)

    | Missing_sleq (sh, nd1, nd2) ->
	Printing.reset_names ();
	Printing.nd_noghost nd1;
	Printing.nd_noghost nd2;
	prepare_scheme sh;
	let scheme ppf = fprint_unsafe ppf sh
	and constrain ppf = 
	  Print.leq (Printing.node None) (Printing.node None) ppf nd1 nd2
	in
	Report.missing_constraint ppf ~scheme ~constrain;
	Printing.cs_reset (Root.cset sh)

    | Missing_lb (sh, expected_lb, provided_lb, ns) ->
	Printing.reset_names ();
	Printing.ns_noghost ns;
	prepare_scheme sh;
	let scheme ppf = fprint_unsafe ppf sh
	and constrain ppf = Printing.lower_bound ppf (provided_lb, [ns])
	and explanation =
	  if Lb.is_bottom expected_lb then None
	  else Some (fun ppf -> 
	    Print.leq Lb.fprint Lb.fprint ppf provided_lb expected_lb)
	in
	Report.missing_bound ppf ~scheme ~constrain ~explanation;
	Printing.cs_reset (Root.cset sh)

    | Missing_ub (sh, expected_ub, provided_ub, ns) ->
	Printing.reset_names ();
	Printing.ns_noghost ns;
	prepare_scheme sh;
	let scheme ppf = fprint_unsafe ppf sh
	and constrain ppf = Printing.upper_bound ppf ([ns], provided_ub)
	and explanation =
	  if Ub.is_top expected_ub then None 
	  else Some (fun ppf ->
	    Print.leq Ub.fprint Ub.fprint ppf expected_ub provided_ub)
	in
	Report.missing_bound ppf ~scheme ~constrain ~explanation;
	Printing.cs_reset (Root.cset sh)


  (*-----------------------------------------------------------------------*)
  (** {3 Drawing} *)

  let string_breaks s =
    let rec loop i =
      try
	let i' = String.index_from s i '\n' in
	i' :: loop (i' + 1)
      with Not_found -> [String.length s]
    in
    (-1) :: loop 0



  let draw window sh x y =

    Drawing.update_param window;
    let cs = Root.cset sh in

    (* Calculating transient fields. *)
    Drawing.reset_names ();
    Root.iter Drawing.nd_occurence sh;
    Root.iter (fun _ nd -> Drawing.nd_name nd) sh;

    (* Printinging the scheme. *)

    let df = Drawing.DF.create ~window ~x ~y in
    Root.fprint df.Drawing.DF.ppf (fun _ _ -> ()) (Drawing.skeleton df) sh;
    pp_print_flush df.Drawing.DF.ppf ();
    let x' = df.Drawing.DF.x in
    let x'', y'' = Drawing.constraints window x y x' cs in

    (* Cleaning transient fields *)
    Printing.cs_reset cs;
    x'', y''

end



(***************************************************************************)
(** {2 Undocumented functions} *)

  let set_expand_manifest f =
    Unification.expand_manifest := f

  let skeleton_stamp nd =
    !! (!! nd.nd_skeleton).sk_stamp

  let rec get_lower_bound nd =
    let ndd = !! nd in
    match ndd.nd_descriptor with
      Variable -> ndd.nd_lb
    | Type _ -> assert false
    | Row (_, nd_lbl, nd') ->
	Lb.union (get_lower_bound nd_lbl) (get_lower_bound nd')

  let rec get_upper_bound nd =
    let ndd = !! nd in
    match ndd.nd_descriptor with
      Variable -> ndd.nd_ub
    | Type _ -> assert false
    | Row (_, nd_lbl, nd') ->
	Ub.inter (get_upper_bound nd_lbl) (get_upper_bound nd')

end
