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

(* $Id: dalton_light.ml,v 1.1 2003/06/30 18:05:32 simonet Exp $ *)

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

      mutable nd_pred: node Tree.t;
      mutable nd_succ: node Tree.t;
      mutable nd_lb: Lb.t;
      mutable nd_ub: Ub.t;
      (** Inequalities involving the current node are recorded in these
	  fields.
       *)

      mutable nd_flags: int;

      mutable nd_copy: node option;
      (** This transient field is used by the {!Copy} module. *)

      mutable nd_tarjan: int;
      (** This transient field is used by the {!Node_tarjan} and the
	  {!Node_tarjan_atomic} modules. *)

      nd_stamp: int
      (** Every node carries an unique integer stamp. *);

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

      mutable sk_flags: int;

      mutable sk_copy: skeleton option;
      (** This transient field is used by the {!Copy} module. *)

      mutable sk_topological: int;
      (** This transient field is used by the {!SkWalk} {!WeakTarjan} 
	  modules. *)

      sk_stamp: int;
      (** Every skeleton carries an unique integer stamp. *)
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

  (** [nd_same nd1 nd2] returns [true] if and only if the two nodes
      [nd1] and [nd2] are the same. *)
  let nd_same nd1 nd2 =
    (!! nd1) == (!! nd2)

  (** [nd_compare nd1 nd2] compares the stamps of the nodes [nd1] and [nd2]
   *)
  let nd_compare nd1 nd2 =
    compare (!! nd1.nd_stamp) (!! nd2.nd_stamp)



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
	  sk_flags = 0;
	  sk_copy = None;
	  sk_topological = 0;
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

    let nd =
      Proxy.create
	{ nd_skeleton = sk;
	  nd_descriptor = desc;
	  nd_pred = Tree.empty;
	  nd_succ = Tree.empty;
	  nd_lb = Lb.bottom;
	  nd_ub = Ub.top;
	  nd_flags = 0;
	  nd_copy = None;
	  nd_tarjan = 0;
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

    The module [Copy] provides functions for copying nodes, skeletons and 
    constraint sets.  When you copy such a structure, a pointer to the copy
    is stored in the original (using the [nd_copy] or [sk_copy] or [cs_copy]
    field, so that successive appeals to the copy function will generates
    only one copy, until the cache has been reseted.

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



  (** [cset cs] returns the copy of the constraint set [cs] : if [cs] has 
      already be copied, this function simply returns the cached copy, 
      otherwise it creates a fresh empty constraint set, stores it in the
      cache and returns it.  (Note that the returned constraint set is empty,
      in general you need to copy the skeletons next.)
   *)
  let cset cs =

    let csd = !! cs in

    match csd.cs_copy with

      Some new_cs -> new_cs

    | None -> 

	let new_cs = cs_create () in
	csd.cs_copy <- Some new_cs;
	new_cs



  (** [skeleton ~subst sk] returns a copy of the skeleton [sk], using
      the same caching mechanism as for constraint sets.  The
      descriptor carried by [sk] is also copied, by recursive calls,
      using the substitution [subst] (if no substitution is provided,
      the identity is used).  Inequality constraints carried by [sk]
      are also copied, by recursive calls to [skeleton] and [node].
      However, the nodes of the skeleton [sk] are not copied.
   *) 
  let rec skeleton ?(subst=identity) sk =

    let skd = !! sk in

    match skd.sk_copy with

      Some new_sk -> new_sk

    | None ->

	let new_sk = sk_create (cset skd.sk_cset) skd.sk_kind Variable in
	let new_skd = !! new_sk in

	skd.sk_copy <- Some new_sk;

	new_skd.sk_descriptor <- subst_descriptor subst
	    (Descriptor.map (fun _ sk' -> skeleton ~subst sk')
	       skd.sk_descriptor);

	new_sk



  (** [node ~subst sk] returns a copy of the node [nd], using
      the same caching mechanism as for constraint sets and skeletons.  
      The descriptor carried by [nd] is also copied, by recursive calls,
      using the substitution [subst] (if no substitution is provided,
      the identity is used).  Inequality constraints carried by [nd]
      are also copied, by recursive calls to [skeleton] and [node].
   *) 
  and node ?(subst=identity) nd =

    let ndd = !! nd in

    match ndd.nd_copy with

      Some new_nd -> new_nd

    | None ->

	assert (Dalton_debug.Stat.node_copy ());

	let new_sk = skeleton ndd.nd_skeleton in
	let new_nd = nd_create_in_sk new_sk Variable in
	let new_ndd = !! new_nd in

	ndd.nd_copy <- Some new_nd;

	new_ndd.nd_descriptor <- subst_descriptor subst
	    (Descriptor.map (fun _ nd' -> node ~subst nd') ndd.nd_descriptor);
	new_ndd.nd_pred <- Tree.map (node ~subst) ndd.nd_pred;
	new_ndd.nd_succ <- Tree.map (node ~subst) ndd.nd_succ;
	new_ndd.nd_lb <- subst.lb ndd.nd_lb;
	new_ndd.nd_ub <- subst.ub ndd.nd_ub;

	new_nd



  (** [cs_reset cs] resets the transient field used for copy in the 
      constraints set [cs].
   *)
  let cs_reset cs = 
    (!! cs).cs_copy <- None

  (** [cs_reset_skeletons cs] resets the transient field used for copy in
      the skeletons of the constraints set [cs].
   *)
  let cs_reset_skeletons cs =
    Tree.iter (fun sk -> (!! sk).sk_copy <- None) (!! cs).cs_skeletons

  (** [cs_reset_nodes cs] resets the transient field used for copy in
      the nodes of the constraints set [cs].
   *)
  let cs_reset_nodes cs =
    Tree.iter (function sk ->
      Tree.iter (function nd -> (!! nd).nd_copy <- None) (!! sk).sk_nodes
    ) (!! cs).cs_skeletons

  (** [cs_reset_all cs] resets all transient fields of [cs] and its nodes
      and skeletons updated during copy.
   *)
  let cs_reset_all cs =
    let csd = !! cs in
    csd.cs_copy <- None;
    Tree.iter (function sk ->
      let skd = !! sk in
      skd.sk_copy <- None;
      Tree.iter (fun nd -> (!! nd).nd_copy <- None) skd.sk_nodes
    ) csd.cs_skeletons

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
	  occur_check sk2 sk1

      |	Type _, Variable ->
	  occur_check sk2 sk1

      |	Row (_, sk1', sk1''), Variable ->
	  occur_check sk2 sk1

      |	Variable, Type _ ->
	  occur_check sk1 sk2;
	  sk1d.sk_descriptor <- sk2d.sk_descriptor

      |	Variable, Row (_, sk2', sk2'') ->
	  occur_check sk1 sk2;
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
    let cs0 = (!! ((!! nd_to0).nd_skeleton)).sk_cset in

    let sk_from = Copy.skeleton (!! nd_from0).nd_skeleton
    and sk_to = Copy.skeleton (!! nd_to0).nd_skeleton in
    merge_cset cs (!! sk_from).sk_cset;
    
    sk_unify sk sk_from; (* TEMPORARY Probablement inutile ! *)

    skd.sk_nodes <- NdTree.compact skd.sk_nodes;

    Tree.iter (function nd ->

      let nd_from = Copy.node nd_from0
      and nd_to = Copy.node nd_to0 in
      Copy.cs_reset_nodes cs0;

      nd_unify nd nd_from;
      (!! nd).nd_descriptor <- (!! nd_to).nd_descriptor

    ) skd.sk_nodes;
    Copy.cs_reset cs0;
    Copy.cs_reset_skeletons cs0;

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
	  ()

      |	Type _, Variable ->
	  ()

      |	Row (_, nd1', nd1''), Variable ->
	  ()

      | Variable, Type _ -> 
	  nd1d.nd_descriptor <- nd2d.nd_descriptor

      |	Variable, Row (_, nd2', nd2'') ->
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

      (* Merging inequalities. *)

      nd1d.nd_pred <- Tree.union nd1d.nd_pred nd2d.nd_pred;
      nd1d.nd_succ <- Tree.union nd1d.nd_succ nd2d.nd_succ;
      nd1d.nd_lb <- Lb.union nd1d.nd_lb nd2d.nd_lb;
      nd1d.nd_ub <- Ub.inter nd1d.nd_ub nd2d.nd_ub;

      (* nd2 is now a link pointing on nd1. *)
      
      Proxy.linksto nd2 nd1

    end


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

    ()

  

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



  (** [lower_bound lb ns] registers the (weak) inequality [lb < ns].
   *)
  let lower_bound lb nd =
    let ndd = !! nd in
    ndd.nd_lb <- Lb.union ndd.nd_lb lb



  (** [upper_bound lb ns] registers the (weak) inequality [lb < ns].
   *)
  let upper_bound nd ub =
    let ndd = !! nd in
    ndd.nd_ub <- Ub.inter ndd.nd_ub ub



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

    same_skel nd1 nd2;
    nd1d.nd_succ <- Tree.add nd2 nd1d.nd_succ;
    nd2d.nd_pred <- Tree.add nd1 nd2d.nd_pred



  let leq = strong_leq


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
      Tree.iter f (!! nd).nd_succ

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

	ndd.nd_pred <- NdTree.compact_except nd ndd.nd_pred;
	ndd.nd_succ <- NdTree.compact_except nd ndd.nd_succ;

	match 
	  ndd_test NdMask.negative ndd, Tree.stest ndd.nd_pred,
	  Lb.is_bottom ndd.nd_lb,

	  ndd_test NdMask.positive ndd, Tree.stest ndd.nd_succ,
	  Ub.is_top ndd.nd_ub,

	  atom
	with
	  false, Tree.SSingleton nd',
          true, _    , _            , _   , _
        | _    , _          , _   , 
	  false, Tree.SSingleton nd', true, _ ->
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

    Tree.iter (function nd ->

      let ndd = !! nd in
    
      ndd.nd_pred <- NdTree.compact_except nd ndd.nd_pred;
      ndd.nd_succ <- NdTree.compact_except nd ndd.nd_succ;

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

      match ndd.nd_descriptor with

	Variable -> 
	  ndd.nd_descriptor <- Descriptor.map (fun _ sk' ->
	    let nd' = nd_create_in_sk sk' Variable in
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
      ) ndd.nd_succ;

      ndd.nd_pred <- Tree.empty;
      ndd.nd_succ <- Tree.empty;

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
    ) skeleton_list

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




  (*-----------------------------------------------------------------------*)
  (** {3 Removing inequalities involving decomposed nodes and skeletons}

      After the reduction of non-terminal skeletons and nodes, terminal ones
      can still carry inequalities involving skeletons or nodes marked as
      decomposed.  Because this is only garbage, it must be removed.
   *)



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
  (** {3 Transitive closure} *)

  (** [strong_normalize set] normalizes a set of nodes (representing a
      list of (strong) predecessors or successors).
      Normalizing consists in removing multiple occurences of nodes
      and sorting them according to their stamps.
   *)
  let strong_normalize set =
    Tree.sort nd_compare set



  exception Inequality of Lb.t * Ub.t

  let polarized_closure term_sk =

    let nodes = Stack.create () in

    let lb = ref Lb.bottom
    and pred = ref Tree.empty
    in

    let ub = ref Ub.top
    and succ = ref Tree.empty
    in

    let init () =
      pred := Tree.empty;
      succ := Tree.empty;
      Stack.iter (nd_unset NdMask.closure) nodes;
      Stack.clear nodes
    in


    let rec loop_pred cont nd =

      let ndd = !! nd in

      if ndd_mark NdMask.wpred ndd then begin
	Stack.push nd nodes;

	lb := Lb.union !lb ndd.nd_lb;
	if ndd_test NdMask.negative ndd then pred := Tree.add nd !pred;
 	if cont then begin
	  let cont' = ndd_not NdMask.closed ndd in
	  Tree.iter (loop_pred cont') ndd.nd_pred
	end

      end

    in

    let rec loop_succ cont nd =

      let ndd = !! nd in

      if ndd_mark NdMask.wsucc ndd then begin
	Stack.push nd nodes;

	ub := Ub.inter !ub ndd.nd_ub;
	if ndd_test NdMask.positive ndd then succ := Tree.add nd !succ;
	if cont then begin
	  let cont' = ndd_not NdMask.closed ndd in
	  Tree.iter (loop_succ cont') ndd.nd_succ
	end

      end

    in

    let nd_closure nd =
      let ndd = !! nd in
      Stack.push nd nodes;
      ndd_set (NdMask.wpred lor NdMask.wsucc) ndd;

      lb := ndd.nd_lb;
      Tree.iter (loop_pred true) ndd.nd_pred;
      ndd.nd_lb <- !lb;
      ndd.nd_pred <- strong_normalize !pred;
	  
      ub := ndd.nd_ub;
      Tree.iter (loop_succ true) ndd.nd_succ;
      ndd.nd_ub <- !ub;
      ndd.nd_succ <- strong_normalize !succ;

      ndd_set NdMask.closed ndd;

      (* Checking constant bounds *)
      if not (Lub.leq ndd.nd_lb ndd.nd_ub) then 
	raise (Inequality (ndd.nd_lb, ndd.nd_ub));

      init ()
    in

    (* TEMPORARY CA NA RIEN A FOUTRE ICI *)

    Tree.iter (function sk ->
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
      NdTree.iter (function nd ->
	let ndd = !! nd in
	if ndd_not NdMask.positive ndd then begin
	  ndd.nd_pred <- Tree.empty;
	  ndd.nd_lb <- Lb.bottom
	end;
	if ndd_not NdMask.negative ndd then begin
	  ndd.nd_succ <- Tree.empty;
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
      if c <> 0 then c else
	Tree.compare nd_compare nd1d.nd_pred nd2d.nd_pred 
    and compare_hi nd1 nd2 =
      let nd1d = !! nd1
      and nd2d = !! nd2 in
      let c = Ub.compare nd1d.nd_ub nd2d.nd_ub in
      if c <> 0 then c else 
      Tree.compare nd_compare nd1d.nd_succ nd2d.nd_succ 
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

    Tree.iter Simplify.cycles term_sk;
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



  (*-----------------------------------------------------------------------*)
  (** {3 Copy} *)

  (** [copy sh] returns a copy of the scheme [sh].
   *)
  let copy ?(subst=Copy.identity) sh =

    let cs = Root.cset sh in
    let new_cs = Copy.cset cs in

    Tree.iter (function sk ->
      ignore (Copy.skeleton ~subst sk);
      Tree.iter (fun nd -> ignore (Copy.node ~subst nd)) (!! sk).sk_nodes
    ) (!! cs).cs_skeletons;

    let new_sh = Root.copy new_cs (Copy.node ~subst) sh in
    Copy.cs_reset_all cs;
    new_sh



  (*-----------------------------------------------------------------------*)
  (** {3 Pretty-print} *)

  (** [fprint ppf sh] pretty prints the scheme [sh] on the formatter 
      [ppf].  
   *)

  let fprint ppf sh =
    ()



  (*-----------------------------------------------------------------------*)
  (** {3 Solving} *)


  type solve_report =
      Ldestr of skeleton
    | Rdestr of skeleton
    | Inequality of Lb.t * Ub.t

  let report_solve ppf _ = ()

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
  (** {3 Minimal instances} *)


  type minimal_report = Root.t * node list

  let report_minimal ppf (sh, nodes) = 

    ()



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
